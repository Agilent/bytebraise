/*!
Experimental algorithmic implementation of BitBake data_smart using a priority heap to encode
operations.

Implemented:
    - setVar/getVar/delVar
    - expandKeys
    - overrides
    - operators

Major todos:
    - Caching
    - Parsing mode
    - Non-string data - this prints 3:
        python() {
            d.setVar("TEST2", 1)
            d.setVar("TEST2:append", 2)
            bb.fatal("TEST2 = " + str(d.getVar("TEST2")))
        }
    - Varflags
    - Variable history
    - append/prepend/remove combined with +=, .=, etc.
*/

use crate::errors::{DataSmartError, DataSmartResult};
use crate::keys_iter::KeysIter;
use crate::macros::get_var;
use crate::nodes::{GraphItem, ScoredOperation, StatementKind, StmtNode};
use crate::variable_operation::{NormalOperator, Operator, OverrideOperator, VariableOperation};
use anyhow::bail;
use bytebraise_util::fifo_heap::FifoHeap;
use bytebraise_util::retain_with_index::RetainWithIndex;
use bytebraise_util::split::{replace_all, split_filter_empty, split_keep};
use fxhash::FxHashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use once_cell::sync::Lazy;
use petgraph::Direction;
use petgraph::data::DataMap;
use petgraph::dot::{Config, Dot};
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::{DefaultIx, EdgeIndex};
use regex::{Captures, Regex};
use scopeguard::{ScopeGuard, defer, guard};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::Write;
use std::ops::Deref;

// TODO: check for latest version in upstream bitbake
static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());

static PYTHON_EXPANSION_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\{@.+?}").unwrap());

static KEYWORD_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?:^|:)(?P<keyword>append|prepend|remove)(?:$|:)").unwrap());

static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\s").unwrap());

#[derive(Debug)]
struct ExpansionState {
    visited: HashSet<String>,
}

impl ExpansionState {
    pub fn new() -> Self {
        ExpansionState {
            visited: HashSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct DataSmart {
    ds: StableGraph<GraphItem, u8>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    unexpanded_operations: HashSet<EdgeIndex<DefaultIx>>,
    expand_state: RefCell<Option<ExpansionState>>,
    active_overrides: RefCell<Option<IndexSet<String>>>,
    inside_compute_overrides: RefCell<()>,
}

pub(crate) type OverrideScore = (Vec<usize>, usize, usize);

// For OVERRIDES = "a:b:c",
//
// ab => ([0, 1, 1], 2, 1)
// ba => ([0, 1, 1], 1, 2)
// aba => ([0, 1, 2], 2, 1)
// bab => ([0, 2, 1], 2, 2)
// aabb => ([0, 2, 2], 3, 1)
// abab => ([0, 2, 2], 3, 1)
// baba => ([0, 2, 2], 2, 2)
//#[tracing::instrument(ret)]
pub(crate) fn score_override(
    active_overrides: &Cow<IndexSet<String>>,
    candidate_overrides: &Vec<String>,
) -> Option<OverrideScore> {
    let c: IndexSet<String> = candidate_overrides.iter().cloned().collect();
    if !c.is_subset(active_overrides) {
        return None;
    }

    let mut ret = (vec![], 0, 0);
    if candidate_overrides.is_empty() {
        return Some(ret);
    }

    let counts = candidate_overrides.iter().counts();
    // Overrides (in `active_overrides`) are listed in priority order, from lowest to highest.
    // Count the # of times each override appears in the given list, then reverse the list so that
    // higher-priority counts come first.
    ret.0 = active_overrides
        .iter()
        .map(|o| counts.get(o).copied().unwrap_or_default())
        .rev()
        .collect();

    let mut candidate = candidate_overrides.clone();

    let mut keep_going = true;
    'outer: while keep_going {
        keep_going = false;

        // Keep track of the # of times it takes to go through the loop. This is the first
        // tiebreaker for ordering.
        ret.1 += 1;

        for (override_index, active_override) in active_overrides.iter().enumerate() {
            // eprintln!(
            //     "\tconsider override {active_override}, left: {}",
            //     candidate.join("")
            // );

            // Has to be len() > 1 because we are emulating checking for :<override>.
            if candidate.len() > 1 && candidate.ends_with(std::slice::from_ref(active_override)) {
                // This emulates:  active[a.replace(":" + o, "")] = t
                // Note the original BitBake code unintentionally(?) removes all existences of the
                // override, not just the one in tail position.
                candidate.retain_with_index(|c, i| i == 0 || c != active_override);
                keep_going = true;
            } else if candidate.len() == 1 && &candidate[0] == active_override {
                assert_eq!(ret.2, 0);
                // Final (least-significant) tiebreaker is index of the override on which we stopped
                ret.2 = override_index + 1;
                break 'outer;
            }
        }
    }

    Some(ret)
}

fn split_overrides<S: AsRef<str>>(input: S) -> Vec<String> {
    split_filter_empty(input.as_ref(), ":")
        .map(String::from)
        .collect()
}

impl Default for DataSmart {
    fn default() -> Self {
        Self::new()
    }
}

static OVERRIDE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"^\w+$").unwrap());

impl DataSmart {
    pub fn new() -> DataSmart {
        DataSmart {
            ds: StableGraph::new(),
            vars: FxHashMap::default(),
            unexpanded_operations: HashSet::new(),
            expand_state: RefCell::new(None),
            active_overrides: RefCell::new(None),
            inside_compute_overrides: RefCell::new(()),
        }
    }

    pub fn dump(&self) {
        let mut f = File::create("/tmp/example1.dot").unwrap();
        let output = format!("{}", Dot::with_config(&self.ds, &[Config::EdgeNoLabel]));
        f.write_all(&output.as_bytes()).unwrap();
    }

    fn apply_removes(&self, input: &str, removes: &HashSet<String>) -> String {
        // TODO only only content flag and if not parsing
        let mut expanded_removes = HashMap::new();
        for r in removes.iter() {
            expanded_removes.insert(
                r.clone(),
                self.expand(r)
                    .unwrap()
                    .split_whitespace()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>(),
            );
        }

        let mut val = String::new();
        for v in split_keep(&WHITESPACE_REGEX, input) {
            let mut skip = false;
            for r in removes.iter() {
                if expanded_removes.get(r).unwrap().contains(&v.to_string()) {
                    //parser.removes.as_mut().unwrap().insert(r.clone());
                    skip = true;
                }
            }
            if skip {
                continue;
            }
            val += v;
        }
        val
    }

    pub fn plus_equals_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, NormalOperator::PlusEqual);
    }

    pub fn equals_plus_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, NormalOperator::EqualPlus);
    }

    pub fn equals_dot_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, NormalOperator::EqualDot);
    }

    pub fn dot_equals_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, NormalOperator::DotEqual);
    }

    pub fn weak_default_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, NormalOperator::WeakDefault);
    }

    pub fn default_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, NormalOperator::Default);
    }

    fn set_var_ex<T: Into<String>, S: Into<String>>(
        &mut self,
        var: T,
        value: S,
        normal_operator: NormalOperator,
    ) -> Option<NodeIndex<DefaultIx>> {
        let var = var.into();

        //dbg!(&var);
        let parsed = parse_statement(&var, normal_operator, value.into())?;
        let base = parsed.var_base;
        let stmt_node = parsed.stmt;
        let operator_kind = stmt_node.operator;

        // TODO: if parsing, and no keyword given, then wipe away removes, prepends, and appends
        //  Also need to do something with overrides - not sure what though (set setVar())

        let stmt_idx = self.ds.add_node(GraphItem::StmtNode(stmt_node));

        // Lookup variable base (stem) and create if it doesn't exist
        let var_entry = self
            .vars
            .entry(base.to_string())
            .or_insert_with(|| self.ds.add_node(GraphItem::new_variable(base)));

        let var_data = self.ds.node_weight_mut(*var_entry).unwrap().variable_mut();

        var_data.operations.push(VariableOperation {
            op_type: operator_kind,
            idx: stmt_idx,
        });

        let e = self.ds.add_edge(*var_entry, stmt_idx, 0);

        // If any part of the key contains '${' (not just the base), then track it as unexpanded.
        // Use [`DataSmart::expand_vars`] to expand it later.
        if var.contains("${") {
            self.unexpanded_operations.insert(e);
        }

        Some(*var_entry)
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn set_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
    ) -> Option<NodeIndex<DefaultIx>> {
        let var = var.into();
        let value = value.into();

        #[cfg(test)]
        if var == "OVERRIDES" && self.vars.contains_key("OVERRIDES") {
            unimplemented!(
                "OVERRIDES are already set! Re-computing OVERRIDES not implemented yet."
            );
        }

        self.set_var_ex(var, value, NormalOperator::Assign)
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn expand<S: AsRef<str> + Debug>(&self, value: S) -> DataSmartResult<String> {
        let value = value.as_ref();

        // |expand_state| is used to track which variables are accessed during an expansion, across
        // recursive calls to this method. The first (i.e. non-recursive) call to this method is
        // responsible for setting up and tearing down the state.

        // Create a scope guard that will clear out the expansion state upon scope exit
        let scope = guard((), |()| {
            RefCell::borrow_mut(&self.expand_state).take();
        });

        // Check if we are in the middle of recursion by seeing if the expand state exists yet
        {
            let mut s = RefCell::borrow_mut(&self.expand_state);
            if s.is_none() {
                // The expansion state doesn't exist yet, so create it
                *s = Some(ExpansionState::new());
            } else {
                // Expansion state exists; defuse scope guard - we are not responsible for the state
                ScopeGuard::into_inner(scope);
            }
        }

        let mut value = value.to_string();
        while value.contains("${") {
            //println!("{}EXPAND: {}", " ".repeat(level), value);
            let new_value = replace_all(
                &VAR_EXPANSION_REGEX,
                value.as_str(),
                |caps: &Captures| -> DataSmartResult<String> {
                    let match_str = caps.get(0).unwrap().as_str();
                    let referenced_var = &match_str[2..match_str.len() - 1];

                    //println!("{} expand: {}", " ".repeat(level), referenced_var);
                    {
                        let mut s = RefCell::borrow_mut(&self.expand_state);
                        let set = s.as_mut().unwrap();
                        if set.visited.contains(referenced_var) {
                            return Err(DataSmartError::RecursiveReferenceError {
                                var: referenced_var.to_string(),
                            }
                            .into());
                        } else {
                            set.visited.insert(referenced_var.to_string());
                        }
                    }

                    defer! {
                        let mut s = RefCell::borrow_mut(&self.expand_state);
                        let set = s.as_mut().unwrap();
                        set.visited.remove(referenced_var);
                    }

                    Ok(self
                        .get_var(referenced_var, false, true)
                        .unwrap_or(match_str.to_string()))
                },
            )?;

            if value == new_value {
                break;
            }
            value = new_value.to_string();
        }

        Ok(value)
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn del_var<S: AsRef<str> + Debug>(&mut self, var: S) -> DataSmartResult<()> {
        let var = var.as_ref();
        let (base_var, overrides) = var
            .split_once(':')
            .map_or((var, None), |p| (p.0, Some(p.1)));

        let Some(var_index) = self.vars.get(base_var).copied() else {
            return Ok(());
        };

        let mut deleted_all_stmts = false;
        if let Some(o) = overrides {
            // Find statements with this exact override
            let mut stmts = vec![];
            let mut walker = self
                .ds
                .neighbors_directed(var_index, Direction::Outgoing)
                .detach();
            while let Some(stmt_node_index) = walker.next_node(&self.ds) {
                let stmt = self.ds.node_weight(stmt_node_index).unwrap().statement();
                if let Some(o2) = stmt.override_str.as_ref()
                    && o == o2
                {
                    stmts.push(stmt_node_index);
                    self.ds.remove_node(stmt_node_index);
                }
            }

            let var_node = self.ds.node_weight_mut(var_index).unwrap().variable_mut();
            var_node.operations.retain(|op| {
                if stmts.contains(&op.idx) {
                    eprintln!("delete {:?}", &op.idx);
                }
                !stmts.contains(&op.idx)
            });

            if var_node.operations.is_empty() {
                deleted_all_stmts = true;
            }
        }

        if deleted_all_stmts || overrides.is_none() {
            // delete entire variable
            self.vars.remove(base_var);

            // No need to delete all statements if we already did it above
            if !deleted_all_stmts {
                // TODO: use VariableOperations instead of walking graph manually?
                let mut walker = self
                    .ds
                    .neighbors_directed(var_index, Direction::Outgoing)
                    .detach();

                while let Some(stmt_node_index) = walker.next_node(&self.ds) {
                    // Only delete the statement if another variable isn't using it

                    // TODO: this should only happen in specific cases, e.g. when delVar is called
                    //  as part of renameVar. Should come up with a special version of delVar just
                    //  for renameVar.
                    let c = self.ds.neighbors_undirected(stmt_node_index).count();
                    if c <= 1 {
                        self.ds.remove_node(stmt_node_index);
                    }
                }
            }

            self.ds.remove_node(var_index);
        }

        Ok(())
    }

    /// expand_keys() has to handle several different kinds of situations. They all play out a bit
    /// differently.
    ///
    /// TEST = "base"
    /// TES${Q}:append = "a"
    /// TES${Q}:append = "b"
    /// Q = "T"
    /// P = "T:append"
    ///
    /// todolist: {'TES${Q}': 'TEST', 'TES${P}': 'TEST:append'}
    /// renameVar(TES${P}, TEST:append)
    /// Variable key TES${Q} (ab) replaces original key TEST (baseQ).
    /// renameVar(TES${Q}, TEST)

    /// TESQ:A = "1"
    /// TESQ:${A} = "a"
    /// A = "A"
    ///
    /// todolist: {'TESQ:${A}': 'TESQ:A'}
    /// Variable key TESQ:${A} (a) replaces original key TESQ:A (1).
    /// renameVar(TESQ:${A}, TESQ:A)

    #[tracing::instrument(skip(self), ret)]
    pub fn rename_var<A: AsRef<str> + Debug, B: AsRef<str> + Debug>(
        &mut self,
        old: A,
        new: B,
    ) -> DataSmartResult<()> {
        let old = old.as_ref();
        let new = new.as_ref();

        if old == new {
            bail!("Calling renameVar with equivalent keys {old} is invalid");
        }

        let old_base_var = old.split_once(':').map_or(old, |p| p.0);
        let new_base_var = new.split_once(':').map_or(new, |p| p.0);
        tracing::info!("{:?} {:?}", old, new);

        // Get unexpanded value of the full old var, and assign it to new var
        if let Some(old_val) = get_var!(&self, old, parsing = true, expand = false) {
            // TODO: parsing mode?
            self.set_var(new, old_val);
        }

        // Next, transplant :appends, :prepends, and :removes
        let new_var_index = *self
            .vars
            .entry(new_base_var.to_string())
            .or_insert_with(|| self.ds.add_node(GraphItem::new_variable(new_base_var)));

        let old_var_index = *self.vars.get(old_base_var).unwrap();

        let old_var_node = self.ds.node_weight(old_var_index).unwrap().variable();

        // Worked example:
        //
        // TEST = "b"
        // TEST:${A}:append = "2"
        // A = "a"
        // OVERRIDES = "a"
        //
        // d.expand_keys();
        //   => rename_var("TEST:${A}", "TEST:a");
        //   =>

        for op in old_var_node.operations.clone().into_iter() {
            let op_data = self.ds.node_weight_mut(op.idx).unwrap();
            if op.op_type.is_override_operator() {
                op_data.statement_mut().override_str = Some("a".to_string());
                match &mut op_data.statement_mut().kind {
                    StatementKind::Operation { scope, .. } => {
                        scope.clear();
                        //scope.push("a".to_string());
                    }
                    StatementKind::PureOverride { .. } => {}
                    StatementKind::Unconditional => {}
                }

                self.ds.add_edge(new_var_index, op.idx, 1);
                self.ds
                    .node_weight_mut(new_var_index)
                    .unwrap()
                    .variable_mut()
                    .operations
                    .push(op);
                //(op);
            } else {
                dbg!(&op_data);
            }
        }

        //dbg!(&self.ds);

        self.del_var(old)?;

        Ok(())
    }

    /// There are three kinds of unexpanded keys we need to handle.
    ///
    /// Case 1: Unexpanded override(s)
    /// Example:
    ///     VAR:${B} = "value"
    ///     B = "whatever"
    ///
    /// Internally, there is a variable "VAR" with an operation ':${B} = "value"'
    /// All we need to do is expand the ${B} part and adjust the variable operation for
    /// VAR to reflect the new override string.
    ///
    /// Case 2: Unexpanded variable name
    /// Example:
    ///     VA${V} = "value"
    ///     V = "R"
    ///
    /// Internally, there is a variable "VA${V}" with an operation '= "value"'.
    ///
    /// In BitBake, expandKeys() here triggers a call to renameVar("VA${V}", "VAR"). BitBake's
    /// renameVar() does the following:
    ///     1. Gets the unexpanded value of VA${V} (in parsing mode, so override-style appends,
    ///         prepends, and removes are not applied)
    ///     2. Calls setVar("VAR", <value>). setVar is also called in parsing mode, which blows
    ///         away all appends, prepends, and removes on VAR.
    ///     3. Transfers the appends, prepends, and removes from VA${V} to VAR. Not sure why it does this,
    ///         since it essentially undoes the blowing away of those varflags from (2).
    ///     4. Calls delVar("VA${V}")
    ///
    /// We are in a position to do all of this much more simply:
    ///     1. Create a "VAR" node.
    ///     2. Transfer operations from VA${V} node onto the VAR node.
    ///     3. Delete VA${V} node.
    ///
    ///
    /// Another example:
    ///     VAR = "value"
    ///     VA${V}:append = " appended"
    ///     V = "R"
    ///
    /// Internally, we have three variables each with a single operation.
    ///
    /// We need to do this:
    ///     1. Transfer operations from VA${V} node onto the VAR node.
    ///     2. Delete VA${V} node.
    ///
    #[tracing::instrument(skip_all)]
    pub fn expand_keys(&mut self) -> DataSmartResult<Vec<String>> {
        let mut unexpanded_operations = std::mem::take(&mut self.unexpanded_operations);
        //dbg!(&unexpanded_operations);

        let mut operations = BTreeMap::new();

        // TODO: if not all 'override' operations, then need to actually do renameKey?
        // Reconstruct the unexpanded keys
        for edge in unexpanded_operations.drain() {
            let node = self.ds.edge_endpoints(edge).unwrap();
            let nodes = self.ds.index_twice_mut(node.0, node.1);

            let mut var_parts = vec![nodes.0.variable().name.clone()];

            // Take all the override str, including any operation (e.g. append)
            if let Some(b) = nodes.1.statement().override_str.clone() {
                debug_assert!(!b.starts_with(":"));
                var_parts.extend(split_overrides(&b));
            }

            dbg!(&nodes.1.statement());
            eprintln!("{:?}", &var_parts);

            let new_var = var_parts.join(":");
            let expanded = self.expand(&new_var)?;
            operations.insert(new_var, expanded);
        }

        tracing::info!("operations: {:?}", &operations);

        let ret = operations.keys().cloned().sorted().collect_vec();
        for o in operations.into_iter() {
            self.rename_var(o.0, o.1)?;
        }

        // Sanity check: did we actually expand everything?
        // TODO: will only be expanded if the referenced variables actually exist
        for stmt in self.ds.node_weights() {
            if let GraphItem::StmtNode(stmt) = stmt
                && let Some(o) = stmt.override_str.as_ref()
            {
                assert!(!o.contains("${"));
            }
        }

        Ok(ret)
    }

    fn compute_overrides(&self) -> DataSmartResult<()> {
        if let Ok(_guard) = RefCell::try_borrow_mut(&self.inside_compute_overrides) {
            if RefCell::borrow(&self.active_overrides).is_some() {
                return Ok(());
            }

            for _ in 0..5 {
                //eprintln!("{}+ override iteration {}", " ".repeat(level), i);
                let s = split_filter_empty(
                    &self.get_var("OVERRIDES", false, true).unwrap_or_default(),
                    ":",
                )
                .map(String::from)
                .collect::<IndexSet<String>>();

                //eprintln!("{} set overides = {:?}", " ".repeat(level), s);
                *RefCell::borrow_mut(&self.active_overrides) = Some(s);

                let s2 = split_filter_empty(
                    &self.get_var("OVERRIDES", false, true).unwrap_or_default(),
                    ":",
                )
                .map(String::from)
                .collect::<IndexSet<String>>();

                if *RefCell::borrow(&self.active_overrides) == Some(s2.clone()) {
                    return Ok(());
                }

                *RefCell::borrow_mut(&self.active_overrides) = Some(s2);
            }
        }

        Ok(())
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn get_var<S: AsRef<str> + Debug>(
        &self,
        var: S,
        parsing: bool,
        expand: bool,
    ) -> Option<String> {
        let var = var.as_ref();
        //dbg!(var);
        // `var_base` is the root/stem part of the variable without any overrides
        let (var_base, var_suffix) = var
            .split_once(":")
            .map_or_else(|| (var, None), |parts| (parts.0, Some(parts.1)));

        // TODO: terminology: direct-variant lookup
        let var_suffix = split_overrides(var_suffix.unwrap_or_default());
        // If an override-style operator is present, then it will never match so return None
        // TODO: what if someone adds one to OVERRIDES?
        if var_suffix
            .iter()
            .any(|o| matches!(o.as_str(), "append" | "prepend" | "remove"))
        {
            return None;
        }

        // Lookup the variable, otherwise return None
        let var_entry = self.vars.get(var_base)?;

        let w = self.ds.node_weight(*var_entry).unwrap();
        let var_data = w.variable();

        // // TODO: return directly from `override_state` for OVERRIDES?
        // if var != "OVERRIDES" {
        //     let cached = RefCell::borrow_mut(&var_data.cached_value);
        //     if cached.is_some() {
        //         return cached.clone();
        //     }
        // }

        // TODO: only do this if needed, i.e. if any operations with overrides are present
        // TODO: this method doesn't handle re-computing overrides!
        self.compute_overrides().unwrap();

        let override_state = RefCell::borrow(&self.active_overrides);
        let override_state = match override_state.as_ref() {
            Some(state) => Cow::Borrowed(state),
            None => Cow::Owned(IndexSet::new()),
        };

        // The union of active overrides with whatever overrides were provided in the
        // direct-variant lookup. This is only used for override-scoped operators.
        let override_selection_context: Cow<IndexSet<String>> = match var_suffix.is_empty() {
            false => {
                // TODO: revisit: are we sure the new overrides should be inserted into the beginning?
                let mut new_overrides = IndexSet::from_iter(var_suffix.clone());
                for old_override in override_state.deref() {
                    new_overrides.insert(old_override.clone());
                }

                Cow::Owned(new_overrides)
            }
            true => override_state.clone(),
        };

        //dbg!(&var);
        //dbg!(&self);

        // Calculate override scores for operations
        let mut resolved_variable_operations: FifoHeap<ScoredOperation> = var_data
            .operations
            .iter()
            .filter_map(|op| {
                let statement = self.ds.node_weight(op.idx).unwrap().statement();

                let original_override = statement.override_str.clone();
                // if original_override.contains("$") {
                //     eprintln!("rejecting original override: {}", &original_override);
                //     return None;
                // }

                // TODO: no_weak_default option
                // if var_op_kind == VariableOperationKind::WeakDefault {
                //     return None;
                // }

                // Handle getting the value of a variable override flavor, e.g. `get_var("MY_VAR:a")`.
                // In that case, pre-filter operations to select those with override strings starting with "a"
                if !var_suffix.is_empty()
                    && !original_override
                        .as_ref()
                        .is_some_and(|e| split_overrides(e).starts_with(var_suffix.as_slice()))
                {
                    //dbg!(op);
                    return None;
                }

                // If in parsing mode, filter out override operators
                if parsing && statement.operator.is_override_operator() {
                    //dbg!(op);
                    return None;
                }

                if !statement
                    .kind
                    .is_active(&override_selection_context, &override_state)
                {
                    //dbg!(op);
                    return None;
                }

                //dbg!(op, &override_state);

                // TODO: this re-checks is active basically.
                let score = statement.kind.score(&override_selection_context)?;
                //dbg!(op);
                let ret = ScoredOperation {
                    stmt_index: op.idx,
                    score,
                    stmt: statement,
                };

                Some(ret)
                // TODO: place expanded LHS in the assignment cache?
            })
            // TODO: something more efficient than a fold?
            .fold(FifoHeap::new(), |mut a, b| {
                a.push(b);
                a
            });

        // eprintln!("SCORING: ");
        // for item in resolved_variable_operations.iter() {
        //     dbg!(item);
        // }
        // eprintln!("=====");

        let resolved_start_value = resolved_variable_operations.first().cloned()?;

        #[derive(Debug)]
        enum RetValue {
            Eager(String),
            Default(String),
            WeakDefault(String),
        }

        impl From<RetValue> for String {
            fn from(value: RetValue) -> Self {
                match value {
                    RetValue::Eager(s) => s,
                    RetValue::Default(s) => s,
                    RetValue::WeakDefault(s) => s,
                }
            }
        }

        impl AsRef<str> for RetValue {
            fn as_ref(&self) -> &str {
                match self {
                    RetValue::Eager(s) => s.as_ref(),
                    RetValue::Default(s) => s.as_ref(),
                    RetValue::WeakDefault(s) => s.as_ref(),
                }
            }
        }

        impl Display for RetValue {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.as_ref())
            }
        }

        let mut ret = match resolved_start_value.stmt.operator {
            Operator::Normal(normal_operator) => match normal_operator {
                NormalOperator::WeakDefault => {
                    RetValue::WeakDefault(resolved_start_value.stmt.rhs.clone())
                }
                NormalOperator::PlusEqual => {
                    RetValue::Eager(format!(" {}", resolved_start_value.stmt.rhs))
                }
                NormalOperator::EqualPlus => {
                    RetValue::Eager(format!("{} ", resolved_start_value.stmt.rhs))
                }
                _ => RetValue::Eager(resolved_start_value.stmt.rhs.clone()),
            },
            _ => RetValue::Eager(resolved_start_value.stmt.rhs.clone()),
        };

        // eprintln!(
        //     "start value = {:?} @ score: {:?} with LHS: {:?}",
        //     ret,
        //     resolved_start_value.override_score(),
        //     resolved_start_value.override_lhs()
        // );

        // TODO: filter in loop below?
        resolved_variable_operations.retain(|op| {
            // Remove the variable operation that we used for the start value, so we don't double apply
            op.stmt_index != resolved_start_value.stmt_index
                // Handle override scoring + LHS
                // TODO: clarify
                && (op.score >= resolved_start_value.score
                    || (op.stmt.operator.is_override_operator()
                        && (op.stmt.kind.override_scope() == resolved_start_value.stmt.kind.override_scope()
                            || op.stmt.kind.override_scope().is_empty())))
        });

        for op in resolved_variable_operations {
            // Weak default is handled the same as assign - priority selection happened above
            match op.stmt.operator {
                Operator::Normal(normal_operator) => match normal_operator {
                    NormalOperator::Assign => {
                        ret = RetValue::Eager(op.stmt.rhs.clone());
                    }
                    NormalOperator::WeakDefault => {
                        if !matches!(ret, RetValue::Eager(_)) {
                            ret = RetValue::WeakDefault(op.stmt.rhs.clone());
                        }
                    }
                    NormalOperator::DotEqual => {
                        if matches!(ret, RetValue::Eager(_)) {
                            ret = RetValue::Eager(ret.to_string() + &op.stmt.rhs);
                        }
                    }
                    NormalOperator::EqualDot => {
                        if matches!(ret, RetValue::Eager(_)) {
                            ret = RetValue::Eager(format!("{}{}", op.stmt.rhs, ret.as_ref()));
                        }
                    }
                    NormalOperator::PlusEqual => {
                        if matches!(ret, RetValue::Eager(_)) {
                            ret = RetValue::Eager(format!("{} {}", ret.as_ref(), op.stmt.rhs));
                        }
                    }
                    NormalOperator::EqualPlus => {
                        if matches!(ret, RetValue::Eager(_)) {
                            ret = RetValue::Eager(format!("{} {}", op.stmt.rhs, ret.as_ref()));
                        }
                    }
                    NormalOperator::Default => {
                        if matches!(ret, RetValue::WeakDefault(_)) {
                            ret = RetValue::Default(op.stmt.rhs.clone())
                        }
                    }
                },
                Operator::Override(override_operator) if !parsing => match override_operator {
                    OverrideOperator::Remove => {
                        // TODO: aggregate all removes and do it in one shot?
                        let mut removes: HashSet<String> = HashSet::new();
                        removes.insert(op.stmt.rhs.clone());
                        let new_ret = self.apply_removes(ret.as_ref(), &removes);
                        ret = RetValue::Eager(new_ret);
                    }
                    OverrideOperator::Append => {
                        ret = RetValue::Eager(ret.to_string() + &op.stmt.rhs);
                    }
                    OverrideOperator::Prepend => {
                        ret = RetValue::Eager(format!("{}{}", op.stmt.rhs, ret.as_ref()));
                    }
                },
                _ => {
                    // ignore
                }
            }
        }

        if expand {
            return Some(self.expand(ret.as_ref()).unwrap());
        }

        Some(ret.to_string())
    }

    // TODO: this should return in insertion order, like BitBake?
    pub fn get_all_keys(&self) -> Vec<String> {
        let mut ret = HashSet::new();

        self.compute_overrides().unwrap();

        let override_state = RefCell::borrow(&self.active_overrides);
        let override_state = match override_state.as_ref() {
            Some(state) => Cow::Borrowed(state),
            None => Cow::Owned(IndexSet::new()),
        };

        for var in &self.vars {
            // Iterate over statements
            let var_node = self.ds.node_weight(*var.1).unwrap().variable();
            for stmt in var_node.operations.iter() {
                let stmt_node = self.ds.node_weight(stmt.idx).unwrap().statement();
                dbg!(stmt_node);

                // TODO: check if edge between var and statement is already recorded in 'unexpanded' map to save time?

                match &stmt_node.kind {
                    // e.g. A:b:append:c
                    //
                    StatementKind::Operation { scope, filter, .. } => {
                        // Yield the override variant (for "A:b:append:c" that is "A:b"), unless ${} ... TODO
                        let mut parts = vec![var.0.clone()];
                        parts.extend(scope.into_iter().cloned());
                        ret.insert(parts.join(":"));

                        // Lop off parts of the scope until we find one that isn't active
                        while let Some(last) = parts.last()
                            && override_state.contains(last)
                            // BitBake treats A:${Q} as a var called 'A:${Q}'
                            && OVERRIDE_REGEX.is_match(last)
                        {
                            parts.pop();
                            ret.insert(parts.join(":"));
                        }
                    }

                    // e.g. A:b:c
                    StatementKind::PureOverride { scope } => {
                        debug_assert!(!scope.is_empty());

                        let mut parts = vec![var.0.clone()];
                        parts.extend(scope.into_iter().cloned());
                        ret.insert(parts.join(":"));

                        // Lop off parts of the scope until we find one that isn't active
                        while let Some(last) = parts.last()
                            && override_state.contains(last)
                            // BitBake treats A:${Q} as a var called 'A:${Q}'
                            && OVERRIDE_REGEX.is_match(last)
                        {
                            parts.pop();
                            ret.insert(parts.join(":"));
                        }
                    }
                    // e.g. A
                    StatementKind::Unconditional => {
                        ret.insert(var.0.clone());
                    }
                }

                if stmt_node
                    .kind
                    .is_active(&Cow::Owned(IndexSet::default()), &override_state)
                {}

                // match stmt_node.overrides_data.as_ref() {
                //     None => continue,
                //     Some(o) => {
                //         dbg!(&stmt_node);
                //         match o {
                //             StatementOverrides::Operation { scope, filter, .. } => {}
                //             StatementOverrides::PureOverride { .. } => {}
                //         }
                //     }
                // }
            }
        }

        ret.into_iter().sorted().collect_vec()
    }

    pub fn keys(&self) -> KeysIter {
        todo!();
    }
}

struct ParsedStatement {
    var_base: String,
    stmt: StmtNode,
}

static OVERRIDE_STR_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[^A-Z]*$").unwrap());

fn parse_statement(
    var: &str,
    normal_operator: NormalOperator,
    mut value: String,
) -> Option<ParsedStatement> {
    let var_parts = var.split_once(':');
    let base = var_parts.map_or(var, |parts| parts.0);
    let override_str = var_parts.map(|parts| parts.1);

    let stmt = match override_str {
        None => Some(StmtNode {
            override_str: None,
            operator: normal_operator.into(),
            kind: StatementKind::Unconditional,
            rhs: value,
        }),
        Some(override_str) => {
            let mut locs = KEYWORD_REGEX.capture_locations();

            // Check for override-style operators (append, prepend, and remove)
            if KEYWORD_REGEX
                .captures_read(&mut locs, override_str)
                .is_none()
            {
                return Some(ParsedStatement {
                    stmt: StmtNode {
                        override_str: Some(override_str.to_string()),
                        operator: normal_operator.into(),
                        kind: StatementKind::PureOverride {
                            scope: split_overrides(override_str),
                        },
                        rhs: value,
                    },
                    var_base: base.to_string(),
                });
            }

            let keyword_pos = locs.get(1).unwrap();
            let override_operator = match &override_str[keyword_pos.0..keyword_pos.1] {
                "append" => OverrideOperator::Append,
                "prepend" => OverrideOperator::Prepend,
                "remove" => OverrideOperator::Remove,
                _ => unreachable!(),
            };

            let operator_kind = Operator::from(override_operator);

            // TODO: BitBake gives a warning when mixing these operators
            match normal_operator {
                NormalOperator::WeakDefault => {
                    // In BitBake, remove, append, prepend are implemented as varflags. However, ??=
                    // is implemented with the _defaultval varflag. So using ??= causes assignment
                    // to the '_defaultval' varflag of the var name, e.g. TEST:remove. This is not
                    // observable, since you can't do `getVar("TEST:remove")`. So just ignore.
                    return None;
                }
                NormalOperator::PlusEqual if override_operator != OverrideOperator::Remove => {
                    // 'remove' is whitespace delimited, so don't bother adding space.
                    value = format!(" {value}");
                }
                NormalOperator::EqualPlus if override_operator != OverrideOperator::Remove => {
                    // 'remove' is whitespace delimited, so don't bother adding space.
                    value = format!("{value} ");
                }
                _ => { /* everything else is handled no differently */ }
            }

            // Overrides before the keyword - unconditionally applied, depending on the
            // start value that is selected
            let override_scope = split_overrides(&override_str[0..keyword_pos.0]);
            debug_assert!(
                !override_scope
                    .iter()
                    .any(|o| matches!(o.as_str(), "append" | "prepend" | "remove"))
            );

            eprintln!("WAT {}", &override_str[keyword_pos.1..]);

            // Overrides after the keyword - conditionally applied
            if !OVERRIDE_STR_REGEX.is_match(&override_str[keyword_pos.1..]) {
                // If not valid, then pretend there is no override (this matches bitbake's original
                // setvar regex behavior)
                return Some(ParsedStatement {
                    stmt: StmtNode {
                        override_str: None,
                        operator: normal_operator.into(),
                        kind: StatementKind::Unconditional,
                        rhs: value,
                    },
                    var_base: var.to_string(),
                });
            }

            let override_filter = split_overrides(&override_str[keyword_pos.1..]);
            debug_assert!(
                !override_filter
                    .iter()
                    .any(|o| matches!(o.as_str(), "append" | "prepend" | "remove"))
            );

            Some(StmtNode {
                override_str: Some(override_str.to_string()),
                operator: operator_kind,
                kind: StatementKind::Operation {
                    scope: override_scope,
                    filter: IndexSet::from_iter(override_filter),
                    override_operator,
                },
                rhs: value,
            })
        }
    };

    stmt.map(|s| ParsedStatement {
        stmt: s,
        var_base: base.to_string(),
    })
}
