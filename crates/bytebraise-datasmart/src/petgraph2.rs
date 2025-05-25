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
use crate::macros::get_var;
use crate::variable_operation::{StmtKind, VariableOperation};
use anyhow::bail;
use bytebraise_util::fifo_heap::FifoHeap;
use bytebraise_util::retain_with_index::RetainWithIndex;
use bytebraise_util::split::{replace_all, split_filter_empty, split_keep};
use fxhash::FxHashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use once_cell::sync::Lazy;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::matrix_graph::Nullable;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::{DefaultIx, EdgeIndex};
use regex::{Captures, Regex};
use scopeguard::{ScopeGuard, defer, guard};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::sync::LazyLock;

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
    ds: StableGraph<GraphItem, ()>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    unexpanded_operations: HashSet<EdgeIndex<DefaultIx>>,
    expand_state: RefCell<Option<ExpansionState>>,
    active_overrides: RefCell<Option<IndexSet<String>>>,
    inside_compute_overrides: RefCell<()>,
}

// fn score_overrides2(
//     active_overrides: impl IntoIterator<Item = String>,
//     candidate_overrides: &Vec<String>,
// ) -> Option<(Vec<usize>, usize, usize)> {
//     let active_overrides = active_overrides.into_iter();
//
//     if candidate_overrides.is_empty() {
//         return Some((vec![], 0, 0));
//     }
//
//     let mut ret = (
//         std::iter::repeat_n(0, active_overrides.len()).collect(),
//         0,
//         0,
//     );
//
//     let mut candidate = candidate_overrides.clone();
//
//     let counts = candidate_overrides.iter().counts();
//     ret.0 = active_overrides
//         .map(|o| counts.get(o).copied().unwrap_or_default())
//         .rev()
//         .collect();
//
//     // for (i, active_override) in active_overrides.iter().enumerate() {
//     //     if candidate_overrides.contains(active_override) {
//     //         ret.0 |= 1 << i;
//     //     }
//     // }
//
//     let mut keep_going = true;
//     'outer: while keep_going {
//         keep_going = false;
//         //eprintln!("iteration {}", ret.1);
//         ret.1 += 1;
//         for (ai, active_override) in active_overrides.enumerate() {
//             //eprintln!("\tconsider override {active_override}");
//             if candidate.len() == 1 && candidate[0] == active_override {
//                 assert_eq!(ret.2, 0);
//                 ret.2 = ai + 1;
//                 break 'outer;
//             } else if candidate.len() > 1 && candidate.ends_with(&[active_override.clone()]) {
//                 let old = candidate.clone();
//                 //eprintln!("{:?}", candidate);
//                 candidate.retain_with_index(|c, i| i == 0 || *c != active_override);
//
//                 //eprintln!("\t\ttransform {old:?} => {candidate:?}");
//
//                 assert_ne!(old, candidate);
//                 keep_going = true;
//             }
//         }
//     }
//
//     Some(ret)
// }

// TODO: better way?
static EMPTY_OVERRIDES: LazyLock<IndexSet<String>> = LazyLock::new(IndexSet::new);

type OverrideScore = (Vec<usize>, usize, usize);

fn score_override(
    active_overrides: &Option<IndexSet<String>>,
    candidate_overrides: &Vec<String>,
) -> Option<OverrideScore> {
    // Reject this override if it contains terms not in active override set
    let temp_cloned_active_overrides = active_overrides
        .as_ref()
        .unwrap_or_else(|| &EMPTY_OVERRIDES);

    let c: IndexSet<String> = candidate_overrides.iter().cloned().collect();
    if !c.is_subset(temp_cloned_active_overrides) {
        return None;
    }

    let mut ret = (vec![], 0, 0);
    if candidate_overrides.is_empty() {
        return Some(ret);
    }

    if let Some(active_overrides) = active_overrides {
        ret = (vec![], 0, 0);

        //
        let mut candidate = candidate_overrides.clone();

        let counts = candidate_overrides.iter().counts();
        // Count the # of times
        ret.0 = active_overrides
            .iter()
            .map(|o| counts.get(o).copied().unwrap_or_default())
            .rev()
            .collect();

        // for (i, active_override) in active_overrides.iter().enumerate() {
        //     if candidate_overrides.contains(active_override) {
        //         ret.0 |= 1 << i;
        //     }
        // }

        let mut keep_going = true;
        'outer: while keep_going {
            keep_going = false;
            //eprintln!("iteration {}", ret.1);
            ret.1 += 1;
            for (ai, active_override) in active_overrides.iter().enumerate() {
                //eprintln!("\tconsider override {active_override}");
                if candidate.len() == 1 && &candidate[0] == active_override {
                    assert_eq!(ret.2, 0);
                    ret.2 = ai + 1;
                    break 'outer;
                } else if candidate.len() > 1 && candidate.ends_with(std::slice::from_ref(active_override)) {
                    let old = candidate.clone();
                    //eprintln!("{:?}", candidate);
                    candidate.retain_with_index(|c, i| i == 0 || c != active_override);

                    //eprintln!("\t\ttransform {old:?} => {candidate:?}");

                    assert_ne!(old, candidate);
                    keep_going = true;
                }
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

fn split_overrides_without_keywords<S: AsRef<str>>(input: S) -> Vec<String> {
    split_overrides(input)
        .into_iter()
        .filter(|o| !matches!(o.as_str(), "append" | "prepend" | "remove"))
        .collect()
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum StatementOverrides {
    Operation {
        scope: Vec<String>,
        filter: IndexSet<String>,
        // TODO: move operation type into here?
    },
    PureOverride {
        scope: Vec<String>,
    },
}

impl StatementOverrides {
    fn is_active(
        &self,
        override_selection_context: &Option<IndexSet<String>>,
        active_overrides: &Option<IndexSet<String>>,
    ) -> bool {
        match self {
            Self::Operation { filter, scope } => {
                // For scope, consider selection context (active set + direct variant lookup)
                let lhs_valid = override_selection_context.as_ref().is_none_or(|o2| {
                    let lhs: IndexSet<String> = scope.iter().cloned().collect();
                    lhs.is_subset(o2)
                });

                // For filter, consider active override set
                let rhs_valid = active_overrides
                    .as_ref()
                    .is_none_or(|o| filter.is_subset(o));

                rhs_valid && lhs_valid
            }
            Self::PureOverride { scope } => {
                override_selection_context
                    .as_ref()
                    .is_none_or(|active_overrides| {
                        let overrides: IndexSet<String> = scope.iter().cloned().collect();
                        overrides.is_subset(active_overrides)
                    })
            }
        }
    }

    fn score(&self, active_overrides: &Option<IndexSet<String>>) -> Option<OverrideScore> {
        // The score is derived from the scope alone
        match self {
            Self::Operation { scope, .. } => score_override(active_overrides, scope),
            Self::PureOverride { scope } => score_override(active_overrides, scope),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum ResolvedStatementKind {
    Operation {
        scope: Vec<String>,
        filter: IndexSet<String>,
        score: OverrideScore,
    },
    PureOverride {
        scope: Vec<String>,
        score: OverrideScore,
    },
    Unconditional,
}

impl ResolvedStatementKind {
    fn override_score(&self) -> OverrideScore {
        match self {
            Self::Unconditional => (vec![], 0, 0),
            Self::Operation { score, .. } => score.clone(),
            Self::PureOverride { score, .. } => score.clone(),
        }
    }

    fn override_scope(&self) -> Vec<String> {
        match self {
            Self::Unconditional => vec![],
            Self::Operation { scope, .. } => scope.clone(),
            Self::PureOverride { scope, .. } => scope.clone(),
        }
    }

    fn is_override_operation(&self) -> bool {
        matches!(self, Self::Operation { .. })
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct ResolvedVariableOperation<'a> {
    kind: ResolvedStatementKind,
    stmt: &'a StmtNode,
    stmt_index: NodeIndex,
}

impl<'a> Ord for ResolvedVariableOperation<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.kind
            .override_score()
            .cmp(&other.kind.override_score())
            .reverse()
            .then(self.stmt.kind.cmp(&other.stmt.kind))
    }
}

impl<'a> PartialOrd for ResolvedVariableOperation<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Default for DataSmart {
    fn default() -> Self {
        Self::new()
    }
}

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

    fn apply_removes(&self, input: &String, removes: &HashSet<String>) -> String {
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
        self.set_var_ex(var, value, Some(StmtKind::PlusEqual));
    }

    pub fn equals_plus_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, Some(StmtKind::EqualPlus));
    }

    pub fn equals_dot_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, Some(StmtKind::EqualDot));
    }

    pub fn dot_equals_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, Some(StmtKind::DotEqual));
    }

    pub fn weak_default_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, Some(StmtKind::WeakDefault));
    }

    pub fn default_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, Some(StmtKind::Default));
    }

    fn set_var_ex<T: Into<String>, S: Into<String>>(
        &mut self,
        var: T,
        value: S,
        mut stmt_kind: Option<StmtKind>,
    ) -> NodeIndex<DefaultIx> {
        let var = var.into();
        let value = value.into();

        dbg!(&var);

        let var_parts = var.split_once(':');
        let base = var_parts.map_or(var.as_str(), |parts| parts.0);
        let override_str = var_parts.map(|parts| parts.1);

        let mut overrides_data = None;
        if let Some(override_str) = override_str {
            let mut locs = KEYWORD_REGEX.capture_locations();

            // Check for override-style operators (append, prepend, and remove)
            if KEYWORD_REGEX
                .captures_read(&mut locs, override_str)
                .is_some()
            {
                match stmt_kind.as_ref() {
                    None | Some(StmtKind::Assign) => { /* fine */ }
                    Some(_) => {
                        todo!("append/prepend/remove combined with operators");
                    }
                }

                let keyword_pos = locs.get(1).unwrap();
                match &override_str[keyword_pos.0..keyword_pos.1] {
                    "append" => {
                        stmt_kind = Some(StmtKind::Append);
                    }
                    "prepend" => {
                        stmt_kind = Some(StmtKind::Prepend);
                    }
                    "remove" => {
                        stmt_kind = Some(StmtKind::Remove);
                    }
                    _ => unreachable!(),
                };

                // Overrides before the keyword - unconditionally applied, depending on the
                // start value that is selected
                let override_scope = split_overrides(&override_str[0..keyword_pos.0]);
                debug_assert!(!override_scope.iter().any(|o| matches!(o.as_str(), "append" | "prepend" | "remove")));

                // Overrides after the keyword - conditionally applied
                let override_filter = split_overrides(&override_str[keyword_pos.1..]);
                debug_assert!(!override_filter.iter().any(|o| matches!(o.as_str(), "append" | "prepend" | "remove")));

                overrides_data = Some(StatementOverrides::Operation {
                    scope: override_scope,
                    filter: IndexSet::from_iter(override_filter),
                });
            } else {
                let overrides = split_overrides(override_str);
                overrides_data = Some(StatementOverrides::PureOverride { scope: overrides });
            }
        }

        let stmt_kind = stmt_kind.unwrap_or(StmtKind::Assign);

        // TODO: if parsing, and no keyword given, then wipe away removes, prepends, and appends
        //  Also need to do something with overrides - not sure what though (set setVar())

        let stmt_idx = self.ds.add_node(GraphItem::StmtNode(StmtNode {
            override_str: override_str.map(String::from),
            overrides_data,
            kind: stmt_kind,
            rhs: value,
        }));

        // Lookup variable base (stem) and create if it doesn't exist
        let var_entry = self
            .vars
            .entry(base.to_string())
            .or_insert_with(|| self.ds.add_node(GraphItem::new_variable(base)));

        let var_data = self.ds.node_weight_mut(*var_entry).unwrap().variable_mut();

        var_data.operations.push(VariableOperation {
            op_type: stmt_kind,
            idx: stmt_idx,
        });

        let e = self.ds.add_edge(*var_entry, stmt_idx, ());

        // If any part of the key contains '${' (not just the base), then track it as unexpanded.
        // Use [`DataSmart::expand_vars`] to expand it later.
        if var.contains("${") {
            self.unexpanded_operations.insert(e);
        }

        *var_entry
    }

    pub fn set_var<T: Into<String>, S: Into<String>>(
        &mut self,
        var: T,
        value: S,
    ) -> NodeIndex<DefaultIx> {
        let var = var.into();
        let value = value.into();

        #[cfg(test)]
        if var == "OVERRIDES" && self.vars.contains_key("OVERRIDES") {
            unimplemented!(
                "OVERRIDES are already set! Re-computing OVERRIDES not implemented yet."
            );
        }

        self.set_var_ex(var, value, None)
    }

    pub fn expand<S: AsRef<str>>(&self, value: S) -> DataSmartResult<String> {
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

    pub fn del_var<S: AsRef<str>>(&mut self, var: S) -> DataSmartResult<()> {
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
                    && o == o2 {
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

    pub fn rename_var<A: AsRef<str>, B: AsRef<str>>(
        &mut self,
        old: A,
        new: B,
    ) -> DataSmartResult<()> {
        let old = old.as_ref();
        let new = new.as_ref();

        let old_base_var = old.split_once(':').map_or(old, |p| p.0);
        let new_base_var = new.split_once(':').map_or(new, |p| p.0);

        eprintln!("renameVar({old}, {new})");
        eprintln!("vars before call: {}", self.vars.keys().sorted().join(", "));

        if old == new {
            bail!("Calling renameVar with equivalent keys {old} is invalid");
        }

        // Get unexpanded value of the full old var, and assign it to new var
        if let Some(old_val) = get_var!(&self, old, parsing = true, expand = false) {
            eprintln!("getVar({old}) = {old_val}");
            eprintln!("setVar({new}, {old_val}");
            // TODO: parsing mode?
            self.set_var(new, old_val);
        }

        // Next, transplant :appends, :prepends, and :removes
        let new_var_index = *self.vars.get(new_base_var).unwrap();
        let old_var_index = *self.vars.get(old_base_var).unwrap();

        if new_var_index != old_var_index {
            let old_var_node = self.ds.node_weight(old_var_index).unwrap().variable();

            for op in old_var_node.operations.clone().into_iter() {
                match op.op_type {
                    StmtKind::Append | StmtKind::Prepend | StmtKind::Remove => {
                        self.ds.add_edge(new_var_index, op.idx, ());
                        self.ds
                            .node_weight_mut(new_var_index)
                            .unwrap()
                            .variable_mut()
                            .operations
                            .push(op);
                        //(op);
                    }
                    _ => continue,
                }
            }
        } else {
            eprintln!("bypass");
        }

        //dbg!(&self.ds);

        eprintln!("del_var({old})");
        self.del_var(old)?;

        eprintln!("vars after call: {}", self.vars.keys().sorted().join(", "));

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
    pub fn expand_keys(&mut self) -> DataSmartResult<Vec<String>> {
        let mut unexpanded_operations = std::mem::take(&mut self.unexpanded_operations);
        dbg!(&unexpanded_operations);

        let mut operations = BTreeMap::new();

        #[derive(Debug)]
        struct Operation {
            kind: UnexpandedOperationKind,
            stmt_index: NodeIndex<DefaultIx>,
            var_index: NodeIndex<DefaultIx>,
        }

        #[derive(Debug)]
        enum UnexpandedOperationKind {
            Variable,
            Override,
            Both,
        }

        // TODO: if not all 'override' operations, then need to actually do renameKey?

        // Reconstruct the unexpanded keys
        for edge in unexpanded_operations.drain() {
            let node = self.ds.edge_endpoints(edge).unwrap();
            let nodes = self.ds.index_twice_mut(node.0, node.1);

            let mut kind = None;
            let mut name = nodes.0.variable().name.clone();
            if name.contains("${") {
                kind = Some(UnexpandedOperationKind::Variable);
            }

            // TODO: use overrides_data, not override_str. Then get rid of the latter.
            if let Some(b) = nodes.1.statement().override_str.clone() {
                // TODO: more efficient
                let overrides = split_overrides_without_keywords(&b);
                if !overrides.is_empty() {
                    name.push(':');
                    name.push_str(&overrides.join(":"));
                }

                if b.contains("${") {
                    if matches!(kind, Some(UnexpandedOperationKind::Variable)) {
                        kind = Some(UnexpandedOperationKind::Both);
                    } else {
                        kind = Some(UnexpandedOperationKind::Override);
                    }
                }
            }

            let expanded = self.expand(&name)?;
            operations.insert(name, expanded);
        }

        // TODO: BitBake stores override variants as separate variables. So given "TEST:${A}:b:a",
        //  we also need to produce "TEST:${A}:b", "TEST:${A}" and rename those in this list.
        let ret = operations.keys().cloned().collect_vec();
        for o in operations.into_iter() {
            self.rename_var(o.0, o.1)?;
        }

        Ok(ret)
    }

    fn compute_overrides(&self) -> DataSmartResult<()> {
        if let Ok(_) = RefCell::try_borrow_mut(&self.inside_compute_overrides) {
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

    pub fn get_var<S: AsRef<str>>(&self, var: S, parsing: bool, expand: bool) -> Option<String> {
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
        if var_suffix.iter().any(|o| matches!(o.as_str(), "append" | "prepend" | "remove")) {
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

        let override_state = &*RefCell::borrow(&self.active_overrides);

        // The union of active overrides with whatever overrides were provided in the
        // direct-variant lookup. This is only used for override-scoped operators.
        let override_selection_context: Option<IndexSet<String>> = match var_suffix.is_empty() {
            false => {
                // TODO: revisit: are we sure the new overrides should be inserted into the beginning?
                let mut new_overrides = IndexSet::from_iter(var_suffix.clone());
                for old_override in override_state.clone().unwrap_or_default() {
                    new_overrides.insert(old_override);
                }

                Some(new_overrides)
            }
            true => override_state.clone(),
        };

        // Calculate override scores for operations
        let mut resolved_variable_operations: FifoHeap<ResolvedVariableOperation> = var_data
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
                    && !original_override.as_ref().is_some_and(|e| {
                        split_overrides(e).starts_with(var_suffix.as_slice())
                    }) {
                        return None;
                    }

                let resolved_stmt_kind = {
                    match statement.overrides_data.as_ref() {
                        Some(o @ StatementOverrides::Operation { scope, filter }) => {
                            if !o.is_active(&override_selection_context, override_state) {
                                return None;
                            }

                            let score = o.score(&override_selection_context);
                            ResolvedStatementKind::Operation {
                                scope: scope.clone(),
                                filter: filter.clone(),
                                score: score.unwrap(),
                            }
                        }
                        Some(o @ StatementOverrides::PureOverride { scope: overrides }) => {
                            if !o.is_active(&override_selection_context, override_state) {
                                return None;
                            }

                            let score = o.score(&override_selection_context);
                            ResolvedStatementKind::PureOverride {
                                scope: overrides.clone(),
                                score: score.unwrap(),
                            }
                        }
                        None => ResolvedStatementKind::Unconditional,
                    }
                };

                let ret = ResolvedVariableOperation {
                    stmt_index: op.idx,
                    kind: resolved_stmt_kind,
                    stmt: statement,
                };

                Some(ret)
                // TODO: place expanded LHS in the assignment cache?
            })
            .filter(|o| !matches!((parsing, o.stmt.kind), (true, StmtKind::Append | StmtKind::Prepend | StmtKind::Remove)))
            // TODO: something more efficient than a fold?
            .fold(FifoHeap::new(), |mut a, b| {
                a.push(b);
                a
            });

        //dbg!(&resolved_variable_operations);

        // eprintln!("SCORING: ");
        // for item in resolved_variable_operations.heap.iter() {
        //     eprintln!(
        //         "{}={} => {:?}",
        //         item.0.unexpanded_override,
        //         item.0.value,
        //         item.0.override_score()
        //     );
        // }
        //
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

        let mut ret = match resolved_start_value.stmt.kind {
            StmtKind::WeakDefault => RetValue::WeakDefault(resolved_start_value.stmt.rhs.clone()),
            _ => RetValue::Eager(match resolved_start_value.stmt.kind {
                StmtKind::PlusEqual => format!(" {}", resolved_start_value.stmt.rhs),
                StmtKind::EqualPlus => format!("{} ", resolved_start_value.stmt.rhs),
                _ => resolved_start_value.stmt.rhs.clone(),
            }),
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
                && (op.kind.override_score() >= resolved_start_value.kind.override_score()
                    || (op.kind.is_override_operation()
                        && (op.kind.override_scope() == resolved_start_value.kind.override_scope()
                            || op.kind.override_scope().is_empty())))
        });

        for op in resolved_variable_operations {
            match op.stmt.kind {
                // Weak default is handled the same as assign - priority selection happened above
                StmtKind::Assign => {
                    ret = RetValue::Eager(op.stmt.rhs.clone());
                }
                StmtKind::WeakDefault => {
                    if !matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::WeakDefault(op.stmt.rhs.clone());
                    }
                }
                StmtKind::Remove if !parsing => {
                    // TODO: aggregate all removes and do it in one shot?
                    let mut removes: HashSet<String> = HashSet::new();
                    removes.insert(op.stmt.rhs.clone());
                    let new_ret = self.apply_removes(&ret.into(), &removes);
                    ret = RetValue::Eager(new_ret);
                }
                StmtKind::Append if !parsing => {
                    ret = RetValue::Eager(ret.to_string() + &op.stmt.rhs);
                }
                StmtKind::DotEqual => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(ret.to_string() + &op.stmt.rhs);
                    }
                }
                StmtKind::Prepend if !parsing => {
                    ret = RetValue::Eager(format!("{}{}", op.stmt.rhs, ret.as_ref()));
                }
                StmtKind::EqualDot => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(format!("{}{}", op.stmt.rhs, ret.as_ref()));
                    }
                }
                StmtKind::PlusEqual => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(format!("{} {}", ret.as_ref(), op.stmt.rhs));
                    }
                }
                StmtKind::EqualPlus => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(format!("{} {}", op.stmt.rhs, ret.as_ref()));
                    }
                }
                StmtKind::Default => {
                    if matches!(ret, RetValue::WeakDefault(_)) {
                        ret = RetValue::Default(op.stmt.rhs.clone())
                    }
                }
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
}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>,
    cached_value: RefCell<Option<String>>,
    // map of varflag name => heap of operations
    // for example, in:
    //   A[depends] = "q'
    // the varflag name is 'depends', and a single operation is added to assign "q"

    // TODO: unlike with the implicit _content vargflag (which is covered by the first-class citizen
    //  `operations`), varflag operations do not care about `OVERRIDES`. However, we still make use of
    //  the operation lhs, since you can do stuff like this:
    //      A[depends] = "q"
    //      A:pn-specific[depends] = "d"
    //  Calling get_var_flag on "A" in the context of 'specific' recipe does NOT however return "d".
    //  Calling get_var_flag "A:pn-specific" does give "d", however.
    //  This is kind of confusiong, so perhaps we shouldn't blinding re-use `VariableOperation` here,
    //  since the semantics are so different.
    varflags: BTreeMap<String, FifoHeap<VariableOperation>>, // TODO: iterative cache for OVERRIDES
}

#[derive(Debug, PartialEq, Eq)]
struct StmtNode {
    kind: StmtKind,

    override_str: Option<String>,

    overrides_data: Option<StatementOverrides>,

    /// The value
    rhs: String,
}

#[derive(Debug)]
enum GraphItem {
    Variable(Variable),
    StmtNode(StmtNode),
}

impl GraphItem {
    fn variable_mut(&mut self) -> &mut Variable {
        match self {
            GraphItem::Variable(v) => v,
            _ => panic!("Expected GraphItem::Variable"),
        }
    }

    fn variable(&self) -> &Variable {
        match self {
            GraphItem::Variable(v) => v,
            _ => panic!("Expected GraphItem::Variable"),
        }
    }

    fn to_variable(self) -> Variable {
        match self {
            GraphItem::Variable(v) => v,
            _ => panic!("Expected GraphItem::Variable"),
        }
    }

    fn statement(&self) -> &StmtNode {
        match self {
            GraphItem::StmtNode(stmt) => stmt,
            _ => panic!("Expected GraphItem::Statement"),
        }
    }

    fn statement_mut(&mut self) -> &mut StmtNode {
        match self {
            GraphItem::StmtNode(stmt) => stmt,
            _ => panic!("Expected GraphItem::Statement"),
        }
    }
}

impl GraphItem {
    fn new_variable<T: Into<String>>(name: T) -> GraphItem {
        GraphItem::Variable(Variable {
            name: name.into(),
            operations: FifoHeap::new(),
            cached_value: RefCell::new(None),
            varflags: BTreeMap::new(),
        })
    }
}

#[cfg(test)]
mod test {
    use crate::evaluate::eval;
    use crate::macros::get_var;
    use crate::petgraph2::{DataSmart, score_override};
    use indexmap::IndexSet;

    fn score<S: AsRef<str>>(input: S) -> (Vec<usize>, usize, usize) {
        let input = input.as_ref().replace(':', "");
        let active_overrides: IndexSet<String> =
            vec!["a", "b", "c"].drain(..).map(String::from).collect();

        let candidate: Vec<String> = input.chars().map(String::from).collect();
        let ret = score_override(&Some(active_overrides.clone()), &candidate).unwrap();

        eprintln!("{input} => {ret:?}");

        ret
    }

    #[test]
    fn doc_examples() {
        let d = eval(
            r#"
A = "1"
A:append = "2"
A:append = "3"
A += "4"
A .= "5"
        "#,
        );

        assert_eq!(get_var!(&d, "A").unwrap(), "1 4523");
    }

    #[test]
    fn none() {
        let d = DataSmart::new();
        assert_eq!(get_var!(&d, "NOT_EXIST"), None);
    }

    #[test]
    fn multiple_append() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:append", "2");
        d.set_var("TEST:append", "2");

        assert_eq!(get_var!(&d, "TEST"), Some("122".into()));
    }

    #[test]
    fn override_score() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("OVERRIDES", "more");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn override_score_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("OVERRIDES", "more:specific");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_score_4() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more:append", "2");

        // All applicable overrides (at the same score level) are applied.
        d.set_var("TEST:more:specific", "3");
        d.set_var("TEST:more:specific", "4");
        d.set_var("OVERRIDES", "more:specific");

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn override_score_5() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("TEST:more:specific", "4");
        d.set_var("TEST:more", "5");
        d.set_var("TEST:more", "6");
        d.set_var("OVERRIDES", "more");

        assert_eq!(get_var!(&d, "TEST"), Some("6".into()));
    }

    #[test]
    fn override_score_6() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("TEST:more:specific", "4");
        d.set_var("TEST:more", "5");
        d.set_var("TEST:more", "6");
        d.set_var("OVERRIDES", "");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_7() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:append", "2");

        assert_eq!(get_var!(&d, "TEST"), Some("12".into()));
    }

    #[test]
    fn override_score_8() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");

        assert_eq!(get_var!(&d, "TEST"), Some("23".into()));
    }

    #[test]
    fn override_score_9() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");

        assert_eq!(get_var!(&d, "TEST"), Some("234".into()));
    }

    #[test]
    fn override_score_10() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:append:a", "NO");

        assert_eq!(get_var!(&d, "TEST"), Some("234".into()));
    }

    #[test]
    fn override_score_11() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:b:append", "BASE");
        d.set_var("OVERRIDES", "b");

        assert_eq!(get_var!(&d, "TEST"), Some("BASE34".into()));
    }

    #[test]
    fn override_score_12() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:b:append", "BASE");
        d.set_var("TEST:b", "OH YES");
        d.set_var("OVERRIDES", "b");

        assert_eq!(get_var!(&d, "TEST"), Some("OH YESBASE34".into()));
    }

    #[test]
    fn override_score_13() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:b:append", "BASE");
        d.set_var("TEST:b", "OH YES");
        d.set_var("TEST:c", "WHAT");
        d.set_var("OVERRIDES", "b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("WHAT34".into()));
    }

    #[test]
    fn override_score_14() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:b:append", "BASE");
        d.set_var("TEST:b", "OH YES");
        d.set_var("TEST:c", "WHAT");
        d.set_var("TEST:c:append", "!");
        d.set_var("OVERRIDES", "b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("WHAT!34".into()));
    }

    #[test]
    fn override_score_15() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:b:append", "BASE");
        d.set_var("TEST:b", "OH YES");
        d.set_var("TEST:c:prepend", "Q");
        d.set_var("TEST:c", "WHAT");
        d.set_var("TEST:c:append", "!");
        d.set_var("OVERRIDES", "b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_score_16() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "10");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");

        assert_eq!(get_var!(&d, "TEST"), Some("1034".into()));
    }

    #[test]
    fn override_score_17() {
        let mut d = DataSmart::new();
        d.set_var("TEST:append", "why?");
        d.set_var("TEST:a:b:append", "first");
        d.set_var("TEST:a:b:${OP}", "OP");
        d.set_var("OP", "append");
        d.set_var("OVERRIDES", "a:b");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some("firstOPwhy?".into()));
    }

    #[test]
    fn override_score_18() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:b:append", "base");
        d.set_var("TEST:b", "OH YES");
        d.set_var("TEST:c:prepend", "Q");
        d.set_var("TEST:c", "WHAT");
        d.set_var("TEST:c:append", "!");
        d.set_var("OVERRIDES", "b:c:");

        assert_eq!(get_var!(&d, "TEST"), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_priority_order() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a", "2");
        d.set_var("TEST:b", "3");
        d.set_var("TEST:b:a", "6");
        d.set_var("TEST:a:b", "5");

        score("");
        score("a");
        score("b");
        score("b:a");
        score("a:b");

        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("5".into()));
    }

    #[test]
    fn override_priority_order_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a", "2");
        d.set_var("TEST:b", "3");
        d.set_var("TEST:a:b", "5");
        d.set_var("TEST:b:a", "6");

        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("5".into()));
    }

    fn override_priority_order_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:c:b:a", "2");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("TEST:c:a:b", "4");
        d.set_var("TEST:b:a:c", "5");
        d.set_var("TEST:b:c:a", "6");

        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    fn override_priority_order_4() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a", "2");
        d.set_var("TEST:a:a", "3");

        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }
    #[test]
    fn override_selection_order_sensitivity() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:b:a:append", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:b:a:append", "2");
        d.set_var("TEST:a:b:append", "3");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:append", "3");
        d.set_var("TEST:b:a:append", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_4() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:append", "3");
        d.set_var("TEST:b:a:append", "2");
        d.set_var("TEST:a:b:a:append", "4");
        d.set_var("OVERRIDES", "a:b:c");

        score("ab");
        score("ba");
        score("aba");

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn tricky_1() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:c:b", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:c:b:a:b:c", "2");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));

        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("TEST:c:b:a:b:c", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("TEST:a:b:c:a", "4");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));

        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:c:a", "4");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn filter_order_sensitivity() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:append:b:a", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("12".into()));
    }

    #[test]
    fn order_of_operations() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");
        d.set_var("TEST:${B}", "2");
        d.set_var("TEST:append", " 4");
        d.set_var("B", "remove");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some("1  3 4".into()));
    }

    #[test]
    fn indirection_1() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");
        d.set_var("TEST:${${B}}", "2");
        d.set_var("B", "${W}");
        d.set_var("W", "Q");
        d.set_var("Q", "remove");

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:remove");
        assert_eq!(get_var!(&d, "TEST"), Some("1  3".into()));
    }

    #[test]
    fn indirection_and_order() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");

        d.set_var("TEST:${${B}}", " 4 ");
        d.set_var("B", "${W}");
        d.set_var("W", "Q");
        d.set_var("Q", "append");

        d.set_var("TEST:append", " 5 ");

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:append");
        assert_eq!(get_var!(&d, "TEST"), Some("1 2 3 5  4 ".into()));
    }

    #[test]
    fn indirection_and_order_and_score() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");

        d.set_var("TEST:${${B}}", " 4 ");
        d.set_var("B", "${W}");
        d.set_var("W", "Q");
        d.set_var("Q", "append");

        d.set_var("TEST:append", " 5 ");

        d.set_var("TEST:b:append", "OK");
        d.set_var("OVERRIDES", "b");

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:append");
        assert_eq!(get_var!(&d, "TEST"), Some("OK 5  4 ".into()));
    }

    #[test]
    fn dumb() {
        let mut d = eval(
            r#"
TEST = "base"
TEST:append = "1"
TEST${B} = " wat"
TEST:append = "2"

B = ":append"

        "#,
        );

        d.expand_keys().unwrap();
        assert_eq!(get_var!(&d, "TEST").unwrap(), "base12 wat");
    }

    #[test]
    fn synthesized_appends() {
        // The behavior of appends vs synthesized appends is different.
        // Normal appends stack:
        // let mut d = DataSmart::new();
        // d.set_var("TEST", "10");
        // d.set_var("TEST:append", "1");
        // d.set_var("TEST:append", "2");
        // assert_eq!(get_var!(&d, "TEST"), Some("1012".into()));

        // But synthesized appends only take the last one:
        let mut d = DataSmart::new();
        d.set_var("TEST", "10");
        d.set_var("TEST:${A}", "1");
        d.set_var("TEST:${A}", "2");
        d.set_var("A", "append");
        let p = d.expand_keys().unwrap();
        assert_eq!(get_var!(&d, "TEST"), Some("102".into()));
    }

    #[test]
    fn plus_equals() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.plus_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("base 2".into()));
    }

    #[test]
    fn plus_equals_no_base() {
        let mut d = DataSmart::new();
        d.plus_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some(" 2".into()));
    }

    #[test]
    fn dot_equals() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.dot_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("base2".into()));
    }

    #[test]
    fn dot_equals_no_base() {
        let mut d = DataSmart::new();
        d.dot_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn equals_plus() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.equals_plus_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2 base".into()));
    }

    #[test]
    fn equals_plus_no_base() {
        let mut d = DataSmart::new();
        d.equals_plus_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2 ".into()));
    }

    #[test]
    fn equals_dot() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.equals_dot_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2base".into()));
    }

    #[test]
    fn equals_dot_no_base() {
        let mut d = DataSmart::new();
        d.dot_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default_2() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        d.weak_default_var("TEST", "3");
        d.weak_default_var("TEST", "4");
        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_doc_example() {
        let mut d = DataSmart::new();
        d.weak_default_var("W", "x");
        d.plus_equals_var("W", "y");
        assert_eq!(get_var!(&d, "W"), Some(" y".into()));

        let mut d = DataSmart::new();
        d.weak_default_var("W", "x");
        d.set_var("W:append", "y");
        assert_eq!(get_var!(&d, "W"), Some("xy".into()));
    }

    #[test]
    fn weak_default_priority() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        d.weak_default_var("TEST:a", "3");
        d.weak_default_var("TEST:a:b", "4");
        d.weak_default_var("TEST:b", "5");

        d.set_var("OVERRIDES", "a:b");
        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_append() {
        let mut d = DataSmart::new();

        d.set_var("TEST", "");
        d.set_var("TEST:append", "wat");
        d.weak_default_var("TEST:a", "OK");
        d.set_var("OVERRIDES", "a:b");

        assert_eq!(get_var!(&d, "TEST"), Some("OKwat".into()));
    }

    #[test]
    fn append_interactions() {
        let mut d = DataSmart::new();

        d.set_var("TEST", "1");
        d.set_var("TEST:a:b", "2");
        d.set_var("TEST:a:b:a:append", "3");
        d.plus_equals_var("TEST:a:b:a", "5");
        d.plus_equals_var("TEST:a:b", "6");
        d.set_var("OVERRIDES", "a:b");

        assert_eq!(get_var!(&d, "TEST"), Some(" 53".into()));
    }

    #[test]
    fn more_synthesized_appends() {
        let mut d = DataSmart::new();

        d.set_var("TEST", "1");
        d.set_var("TEST:a:b", "2");
        d.set_var("TEST:a:b:a:append", "3");
        d.plus_equals_var("TEST:a:b:a", "5");
        d.plus_equals_var("TEST:a:b", "6");

        d.set_var("OP", "append");
        d.set_var("TEST:a:b:${OP}", "Q");

        d.set_var("A", "a");
        d.set_var("TEST:${A}:b:a:append", "7");
        d.set_var("TEST:${A}:b:a:append", "7");

        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some(" 53".into()));

        let ret = d.expand_keys().unwrap();
        assert_eq!(ret, Vec::<String>::new());

        assert_eq!(get_var!(&d, "TEST"), Some(" 5377".into()));
    }

    #[test]
    fn more_synthesized_appends_2() {
        let mut d = DataSmart::new();

        d.set_var("TEST", "1");
        d.set_var("TEST:a:b", "2");
        d.set_var("TEST:a:b:a:append", "3");
        d.plus_equals_var("TEST:a:b:a", "5");
        d.plus_equals_var("TEST:a:b", "6");

        d.set_var("OP", "append");
        d.set_var("TEST:a:b:${OP}", "Q");

        d.set_var("A", "a");
        d.set_var("TEST:${A}:b:a:${OP}", "7");
        d.set_var("TEST:${A}:b:a:${OP}", "7");

        d.set_var("OVERRIDES", "a:b:c");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some(" 537".into()));
    }

    #[test]
    fn default_var() {
        let mut d = DataSmart::new();

        d.default_var("TEST", "1");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));

        let mut d = DataSmart::new();

        d.default_var("TEST", "1");
        d.default_var("TEST", "2");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn default_precedence() {
        let mut d = DataSmart::new();

        d.weak_default_var("TEST", "2");
        d.default_var("TEST", "1");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn weak_default_precedence() {
        let mut d = DataSmart::new();

        d.weak_default_var("TEST:a", "2");
        d.default_var("TEST", "1");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn finalization() {
        let mut d = DataSmart::new();

        d.set_var("A${B}", "X");
        d.set_var("B", "2");
        d.set_var("A2", "Y");

        d.expand_keys().unwrap();
        assert_eq!(get_var!(&d, "A2"), Some("X".into()));
    }

    #[test]
    fn key_expansion() {
        let mut d = DataSmart::new();

        d.set_var("TEST${A}", "1");
        assert_eq!(get_var!(&d, "TEST${A}"), Some("1".into()));

        d.set_var("TEST2", "2");
        d.set_var("A", "2");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST2"), Some("1".into()));
    }

    // #[test]
    // fn get_var_varflag_operations() {
    //     let mut d = DataSmart::new();
    //     d.set_var("P", "");
    //     d.set_var("P:append", "append!");
    //     assert!(get_var!(&d, "P:append").is_none());
    // }

    #[test]
    fn test_wat() {
        let mut d = DataSmart::new();
        d.set_var("P", "");
        d.set_var("P:a", "append");
        d.set_var("Q", "base ");
        d.set_var("Q:${P}", "OK2");
        d.set_var("Q:append", "me first");
        d.set_var("OVERRIDES", "a");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "Q").unwrap(), "base me firstOK2");
    }

    #[test]
    fn variable_roots_1() {
        let mut d = DataSmart::new();
        d.set_var("P:inactive", ":)");
        //dbg!(&d);
        //assert!(get_var!(&d, "P").is_none());
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_2() {
        let mut d = DataSmart::new();
        d.set_var("P", "p");
        d.set_var("P:inactive", ":)");
        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_3() {
        let mut d = DataSmart::new();
        d.set_var("P", "p");
        d.set_var("P:t", "t");
        d.set_var("P:inactive", ":)");
        d.set_var("OVERRIDES", "t");
        assert_eq!(get_var!(&d, "P").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:t").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_4() {
        let mut d = DataSmart::new();
        d.set_var("P", "p");
        d.set_var("P:t", "t");
        d.set_var("P:inactive", ":)");
        d.set_var("P:append", "base");
        d.set_var("OVERRIDES", "t");

        assert_eq!(get_var!(&d, "P").unwrap(), "tbase");

        // selected start values is P:t, so P:append doesn't apply
        assert_eq!(get_var!(&d, "P:t").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_5() {
        let mut d = DataSmart::new();
        d.set_var("P", "p");
        d.set_var("P:inactive", ":)");
        d.set_var("P:inactive:append", "!");

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)!");
    }

    #[test]
    fn variable_roots_6() {
        let mut d = DataSmart::new();
        d.set_var("P", "p");
        d.set_var("P:inactive", ":)");
        // this :append only applies when 'inactive' is in override set, which it's not
        d.set_var("P:append:inactive", "!");

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_7() {
        let mut d = DataSmart::new();
        d.set_var("P", "p");
        d.set_var("P:inactive", ":)");
        // applies if P:inactive is selected as starting value
        d.set_var("P:inactive:append", "@");
        // this :append only applies when 'inactive' is in override set, which it's not
        d.set_var("P:append:inactive", "!");
        // applies if P:inactive is selected as starting value AND 'inactive' is in override set
        d.set_var("P:inactive:append:inactive", "?");

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)@");
    }

    #[test]
    fn variable_roots_8() {
        let mut d = DataSmart::new();
        d.set_var("P", "p");
        d.set_var("P:O", "t");
        d.set_var("P:O:append", "!");
        d.set_var("OVERRIDES", "O");
        //assert_eq!(get_var!(&d, "P").unwrap(), "t!");
        assert!(get_var!(&d, "P:notexist").is_none());
        assert_eq!(get_var!(&d, "P:O").unwrap(), "t!");
    }

    #[test]
    fn variable_roots_9() {
        let mut d = DataSmart::new();
        d.set_var("Q", "q");
        d.set_var("Q:${IN}", "t");
        d.set_var("IN", "please");
        //assert_eq!(get_var!(&d, "Q").unwrap(), "q");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "Q:please").unwrap(), "t");
    }

    #[test]
    fn variable_roots_10() {
        let mut d = DataSmart::new();
        d.set_var("Q", "q");
        d.set_var("Q:a:b", "ab");
        d.set_var("Q:b:a", "ba");
        assert_eq!(get_var!(&d, "Q:a:b").unwrap(), "ab");
        assert_eq!(get_var!(&d, "Q:b:a").unwrap(), "ba");
    }

    #[test]
    fn get_var_append() {
        let d = eval(
            r#"
TEST = "a"
TEST:append = "b"
        "#,
        );

        assert_eq!(get_var!(&d, "TEST:append"), None);
    }

    #[test]
    fn doccc() {
        let d = eval(
            r#"
MY_VAR = "base"
MY_VAR:a = "different"
MY_VAR:a:append = "!"
MY_VAR:append:a = "?"
        "#,
        );

        assert_eq!(get_var!(&d, "MY_VAR").unwrap(), "base");
        assert_eq!(get_var!(&d, "MY_VAR:a").unwrap(), "different!");
    }

    #[test]
    fn doccc2() {
        let d = eval(
            r#"
MY_VAR = "base"
MY_VAR:a = "different"
MY_VAR:a:append = "!"
MY_VAR:append:a = "?"
OVERRIDES = "a:b:c"
        "#,
        );

        assert_eq!(get_var!(&d, "MY_VAR").unwrap(), "different!?");
        assert_eq!(get_var!(&d, "MY_VAR:a").unwrap(), "different!");
    }
}
