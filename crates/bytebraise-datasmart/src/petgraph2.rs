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
    - Non-string data - this should print 3:
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
use crate::macros::{get_var, set_var_ex};
use crate::nodes::{GraphItem, ScoredOperation, Variable};
use crate::variable_operation::{NormalOperator, Operator, OverrideOperator, VariableOperation};
use crate::variable_parser::VariableExpressionKind::{Assignment, OverrideOperation};
use crate::variable_parser::{parse_statement, parse_variable};
use anyhow::bail;
use bytebraise_util::fifo_heap::FifoHeap;
use bytebraise_util::retain_with_index::RetainWithIndex;
use bytebraise_util::split::{replace_all, split_filter_empty, split_keep};
use fxhash::FxHashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use petgraph::Direction;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::prelude::{EdgeRef, StableGraph};
use petgraph::stable_graph::DefaultIx;
use petgraph::visit::IntoEdges;
use regex::{Captures, Regex};
use scopeguard::{ScopeGuard, defer, guard};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::Write;
use std::ops::Deref;
use std::path::Path;
use std::sync::LazyLock;

// TODO: check for latest version in upstream bitbake
static VAR_EXPANSION_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());

static PYTHON_EXPANSION_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\$\{@.+?}").unwrap());

static WHITESPACE_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\s").unwrap());

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
    // TODO: edge type is Operator which is correct for Variable -> Statement edges
    //  but will not work when we start adding Variable -> Variable edges (for caching)
    ds: StableGraph<GraphItem, EdgeOperation>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    expand_state: RefCell<Option<ExpansionState>>,
    active_overrides: RefCell<Option<IndexSet<String>>>,
    inside_compute_overrides: RefCell<()>,

    statement_id: usize,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct EdgeOperation {
    pub(crate) op_type: Operator,
    pub(crate) sequence_id: usize,
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

static OVERRIDE_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^\w+$").unwrap());

impl DataSmart {
    pub fn new() -> DataSmart {
        DataSmart {
            ds: StableGraph::new(),
            vars: FxHashMap::default(),
            expand_state: RefCell::new(None),
            active_overrides: RefCell::new(None),
            inside_compute_overrides: RefCell::new(()),
            statement_id: 0,
        }
    }

    pub fn dump<P: AsRef<Path>>(&self, path: P) {
        let mut f = File::create(path).unwrap();
        let output = format!("{:?}", Dot::with_config(&self.ds, &[]));
        f.write_all(output.as_bytes()).unwrap();
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

    pub fn plus_equals_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
    ) {
        set_var_ex!(self, var, value, operator = NormalOperator::PlusEqual);
    }

    pub fn equals_plus_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
    ) {
        set_var_ex!(self, var, value, operator = NormalOperator::EqualPlus);
    }

    pub fn equals_dot_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
    ) {
        set_var_ex!(self, var, value, operator = NormalOperator::EqualDot);
    }

    pub fn dot_equals_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
    ) {
        set_var_ex!(self, var, value, operator = NormalOperator::DotEqual);
    }

    pub fn weak_default_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
    ) {
        set_var_ex!(self, var, value, operator = NormalOperator::WeakDefault);
    }

    pub fn default_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
    ) {
        set_var_ex!(self, var, value, operator = NormalOperator::Default);
    }

    #[tracing::instrument(skip(self), ret)]
    pub(crate) fn set_var_ex<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
        parsing: bool,
        normal_operator: NormalOperator,
    ) -> Option<NodeIndex<DefaultIx>> {
        let var = var.into();

        let stmt_node = parse_statement(&var, normal_operator, value.into())?;
        let base = stmt_node.lhs.var_base.clone();
        let lhs_cloned = stmt_node.lhs.clone();

        let resolved_op = stmt_node.resolved_operator();
        let stmt_idx = self.ds.add_node(GraphItem::StmtNode(stmt_node));

        // Lookup variable base (stem) and create if it doesn't exist
        let var_entry = self
            .vars
            .entry(base.to_string())
            .or_insert_with(|| self.ds.add_node(GraphItem::new_variable(base)));

        // // If not parsing, wipe away overrides
        // if !parsing {
        //     // TODO: not sure if it's this easy...
        //     var_data.operations.clear();
        // }

        // if !parsing {
        //     let existing_edges = self.ds.edges(*var_entry);
        //     let mut s = vec![];
        //
        //     for edge in existing_edges {
        //         s.push(edge.id());
        //         // TODO delete node too
        //     }
        //     dbg!(&s);
        //
        //     for e in s {
        //         self.ds.remove_edge(e);
        //     }
        // }

        if normal_operator == NormalOperator::Assign {
            let existing_edges = self.ds.edges(*var_entry);
            let mut s = vec![];

            for edge in existing_edges {
                if edge.weight().op_type == Operator::Normal(NormalOperator::Assign) {
                    let stmt = self.ds.node_weight(edge.target()).unwrap().statement();

                    if lhs_cloned == stmt.lhs {
                        s.push(edge.id());
                    }
                }
                // TODO delete node too
            }

            for e in s {
                self.ds.remove_edge(e);
            }
        }

        let _e = self.ds.add_edge(
            *var_entry,
            stmt_idx,
            EdgeOperation {
                op_type: resolved_op,
                sequence_id: self.statement_id,
            },
        );
        self.statement_id += 1;

        Some(*var_entry)
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn set_var<T: Into<String> + Debug, S: Into<String> + Debug>(
        &mut self,
        var: T,
        value: S,
        parsing: bool,
    ) -> Option<NodeIndex<DefaultIx>> {
        let var = var.into();
        let value = value.into();

        #[cfg(test)]
        if var == "OVERRIDES" && self.vars.contains_key("OVERRIDES") {
            unimplemented!(
                "OVERRIDES are already set! Re-computing OVERRIDES not implemented yet."
            );
        }

        self.set_var_ex(var, value, parsing, NormalOperator::Assign)
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

                    Ok(get_var!(self, referenced_var).unwrap_or(match_str.to_string()))
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
        let parsed = parse_variable(var);

        // In bitbake, delVar with an override operation doesn't work:
        //    d.delVar("TEST:append")
        // so calls like that have no effect.
        if matches!(parsed.kind, OverrideOperation { .. }) {
            return Ok(());
        }

        eprintln!("lookup: {:?}", &parsed);

        let Some(var_index) = self.vars.get(&parsed.var_base).copied() else {
            return Ok(());
        };

        let mut stmts = vec![];
        let mut walker = self
            .ds
            .neighbors_directed(var_index, Direction::Outgoing)
            .detach();

        let _deleted_all_stmts = false;
        while let Some(stmt_node_index) = walker.next_node(&self.ds) {
            let stmt = self.ds.node_weight(stmt_node_index).unwrap().statement();

            // As above, only consider normal assignments.
            let Assignment { scope } = &stmt.lhs.kind else {
                continue;
            };

            if scope.join(":") == parsed.override_string() {
                stmts.push(stmt_node_index);
                self.ds.remove_node(stmt_node_index);
            }

            // let var_node = self.ds.node_weight_mut(var_index).unwrap().variable_mut();
            // var_node.operations.retain(|op| {
            //     if stmts.contains(&op.idx) {
            //         eprintln!("delete {:?}", op.idx);
            //     }
            //     !stmts.contains(&op.idx)
            // });
            //
            // if var_node.operations.is_empty() {
            //     deleted_all_stmts = true;
            // }
        }

        Ok(())
    }

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

        let old_parsed = parse_variable(old);
        let new_parsed = parse_variable(new);

        let old_base = &old_parsed.var_base;
        let new_base = &new_parsed.var_base;

        let old_target_scope = old_parsed.override_scope();

        let old_var_index = match self.vars.get(old_base) {
            Some(&idx) => idx,
            None => return Ok(()),
        };

        // TODO: rewrite this

        // --- PHASE 1: Collect and Explicitly Isolate Edge IDs (Immutable Read) ---
        let mut edges_to_move = Vec::new();

        let mut walker = self
            .ds
            .neighbors_directed(old_var_index, Direction::Outgoing)
            .detach();
        while let Some((edge_idx, target_node_idx)) = walker.next(&self.ds) {
            if let Some(GraphItem::StmtNode(stmt)) = self.ds.node_weight(target_node_idx)
                && stmt.lhs.override_scope().starts_with(&old_target_scope)
            {
                let op_metadata = *self.ds.edge_weight(edge_idx).unwrap();
                edges_to_move.push((edge_idx, target_node_idx, op_metadata));
            }
        }

        if edges_to_move.is_empty() {
            return Ok(());
        }

        edges_to_move.sort_by_key(|o| o.2.sequence_id);

        // --- PHASE 2: Ensure Target Root Exists ---
        let new_var_index = match self.vars.get(new_base) {
            Some(&idx) => idx,
            None => {
                let new_node = self.ds.add_node(GraphItem::Variable(Variable {
                    name: new_base.clone(),
                    cached_value: RefCell::new(None),
                    varflags: BTreeMap::new(),
                }));
                self.vars.insert(new_base.clone(), new_node);
                new_node
            }
        };

        // --- PHASE 3: Mutate Statement Internals & Re-Shape ---
        let mut moving_a_base_assignment = false;

        for (_, stmt_idx, _) in &edges_to_move {
            if let Some(GraphItem::StmtNode(stmt)) = self.ds.node_weight_mut(*stmt_idx) {
                eprintln!("original statement {stmt:?}");

                // 1. Update the base variable base name natively
                stmt.lhs.var_base = new_base.clone();

                // 2. Adjust scopes dynamically without blindly discarding the layout 'kind'
                match &mut stmt.lhs.kind {
                    Assignment { scope } => {
                        let trailing_scope = scope.split_off(old_target_scope.len());

                        match &new_parsed.kind {
                            Assignment { scope: new_scope } => {
                                let mut updated_scope = new_scope.clone();
                                updated_scope.extend(trailing_scope);
                                *scope = updated_scope;

                                // If the resulting full expression maps to a plain base assignment (no overrides left),
                                // it means BitBake's setVar semantic will clobber any existing base value on the target node.
                                if scope.is_empty() {
                                    moving_a_base_assignment = true;
                                }
                            }
                            OverrideOperation {
                                scope: new_scope,
                                operator,
                                filter,
                            } => {
                                // If the expanded baseline introduces an operator, transmute the structure
                                let mut updated_scope = new_scope.clone();
                                updated_scope.extend(trailing_scope);
                                stmt.lhs.kind = OverrideOperation {
                                    scope: updated_scope,
                                    operator: *operator,
                                    filter: filter.clone(),
                                };
                            }
                        }
                    }
                    OverrideOperation { scope, .. } => {
                        // FIXES THE TODO: Keep the existing OverrideOperation layout intact!
                        let trailing_scope = scope.split_off(old_target_scope.len());

                        match &new_parsed.kind {
                            Assignment { scope: new_scope } => {
                                // Just prepend the new prefix scope fragments from new_parsed
                                let mut updated_scope = new_scope.clone();
                                updated_scope.extend(trailing_scope);
                                *scope = updated_scope;
                            }
                            OverrideOperation {
                                scope: new_scope, ..
                            } => {
                                let mut updated_scope = new_scope.clone();
                                updated_scope.extend(trailing_scope);
                                *scope = updated_scope;
                            }
                        }
                    }
                }
                eprintln!("mutated statement {:?}", stmt);
            }
        }

        // --- PHASE 3.5: Handle BitBake setVar Clobbering ---
        // If we are moving a pure base assignment into the destination variable node,
        // BitBake semantics dictate that the previous destination base assignments are discarded.
        if moving_a_base_assignment {
            let mut dest_walker = self
                .ds
                .neighbors_directed(new_var_index, Direction::Outgoing)
                .detach();

            let mut dest_edges_to_remove = Vec::new();
            while let Some((edge_idx, target_node_idx)) = dest_walker.next(&self.ds) {
                if let Some(GraphItem::StmtNode(stmt)) = self.ds.node_weight(target_node_idx)
                    && let Assignment { scope } = &stmt.lhs.kind
                    && scope.is_empty()
                {
                    dest_edges_to_remove.push((edge_idx, target_node_idx));
                }
            }

            for (edge_idx, stmt_idx) in dest_edges_to_remove {
                self.ds.remove_edge(edge_idx);
                self.ds.remove_node(stmt_idx);
            }
        }

        // --- PHASE 4: Graph Topology Updates (With Edge Type Synchronization) ---
        for (edge_idx, stmt_idx, mut op_metadata) in edges_to_move {
            self.ds.remove_edge(edge_idx);

            if let Some(GraphItem::StmtNode(stmt)) = self.ds.node_weight(stmt_idx) {
                op_metadata.op_type = match &stmt.lhs.kind {
                    Assignment { .. } => op_metadata.op_type,
                    OverrideOperation { operator, .. } => Operator::from(*operator),
                };
            }

            op_metadata.sequence_id = self.statement_id;
            self.statement_id += 1;

            self.ds.add_edge(new_var_index, stmt_idx, op_metadata);
        }

        // --- PHASE 5: Clean Up Residual Variable Nodes ---
        let remaining_edges = self
            .ds
            .neighbors_directed(old_var_index, petgraph::Direction::Outgoing)
            .count();
        if remaining_edges == 0 {
            self.vars.remove(old_base);
            self.ds.remove_node(old_var_index);
        }

        self.del_var(old)?;

        Ok(())
    }

    #[tracing::instrument(skip_all)]
    pub fn expand_keys(&mut self) -> DataSmartResult<Vec<String>> {
        let mut todolist = BTreeMap::new();
        for key in self.get_all_keys() {
            if !key.contains("${") {
                continue;
            }

            let expanded = self.expand(&key)?;
            if key == expanded {
                continue;
            }

            todolist.insert(key, expanded);
        }

        let ret = todolist.keys().cloned().sorted().collect_vec();
        for o in todolist.into_iter() {
            eprintln!("rename {} to {}", o.0, o.1);
            self.rename_var(o.0, o.1)?;
        }

        // Sanity check: did we actually expand everything?
        for stmt in self.ds.node_weights() {
            if let GraphItem::StmtNode(stmt) = stmt {
                let o = stmt.lhs.override_string();
                if !o.is_empty() {
                    assert!(!o.contains("${"));
                }
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
                let s = split_filter_empty(&get_var!(self, "OVERRIDES").unwrap_or_default(), ":")
                    .map(String::from)
                    .collect::<IndexSet<String>>();

                //eprintln!("{} set overides = {:?}", " ".repeat(level), s);
                *RefCell::borrow_mut(&self.active_overrides) = Some(s);

                let s2 = split_filter_empty(&get_var!(self, "OVERRIDES").unwrap_or_default(), ":")
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
        no_weak_default: bool,
    ) -> Option<String> {
        let parsed = parse_variable(var);

        // If an override-style operator is present, then it will never match so return None
        // TODO: what if someone adds one to OVERRIDES?
        if matches!(parsed.kind, OverrideOperation { .. }) {
            return None;
        }

        // Lookup the variable, otherwise return None
        let var_entry = self.vars.get(&parsed.var_base)?;

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
        let var_suffix = parsed.override_scope().to_vec();
        let override_selection_context: Cow<IndexSet<String>> = match var_suffix.is_empty() {
            false => {
                // TODO: revisit: are we sure the new overrides should be inserted into the beginning?
                let mut new_overrides = IndexSet::from_iter(var_suffix.clone());
                for old_override in override_state.iter() {
                    new_overrides.insert(old_override.clone());
                }

                Cow::Owned(new_overrides)
            }
            true => override_state.clone(),
        };

        let mut o: Vec<(usize, VariableOperation)> = vec![];
        let mut operations: FifoHeap<VariableOperation> = FifoHeap::new();

        for edge in self.ds.edges(*var_entry) {
            o.push((
                edge.weight().sequence_id,
                VariableOperation {
                    op_type: edge.weight().op_type,
                    idx: edge.target(),
                },
            ))
        }

        o.sort_by_key(|e| e.0);

        for op in o {
            operations.push(op.1);
        }

        // Calculate override scores for operations
        let mut resolved_variable_operations: FifoHeap<ScoredOperation> = operations
            .iter()
            .filter_map(|op| {
                let statement = self.ds.node_weight(op.idx).unwrap().statement();
                if !var_suffix.is_empty() {
                    let stmt_scope = statement.lhs.kind.override_scope();

                    // Use exact slice equality so "a:append" (scope=["a"]) matches your lookup "MY_VAR:a" (var_suffix=["a"])
                    if stmt_scope != var_suffix.as_slice() {
                        return None;
                    }
                }

                // If in parsing mode, filter out override operators
                if parsing && statement.is_override_operation() {
                    return None;
                }

                if !statement
                    .lhs
                    .kind
                    .is_active(&override_selection_context, &override_state)
                {
                    return None;
                }

                // TODO: this re-checks is active basically.
                let score = statement.lhs.kind.score(&override_selection_context)?;
                let ret = ScoredOperation {
                    stmt_index: op.idx,
                    score,
                    stmt: statement,
                };

                Some(ret)
            })
            .fold(FifoHeap::new(), |mut a, b| {
                a.push(b);
                a
            });

        dbg!(&resolved_variable_operations);

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

        let mut ret = match resolved_start_value.stmt.resolved_operator() {
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

        // TODO: filter in loop below?
        resolved_variable_operations.retain(|op| {
            // Remove the variable operation that we used for the start value, so we don't double apply
            op.stmt_index != resolved_start_value.stmt_index
                // Handle override scoring + LHS
                // TODO: clarify
                && (op.score >= resolved_start_value.score
                    || (op.stmt.is_override_operation()
                        && (op.stmt.lhs.kind.override_scope() == resolved_start_value.stmt.lhs.kind.override_scope()
                            || op.stmt.lhs.kind.override_scope().is_empty())))
        });

        eprintln!("start value for get {:?} = {:?} ", parsed, ret,);

        for op in resolved_variable_operations {
            // Weak default is handled the same as assign - priority selection happened above
            match op.stmt.resolved_operator() {
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
    //  can use IndexMap instead of FxHashMap
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
            for edge in self.ds.edges(*var.1) {
                let stmt_node = self.ds.node_weight(edge.target()).unwrap().statement();

                let scope = stmt_node.lhs.override_scope().to_vec();
                let mut parts = vec![var.0.clone()];
                parts.extend(scope);
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
        }

        ret.into_iter().sorted().collect_vec()
    }

    pub fn keys(&self) -> KeysIter {
        todo!();
    }
}
