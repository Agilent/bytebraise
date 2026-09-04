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
    - Runtime setVar semantics when parsing=false
    - Non-string data - this should print 3:
        python() {
            d.setVar("TEST2", 1)
            d.setVar("TEST2:append", 2)
            bb.fatal("TEST2 = " + str(d.getVar("TEST2")))
        }
    - Varflags
    - Variable history
*/

use crate::errors::{DataSmartError, DataSmartResult};
use crate::keys_iter::KeysIter;
use crate::macros::{get_var, set_var_ex};
use crate::nodes::{OperationScope, ResolvedOperation, Variable};
use crate::variable_operation::{NormalOperator, Operator, OverrideOperator};
use crate::variable_parser::VariableExpressionKind::{Assignment, OverrideOperation};
use crate::variable_parser::{parse_statement, parse_variable};
use anyhow::bail;
use bytebraise_util::fifo_heap::FifoHeap;
use bytebraise_util::split::{replace_all, split_filter_empty, split_keep};
use fxhash::FxHashMap;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::DefaultIx;
use regex::{Captures, Regex};
use scopeguard::{ScopeGuard, defer, guard};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::Write;
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
    ds: StableGraph<Variable, ()>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    expand_state: RefCell<Option<ExpansionState>>,
    active_overrides: RefCell<Option<IndexSet<String>>>,
    inside_compute_overrides: RefCell<()>,
}

// BitBake reduces all active override variants together. Distinct scopes can collapse to the same
// key, so insertion order and replacement behavior are part of selection semantics.
fn select_override_scope(variable: &Variable, active_overrides: &IndexSet<String>) -> Vec<String> {
    let mut active = IndexMap::<Vec<String>, Vec<String>>::new();

    for statement in &variable.statements {
        let scope = statement.lhs.override_scope();
        if !scope.is_empty()
            && scope
                .iter()
                .all(|override_name| active_overrides.contains(override_name))
        {
            active.insert(scope.to_vec(), scope.to_vec());
        }
    }

    let mut selected = Vec::new();
    let mut modified = true;
    while modified {
        modified = false;

        for active_override in active_overrides {
            let scopes = active.keys().cloned().collect_vec();
            for scope in scopes {
                if scope.len() > 1 && scope.last() == Some(active_override) {
                    let Some(original_scope) = active.shift_remove(&scope) else {
                        continue;
                    };
                    let reduced_scope = scope
                        .iter()
                        .enumerate()
                        .filter(|(index, part)| *index == 0 || *part != active_override)
                        .map(|(_, part)| part)
                        .cloned()
                        .collect();
                    // IndexMap, like Python's dict, replaces an existing value without moving its
                    // key. A newly reduced key is appended instead.
                    active.insert(reduced_scope, original_scope);
                    modified = true;
                } else if scope.as_slice() == std::slice::from_ref(active_override)
                    && let Some(original_scope) = active.shift_remove(&scope)
                {
                    selected = original_scope;
                }
            }
        }
    }

    selected
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

        // Lookup variable base (stem) and create if it doesn't exist
        let var_entry = self
            .vars
            .entry(base.to_string())
            .or_insert_with(|| self.ds.add_node(Variable::new(base)));

        if normal_operator == NormalOperator::Assign {
            self.ds
                .node_weight_mut(*var_entry)
                .unwrap()
                .statements
                .retain(|stmt| {
                    stmt.resolved_operator() != Operator::Normal(NormalOperator::Assign)
                        || lhs_cloned != stmt.lhs
                });
        }

        self.ds
            .node_weight_mut(*var_entry)
            .unwrap()
            .statements
            .push(stmt_node);

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

        self.ds
            .node_weight_mut(var_index)
            .unwrap()
            .statements
            .retain(|stmt| {
                let Assignment { scope } = &stmt.lhs.kind else {
                    return true;
                };
                scope.join(":") != parsed.override_string()
            });

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

        // Override operations belong to their base variable; they are not standalone keys.
        if matches!(old_parsed.kind, OverrideOperation { .. }) {
            return Ok(());
        }

        let old_base = &old_parsed.var_base;
        let new_base = &new_parsed.var_base;

        let old_var_index = match self.vars.get(old_base) {
            Some(&idx) => idx,
            None => return Ok(()),
        };

        let old_statements =
            std::mem::take(&mut self.ds.node_weight_mut(old_var_index).unwrap().statements);
        let mut remaining_statements = Vec::with_capacity(old_statements.len());
        let mut statements_to_move = Vec::new();

        for mut stmt in old_statements {
            if let Some(new_lhs) = stmt.lhs.replace_assignment_prefix(&old_parsed, &new_parsed) {
                stmt.lhs = new_lhs;
                statements_to_move.push(stmt);
            } else {
                remaining_statements.push(stmt);
            }
        }

        if statements_to_move.is_empty() {
            self.ds.node_weight_mut(old_var_index).unwrap().statements = remaining_statements;
            return Ok(());
        }

        let moving_a_base_assignment = statements_to_move
            .iter()
            .any(|stmt| matches!(&stmt.lhs.kind, Assignment { scope } if scope.is_empty()));
        let old_is_empty = remaining_statements.is_empty();
        self.ds.node_weight_mut(old_var_index).unwrap().statements = remaining_statements;

        let new_var_index = match self.vars.get(new_base) {
            Some(&idx) => idx,
            None => {
                let new_node = self.ds.add_node(Variable::new(new_base.clone()));
                self.vars.insert(new_base.clone(), new_node);
                new_node
            }
        };
        let destination = self.ds.node_weight_mut(new_var_index).unwrap();
        if moving_a_base_assignment {
            destination
                .statements
                .retain(|stmt| !matches!(&stmt.lhs.kind, Assignment { scope } if scope.is_empty()));
        }
        destination.statements.extend(statements_to_move);

        if old_var_index != new_var_index && old_is_empty {
            self.vars.remove(old_base);
            self.ds.remove_node(old_var_index);
        }

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
        for var in self.ds.node_weights() {
            for stmt in &var.statements {
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

        let variable = self.ds.node_weight(*var_entry).unwrap();
        let selected_scope = if var_suffix.is_empty() {
            select_override_scope(variable, &override_selection_context)
        } else {
            var_suffix.clone()
        };

        // The winning scope is selected globally above. Scores only order the operations that
        // belong to that scope and the unqualified deferred operations that also apply to it.
        let mut resolved_variable_operations: FifoHeap<ResolvedOperation> = variable
            .statements
            .iter()
            .enumerate()
            .filter_map(|(stmt_index, statement)| {
                let stmt_scope = statement.lhs.kind.override_scope();
                if stmt_scope != selected_scope.as_slice()
                    && !(var_suffix.is_empty()
                        && statement.is_override_operation()
                        && stmt_scope.is_empty())
                {
                    return None;
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

                let scope = if stmt_scope == selected_scope.as_slice() {
                    OperationScope::Selected
                } else {
                    OperationScope::Unqualified
                };
                let ret = ResolvedOperation {
                    stmt_index,
                    scope,
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

        for (name, var_index) in &self.vars {
            for stmt in &self.ds.node_weight(*var_index).unwrap().statements {
                let scope = stmt.lhs.override_scope().to_vec();
                let mut parts = vec![name.clone()];
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
