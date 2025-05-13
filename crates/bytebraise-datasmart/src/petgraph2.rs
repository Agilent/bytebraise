use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};

use fxhash::FxHashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use once_cell::sync::Lazy;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::DefaultIx;
use regex::{Captures, Regex};
use scopeguard::{ScopeGuard, defer, guard};

use crate::errors::{DataSmartError, DataSmartResult};
use crate::variable_operation::{StmtKind, VariableOperation, VariableOperationKind};
use bytebraise_util::fifo_heap::FifoHeap;
use bytebraise_util::retain_with_index::RetainWithIndex;
use bytebraise_util::split::{replace_all, split_filter_empty, split_keep};

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
struct DataSmart {
    ds: StableGraph<GraphItem, ()>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    unexpanded_vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    expand_state: RefCell<Option<ExpansionState>>,
    active_overrides: RefCell<Option<IndexSet<String>>>,
    inside_compute_overrides: RefCell<()>,
}

// TODO: need to support more than 64 overrides?
fn score_override(
    active_overrides: &Option<IndexSet<String>>,
    candidate_overrides: &Vec<String>,
) -> Option<(Vec<usize>, usize, usize)> {
    eprintln!("scoring {candidate_overrides:?}");
    // Reject this override if it contains terms not in active override set
    // TODO: change this to not clone
    let temp_cloned_active_overrides = active_overrides.clone().unwrap_or_default();

    let c: IndexSet<String> = candidate_overrides.iter().cloned().collect();
    if !c.is_subset(&temp_cloned_active_overrides) {
        return None;
    }

    let mut ret = (vec![], 0, 0);
    if candidate_overrides.is_empty() {
        return Some(ret);
    }

    if let Some(active_overrides) = active_overrides {
        ret = (
            std::iter::repeat_n(0, active_overrides.len()).collect(),
            0,
            0,
        );

        let mut candidate = candidate_overrides.clone();

        let counts = candidate_overrides.iter().counts();
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
            eprintln!("iteration {}", ret.1);
            ret.1 += 1;
            for (ai, active_override) in active_overrides.iter().enumerate() {
                eprintln!("\tconsider override {active_override}");
                if candidate.len() == 1 && &candidate[0] == active_override {
                    assert_eq!(ret.2, 0);
                    ret.2 = ai + 1;
                    break 'outer;
                } else if candidate.len() > 1 && candidate.ends_with(&[active_override.clone()]) {
                    let old = candidate.clone();
                    //eprintln!("{:?}", candidate);
                    candidate.retain_with_index(|c, i| i == 0 || c != active_override);

                    eprintln!("\t\ttransform {old:?} => {candidate:?}");

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

#[derive(Debug, Clone, Eq, PartialEq)]
enum ResolvedOverridesData {
    Operation {
        lhs: Vec<String>,
        rhs: IndexSet<String>,
        score: (Vec<usize>, usize, usize),
    },
    PureOverride {
        overrides: Vec<String>,
        score: (Vec<usize>, usize, usize),
    },
}

impl ResolvedOverridesData {
    fn override_filter(&self) -> IndexSet<String> {
        match self {
            ResolvedOverridesData::Operation { lhs, .. } => lhs.iter().cloned().collect(),
            ResolvedOverridesData::PureOverride { overrides, .. } => {
                overrides.iter().cloned().collect()
            }
        }
    }

    fn is_valid_for_filter(&self, override_filter: &IndexSet<String>) -> bool {
        match self {
            ResolvedOverridesData::Operation { lhs, .. } => {
                let lhs: IndexSet<String> = lhs.iter().cloned().collect();
                lhs.is_empty() || lhs == *override_filter
            }
            ResolvedOverridesData::PureOverride { overrides, .. } => {
                let overrides: IndexSet<String> = overrides.iter().cloned().collect();
                eprintln!(
                    "checking {:?} == {:?} ({})",
                    overrides,
                    override_filter,
                    overrides == *override_filter
                );
                overrides.is_empty() || overrides == *override_filter
            }
        }
    }

    fn is_active(&self, active_overrides: &Option<IndexSet<String>>) -> bool {
        active_overrides
            .as_ref()
            .is_some_and(|active_overrides| match self {
                ResolvedOverridesData::Operation { rhs, .. } => rhs.is_subset(active_overrides),
                ResolvedOverridesData::PureOverride { overrides, .. } => {
                    let overrides: IndexSet<String> = overrides.iter().cloned().collect();
                    overrides.is_subset(active_overrides)
                }
            })
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct ResolvedVariableOperation {
    overrides_data: Option<ResolvedOverridesData>,
    unexpanded_override: String,
    op_type: VariableOperationKind,
    value: String,
    stmt_index: NodeIndex,
}

impl ResolvedVariableOperation {
    fn override_score(&self) -> (Vec<usize>, usize, usize) {
        match &self.overrides_data {
            // TODO: revisit None?
            None => (vec![], 0, 0),
            Some(ResolvedOverridesData::Operation { score, .. }) => score.clone(),
            Some(ResolvedOverridesData::PureOverride { score, .. }) => score.clone(),
        }
    }

    fn is_override_operation(&self) -> bool {
        match self.overrides_data {
            None => false,
            Some(ResolvedOverridesData::PureOverride { .. }) => false,
            Some(ResolvedOverridesData::Operation { .. }) => true,
        }
    }

    fn is_synthesized_operation(&self) -> bool {
        match self.op_type {
            VariableOperationKind::SynthesizedAppend
            | VariableOperationKind::SynthesizedPrepend => true,
            _ => false,
        }
    }

    fn override_lhs(&self) -> Vec<String> {
        match &self.overrides_data {
            None => vec![],
            Some(ResolvedOverridesData::PureOverride { overrides, .. }) => overrides.clone(),
            Some(ResolvedOverridesData::Operation { lhs, .. }) => lhs.clone(),
        }
    }
}

impl Ord for ResolvedVariableOperation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.override_score()
            .cmp(&other.override_score())
            .reverse()
            .then(self.op_type.cmp(&other.op_type))
    }
}

impl PartialOrd for ResolvedVariableOperation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl DataSmart {
    pub fn new() -> DataSmart {
        DataSmart {
            ds: StableGraph::new(),
            vars: FxHashMap::default(),
            unexpanded_vars: FxHashMap::default(),
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
        self.set_var_ex(var, value, StmtKind::PlusEqual, true);
    }

    pub fn equals_plus_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, StmtKind::EqualPlus, true);
    }

    pub fn equals_dot_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, StmtKind::EqualDot, true);
    }

    pub fn dot_equals_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, StmtKind::DotEqual, true);
    }

    pub fn weak_default_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, StmtKind::WeakDefault, true);
    }

    pub fn default_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        self.set_var_ex(var, value, StmtKind::Default, true);
    }

    fn set_var_ex<T: Into<String>, S: Into<String>>(
        &mut self,
        var: T,
        value: S,
        stmt_kind: StmtKind,
        check_keyword: bool,
    ) {
        let var = var.into();
        let value = value.into();

        let var_parts = var.split_once(':');
        let base = var_parts.map_or(var.as_str(), |parts| parts.0);
        let override_str = var_parts.map(|parts| parts.1);

        if check_keyword {
            let keyword_match = override_str.and_then(|s| KEYWORD_REGEX.captures(s));

            if keyword_match.and_then(|m| m.name("keyword").map(|k| k.as_str())).is_some() {
                unimplemented!()
            };
        }

        let stmt_idx = self.ds.add_node(GraphItem::StmtNode(StmtNode {
            lhs: override_str.map(String::from),
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

        if base.contains("${") {
            self.unexpanded_vars.insert(base.to_string(), *var_entry);
        }

        self.ds.add_edge(*var_entry, stmt_idx, ());
    }

    pub fn set_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        let var = var.into();
        let value = value.into();

        let var_parts = var.split_once(':');
        let override_str = var_parts.map(|parts| parts.1);

        let keyword_match = override_str.and_then(|s| KEYWORD_REGEX.captures(s));

        let stmt_kind = match keyword_match.and_then(|m| m.name("keyword").map(|k| k.as_str())) {
            None => StmtKind::Assign,
            Some("append") => StmtKind::Append,
            Some("prepend") => StmtKind::Prepend,
            Some("remove") => StmtKind::Remove,
            Some(_) => unreachable!(),
        };

        self.set_var_ex(var, value, stmt_kind, false);
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
                        .get_var(referenced_var)
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
        if let Some(idx) = self.vars.remove(var.as_ref()) {
            self.ds.remove_node(idx).unwrap();
        }

        Ok(())
    }

    pub fn expand_keys(&mut self) -> DataSmartResult<()> {
        for unexpanded_key in &self.unexpanded_vars {
            let expanded_key = self.expand(unexpanded_key.0)?;

            if let Some(data) = self.ds.node_weight_mut(*unexpanded_key.1) {
                // TODO: correct warning message to include values
                eprintln!(
                    "WARNING: Variable key {} replaces original key {}",
                    unexpanded_key.0, expanded_key
                );

                self.vars.remove(unexpanded_key.0);

                eprintln!("new name: {expanded_key}");
                data.variable_mut().name = expanded_key.clone();
                self.vars.insert(expanded_key, *unexpanded_key.1);
            }
        }

        Ok(())
    }

    fn compute_overrides(&self) -> DataSmartResult<()> {
        if let Ok(_) = RefCell::try_borrow_mut(&self.inside_compute_overrides) {
            if RefCell::borrow(&self.active_overrides).is_some() {
                return Ok(());
            }

            for i in 0..5 {
                //eprintln!("{}+ override iteration {}", " ".repeat(level), i);
                let s = split_filter_empty(&self.get_var("OVERRIDES").unwrap_or_default(), ":")
                    .map(String::from)
                    .collect::<IndexSet<String>>();

                //eprintln!("{} set overides = {:?}", " ".repeat(level), s);
                *RefCell::borrow_mut(&self.active_overrides) = Some(s);

                let s2 = split_filter_empty(&self.get_var("OVERRIDES").unwrap_or_default(), ":")
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

    pub fn get_var<S: AsRef<str>>(&self, var: S) -> Option<String> {
        let var = var.as_ref();

        let var_parts = var.split_once(':');

        // TODO: handle override syntax, e.g. getVar("A:pn-waves")
        //  All this should have to do (TM) is apply a pre-filter to the variable operations.
        let var_entry = self.vars.get(var)?;
        //println!("{}get_var = {}", " ".repeat(level), var);

        let w = self.ds.node_weight(*var_entry).unwrap();
        let var_data = w.variable();

        // TODO: return directly from `override_state` for OVERRIDES?
        if var != "OVERRIDES" {
            let cached = RefCell::borrow_mut(&var_data.cached_value);
            if cached.is_some() {
                return cached.clone();
            }
        }

        // TODO: only do this if needed, i.e. if any operations with overrides are present
        self.compute_overrides().unwrap();

        let override_state = &*RefCell::borrow(&self.active_overrides);

        let mut resolved_variable_operations: FifoHeap<ResolvedVariableOperation> = var_data
            .operations
            .heap
            .iter()
            .filter_map(|op| {
                let statement = self.ds.node_weight(op.0.idx).unwrap().statement();
                let original_override = statement.lhs.clone().unwrap_or_default();
                let expanded_lhs = statement
                    .lhs
                    .as_ref()
                    .map(|s| self.expand(s))
                    .transpose()
                    .unwrap();

                let mut var_op_kind: VariableOperationKind = op.0.op_type.into();

                let mut resolved_od = None;

                if let Some(expanded_lhs) = expanded_lhs {
                    let mut locs = KEYWORD_REGEX.capture_locations();

                    if let Some(keyword_match) =
                        KEYWORD_REGEX.captures_read(&mut locs, &expanded_lhs)
                    {
                        let c = locs.get(1).unwrap();

                        match &expanded_lhs[c.0..c.1] {
                            "append" => {
                                if var_op_kind != VariableOperationKind::Append {
                                    var_op_kind = VariableOperationKind::SynthesizedAppend;
                                }
                            }
                            "prepend" => {
                                if var_op_kind != VariableOperationKind::Prepend {
                                    var_op_kind = VariableOperationKind::SynthesizedPrepend;
                                }
                            }
                            "remove" => {
                                var_op_kind = VariableOperationKind::Remove;
                            }
                            _ => unreachable!("{}", expanded_lhs),
                        };

                        let override_lhs = split_overrides(&expanded_lhs[0..c.0]);
                        let override_rhs = &expanded_lhs[c.1..];
                        assert!(!override_lhs.contains(&String::from("remove")));

                        let Some(override_score) = score_override(override_state, &override_lhs)
                        else {
                            // Reject operations if the override is not active
                            return None;
                        };

                        resolved_od = Some(ResolvedOverridesData::Operation {
                            lhs: override_lhs,
                            rhs: split_overrides(override_rhs).iter().cloned().collect(),
                            score: override_score,
                        });
                    } else {
                        let overrides = split_overrides(expanded_lhs);
                        assert!(!overrides.contains(&String::from("remove")));
                        let Some(override_score) = score_override(override_state, &overrides)
                        else {
                            // Reject operations if the override is not active
                            return None;
                        };

                        resolved_od = Some(ResolvedOverridesData::PureOverride {
                            overrides,
                            score: override_score,
                        })
                    }
                }

                let ret = ResolvedVariableOperation {
                    value: statement.rhs.clone(),
                    unexpanded_override: original_override,
                    stmt_index: op.0.idx,
                    overrides_data: resolved_od,
                    op_type: var_op_kind,
                };

                Some(ret)
                // TODO: place expanded LHS in the assignment cache?
            })
            // TODO: something more efficient than a fold?
            .fold(FifoHeap::new(), |mut a, b| {
                a.heap.retain(|item| {
                    !item.0.is_synthesized_operation()
                        || (item.0.unexpanded_override != b.unexpanded_override)
                });
                a.push(b);
                a
            });

        eprintln!("{resolved_variable_operations:#?}");

        eprintln!("SCORING: ");
        for item in resolved_variable_operations.heap.iter() {
            eprintln!(
                "{}={} => {:?}",
                item.0.unexpanded_override,
                item.0.value,
                item.0.override_score()
            );
        }
        eprintln!("=====");

        let Some((resolved_start_value, _)) = resolved_variable_operations.heap.first().cloned()
        else {
            return None;
        };

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

        let mut ret = match resolved_start_value.op_type {
            VariableOperationKind::WeakDefault => {
                RetValue::WeakDefault(resolved_start_value.value.clone())
            }
            _ => RetValue::Eager(match resolved_start_value.op_type {
                VariableOperationKind::PlusEqual => format!(" {}", resolved_start_value.value),
                VariableOperationKind::EqualPlus => format!("{} ", resolved_start_value.value),
                _ => resolved_start_value.value.clone(),
            }),
        };

        eprintln!(
            "start value = {:?} @ score: {:?} with LHS: {:?}",
            ret,
            resolved_start_value.override_score(),
            resolved_start_value.override_lhs()
        );
        resolved_variable_operations
            .heap
            .retain(|(op, _)| op.stmt_index != resolved_start_value.stmt_index);
        eprintln!("before filter: {resolved_variable_operations:#?}");

        // TODO: just filter in loop below
        resolved_variable_operations.heap.retain(|(op, _)| {
            op.override_score() >= resolved_start_value.override_score()
                || (op.is_override_operation()
                    && (op.override_lhs() == resolved_start_value.override_lhs()
                        || op.override_lhs().is_empty()))
        });

        eprintln!("{resolved_variable_operations:#?}");

        let rhs_filter: IndexSet<String> = resolved_start_value
            .overrides_data
            .as_ref()
            .map(|od| od.override_filter())
            .unwrap_or_default();

        for (op, _) in &resolved_variable_operations.heap {
            if !op.overrides_data.as_ref().is_none_or(|od| {
                od.is_active(override_state) // && od.is_valid_for_filter(&rhs_filter)
            }) {
                continue;
            }
            match op.op_type {
                // Weak default is handled the same as assign - priority selection happened above
                VariableOperationKind::Assign => {
                    ret = RetValue::Eager(op.value.clone());
                }
                VariableOperationKind::WeakDefault => {
                    if !matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::WeakDefault(op.value.clone());
                    }
                }
                VariableOperationKind::Remove => {
                    // TODO: aggregate all removes and do it in one shot?
                    let mut removes: HashSet<String> = HashSet::new();
                    removes.insert(op.value.clone());
                    let new_ret = self.apply_removes(&ret.into(), &removes);
                    ret = RetValue::Eager(new_ret);
                }
                VariableOperationKind::Append | VariableOperationKind::SynthesizedAppend => {
                    ret = RetValue::Eager(ret.to_string() + &op.value);
                }
                VariableOperationKind::DotEqual => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(ret.to_string() + &op.value);
                    }
                }
                VariableOperationKind::Prepend | VariableOperationKind::SynthesizedPrepend => {
                    ret = RetValue::Eager(format!("{}{}", op.value, ret.as_ref()));
                }
                VariableOperationKind::EqualDot => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(format!("{}{}", op.value, ret.as_ref()));
                    }
                }
                VariableOperationKind::PlusEqual => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(format!("{} {}", ret.as_ref(), op.value));
                    }
                }
                VariableOperationKind::EqualPlus => {
                    if matches!(ret, RetValue::Eager(_)) {
                        ret = RetValue::Eager(format!("{} {}", op.value, ret.as_ref()));
                    }
                }
                VariableOperationKind::Default => {
                    if matches!(ret, RetValue::WeakDefault(_)) {
                        ret = RetValue::Default(op.value.clone())
                    }
                }
            }
        }

        let ret_str = self.expand(ret.as_ref()).unwrap();
        //*cached = ret.clone();

        Some(ret_str)
    }
}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>,
    cached_value: RefCell<Option<String>>,
    // TODO: iterative cache for OVERRIDES
}

#[derive(Debug)]
struct StmtNode {
    kind: StmtKind,

    /// The override(s) to the left of
    lhs: Option<String>,

    /// The value
    rhs: String,
}

#[derive(Debug)]
struct UnexpandedStatementNode {}

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

    fn statement(&self) -> &StmtNode {
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
        })
    }
}

#[cfg(test)]
mod test {
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
    fn none() {
        let d = DataSmart::new();
        assert_eq!(d.get_var("NOT_EXIST"), None);
    }

    #[test]
    fn multiple_append() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:append", "2");
        d.set_var("TEST:append", "2");

        assert_eq!(d.get_var("TEST"), Some("122".into()));
    }

    #[test]
    fn override_score() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");

        assert_eq!(d.get_var("TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("OVERRIDES", "more");

        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn override_score_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("OVERRIDES", "more:specific");

        assert_eq!(d.get_var("TEST"), Some("3".into()));
    }

    #[test]
    fn override_score_4() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("TEST:more:specific", "4");
        d.set_var("OVERRIDES", "more:specific");

        assert_eq!(d.get_var("TEST"), Some("4".into()));
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

        assert_eq!(d.get_var("TEST"), Some("6".into()));
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

        assert_eq!(d.get_var("TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_7() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:append", "2");

        assert_eq!(d.get_var("TEST"), Some("12".into()));
    }

    #[test]
    fn override_score_8() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");

        assert_eq!(d.get_var("TEST"), Some("23".into()));
    }

    #[test]
    fn override_score_9() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");

        assert_eq!(d.get_var("TEST"), Some("234".into()));
    }

    #[test]
    fn override_score_10() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:append:a", "NO");

        assert_eq!(d.get_var("TEST"), Some("234".into()));
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

        assert_eq!(d.get_var("TEST"), Some("BASE34".into()));
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

        assert_eq!(d.get_var("TEST"), Some("OH YESBASE34".into()));
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

        assert_eq!(d.get_var("TEST"), Some("WHAT34".into()));
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

        assert_eq!(d.get_var("TEST"), Some("WHAT!34".into()));
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

        assert_eq!(d.get_var("TEST"), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_score_16() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "10");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");

        assert_eq!(d.get_var("TEST"), Some("1034".into()));
    }

    #[test]
    fn override_score_17() {
        let mut d = DataSmart::new();
        d.set_var("TEST:append", "why?");
        d.set_var("TEST:a:b:append", "first");
        d.set_var("TEST:a:b:${OP}", "OP");
        d.set_var("OP", "append");
        d.set_var("OVERRIDES", "a:b");

        assert_eq!(d.get_var("TEST"), Some("firstOPwhy?".into()));
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

        assert_eq!(d.get_var("TEST"), Some("QWHAT!34".into()));
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

        assert_eq!(d.get_var("TEST"), Some("5".into()));
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

        assert_eq!(d.get_var("TEST"), Some("5".into()));
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

        assert_eq!(d.get_var("TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:b:a:append", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:b:a:append", "2");
        d.set_var("TEST:a:b:append", "3");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:append", "3");
        d.set_var("TEST:b:a:append", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("3".into()));
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

        assert_eq!(d.get_var("TEST"), Some("4".into()));
    }

    #[test]
    fn tricky_1() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:c:b", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:c:b:a:b:c", "2");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("2".into()));

        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("TEST:c:b:a:b:c", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("TEST:a:b:c:a", "4");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("4".into()));

        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:a:b:c:a", "4");
        d.set_var("TEST:a:b:c", "3");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("4".into()));
    }

    #[test]
    fn filter_order_sensitivity() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:append:b:a", "2");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("12".into()));
    }

    #[test]
    fn order_of_operations() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");
        d.set_var("TEST:${B}", "2");
        d.set_var("TEST:append", " 4");
        d.set_var("B", "remove");

        assert_eq!(d.get_var("TEST"), Some("1  3 4".into()));
    }

    #[test]
    fn indirection_1() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");
        d.set_var("TEST:${${B}}", "2");
        d.set_var("B", "${W}");
        d.set_var("W", "Q");
        d.set_var("Q", "remove");

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:remove");
        assert_eq!(d.get_var("TEST"), Some("1  3".into()));
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

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:append");
        assert_eq!(d.get_var("TEST"), Some("1 2 3 5  4 ".into()));
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

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:append");
        assert_eq!(d.get_var("TEST"), Some("OK 5  4 ".into()));
    }

    #[test]
    fn synthesized_appends() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "10");
        d.set_var("TEST:${A}", "1");
        d.set_var("TEST:${A}", "2");
        d.set_var("A", "append");
        assert_eq!(d.get_var("TEST"), Some("102".into()));
    }

    #[test]
    fn plus_equals() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.plus_equals_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("base 2".into()));
    }

    #[test]
    fn plus_equals_no_base() {
        let mut d = DataSmart::new();
        d.plus_equals_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some(" 2".into()));
    }

    #[test]
    fn dot_equals() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.dot_equals_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("base2".into()));
    }

    #[test]
    fn dot_equals_no_base() {
        let mut d = DataSmart::new();
        d.dot_equals_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn equals_plus() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.equals_plus_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("2 base".into()));
    }

    #[test]
    fn equals_plus_no_base() {
        let mut d = DataSmart::new();
        d.equals_plus_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("2 ".into()));
    }

    #[test]
    fn equals_dot() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "base");
        d.equals_dot_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("2base".into()));
    }

    #[test]
    fn equals_dot_no_base() {
        let mut d = DataSmart::new();
        d.dot_equals_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default_2() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        d.weak_default_var("TEST", "3");
        d.weak_default_var("TEST", "4");
        assert_eq!(d.get_var("TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_doc_example() {
        let mut d = DataSmart::new();
        d.weak_default_var("W", "x");
        d.plus_equals_var("W", "y");
        assert_eq!(d.get_var("W"), Some(" y".into()));

        let mut d = DataSmart::new();
        d.weak_default_var("W", "x");
        d.set_var("W:append", "y");
        assert_eq!(d.get_var("W"), Some("xy".into()));
    }

    #[test]
    fn weak_default_priority() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        d.weak_default_var("TEST:a", "3");
        d.weak_default_var("TEST:a:b", "4");
        d.weak_default_var("TEST:b", "5");

        d.set_var("OVERRIDES", "a:b");
        assert_eq!(d.get_var("TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_append() {
        let mut d = DataSmart::new();

        d.set_var("TEST", "");
        d.set_var("TEST:append", "wat");
        d.weak_default_var("TEST:a", "OK");
        d.set_var("OVERRIDES", "a:b");

        assert_eq!(d.get_var("TEST"), Some("OKwat".into()));
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

        assert_eq!(d.get_var("TEST"), Some(" 53".into()));
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

        assert_eq!(d.get_var("TEST"), Some(" 5377".into()));
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

        assert_eq!(d.get_var("TEST"), Some(" 537".into()));
    }

    #[test]
    fn default_var() {
        let mut d = DataSmart::new();

        d.default_var("TEST", "1");

        assert_eq!(d.get_var("TEST"), Some("1".into()));

        let mut d = DataSmart::new();

        d.default_var("TEST", "1");
        d.default_var("TEST", "2");

        assert_eq!(d.get_var("TEST"), Some("1".into()));
    }

    #[test]
    fn default_precedence() {
        let mut d = DataSmart::new();

        d.weak_default_var("TEST", "2");
        d.default_var("TEST", "1");

        assert_eq!(d.get_var("TEST"), Some("1".into()));
    }

    #[test]
    fn weak_default_precedence() {
        let mut d = DataSmart::new();

        d.weak_default_var("TEST:a", "2");
        d.default_var("TEST", "1");
        d.set_var("OVERRIDES", "a:b:c");

        assert_eq!(d.get_var("TEST"), Some("2".into()));
    }

    #[test]
    fn finalization() {
        let mut d = DataSmart::new();

        d.set_var("A${B}", "X");
        d.set_var("B", "2");
        d.set_var("A2", "Y");

        d.expand_keys().unwrap();
        assert_eq!(d.get_var("A2"), Some("X".into()));
    }

    #[test]
    fn key_expansion() {
        let mut d = DataSmart::new();

        d.set_var("TEST${A}", "1");
        assert_eq!(d.get_var("TEST${A}"), Some("1".into()));

        d.set_var("TEST2", "2");
        d.set_var("A", "2");

        d.expand_keys().unwrap();

        assert_eq!(d.get_var("TEST2"), Some("1".into()));
    }

    #[test]
    fn test_wat() {
        let mut d = DataSmart::new();
        d.set_var("P", "");
        d.set_var("P:a", "append");
        d.set_var("Q", "base ");
        d.set_var("Q:${P}", "OK2");
        d.set_var("Q:append", "me first");
        d.set_var("OVERRIDES", "a");

        assert_eq!(d.get_var("Q").unwrap(), "base me firstOK2");
    }
}

fn main() {
    let mut d = DataSmart::new();

    // ABIEXTENSION ??= ""
    // ABIEXTENSION_class-nativesdk = ""
    // CLASSOVERRIDE ?= "class-target"
    // TARGET_OS = "linux${LIBCEXTENSION}${ABIEXTENSION}"
    // OVERRIDES = "${TARGET_OS}:${TRANSLATED_TARGET_ARCH}:pn-${PN}:layer-${FILE_LAYERNAME}:${MACHINEOVERRIDES}:${DISTROOVERRIDES}:${CLASSOVERRIDE}${LIBCOVERRIDE}:forcevariable"

    /*d.set_var("ABIEXTENSION", "");
        d.set_var("ABIEXTENSION:class-nativesdk", "wat");
        d.set_var("CLASSOVERRIDE", "class-nativesdk");
        d.set_var("TARGET_OS", "linux${LIBCEXTENSION}${ABIEXTENSION}");
        d.set_var("LIBCEXTENSION", "");
        d.set_var("LIBCOVERRIDE", "");
        //d.set_var("TRANSLATED_TARGET_ARCH", "wat");
        //d.set_var("PN", "waves");
        //d.set_var("B", "pn-waves");
        d.set_var("OVERRIDES", "${TARGET_OS}:${TRANSLATED_TARGET_ARCH}:pn-${PN}:layer-${FILE_LAYERNAME}:${MACHINEOVERRIDES}:${DISTROOVERRIDES}:${CLASSOVERRIDE}${LIBCOVERRIDE}:forcevariable");
    */

    // d.set_var("TEST", "a b c");
    // d.set_var("TEST:${A}", "b");
    // d.set_var("TEST:${P}", "b");
    // d.set_var("P", "append");
    // d.set_var("A", "append");
    // d.set_var("TEST:append", "OK");
    // d.set_var("TEST:append", "OK");
    // d.set_var("TEST:prepend", "prep");
    // d.set_var("TEST:${B}", "crazy");
    // d.set_var("B", "prepend");

    //d.set_var("TEST:q", "WAT");
    //d.set_var("OVERRIDES", "q");

    // d.set_var("TEST:${${Q}}", "indirect");
    // d.set_var("W", "P");
    // d.set_var("Q", "${W}");

    d.set_var("TEST", "a b c");
    d.set_var("TEST:q:b", "WAT");
    d.set_var("TEST:q:b:c:append", "p!");
    d.set_var("TEST:q:b:c:append", "q");
    d.set_var("OVERRIDES", "q:b:c");

    //println!(">>>> {:?}", d.expand("TEST:${${Q}}", 1));

    //d.set_var("A:remove:C", "C");
    //d.set_var("A:${B}", "D");

    //parse_value("${${M}}");

    // println!("\n");
    // //println!("\nOVERRIDES = {:?}\n", d.get_var("OVERRIDES"));
    println!("TEST = {:?}\n", d.get_var("TEST"));
    //
    // println!("{:?}", Dot::with_config(&d.ds, &[]));
}
