use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use fxhash::FxHashMap;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use once_cell::sync::Lazy;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::DefaultIx;
use regex::{Captures, Regex};
use scopeguard::{defer, guard, ScopeGuard};

use bytebraise::data_smart::errors::{DataSmartError, DataSmartResult};
use bytebraise::data_smart::utils::{replace_all, split_filter_empty, split_keep};

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());

static PYTHON_EXPANSION_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\{@.+?}").unwrap());

static KEYWORD_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?:^|:)(?P<keyword>append|prepend|remove)(?:$|:)").unwrap());

static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\s").unwrap());

#[derive(Clone, Debug)]
pub struct FifoHeap<T> {
    seq: usize,
    heap: BTreeSet<(T, usize)>,
}

// TODO: An 'assign' may actually act as an 'append' (or others) e.g.
//  P = ""
//  P:a = "append"
//  Q = "base "
//  Q:${P} = "OK2"         <-- this is executed second
//  Q:append = "me first"  <-- this is executed first
//  OVERRIDES = "a"
#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
enum StmtKind {
    WeakDefault,
    Assign,
    PlusEqual,
    EqualPlus,
    DotEqual,
    EqualDot,
    Append,
    Prepend,
    Remove,
}

impl Ord for StmtKind {
    fn cmp(&self, other: &Self) -> Ordering {
        self.order_value().cmp(&other.order_value())
    }
}

impl PartialOrd for StmtKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl StmtKind {
    // Default (?=) is handled at parse time
    fn order_value(&self) -> u8 {
        match self {
            StmtKind::Assign
            | StmtKind::PlusEqual
            | StmtKind::EqualPlus
            | StmtKind::DotEqual
            | StmtKind::EqualDot => 1,
            // ??=
            StmtKind::WeakDefault => 2,
            // :remove
            StmtKind::Append => 3,
            // :prepend
            StmtKind::Prepend => 4,
            // :remove
            StmtKind::Remove => 5,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
enum VariableOperationKind {
    WeakDefault,
    Assign,
    PlusEqual,
    EqualPlus,
    DotEqual,
    EqualDot,
    Append,
    SynthesizedAppend,
    Prepend,
    SynthesizedPrepend,
    Remove,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum OverrideOperation {
    Remove,
    Prepend,
    Append,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
struct VariableOperation {
    op_type: StmtKind,
    idx: NodeIndex<DefaultIx>,
}

impl Ord for VariableOperation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.op_type.cmp(&other.op_type)
    }
}

impl PartialOrd for VariableOperation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Default for FifoHeap<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Ord> FifoHeap<T> {
    pub fn new() -> Self {
        FifoHeap {
            seq: usize::MIN,
            heap: BTreeSet::new(),
        }
    }

    pub fn push(&mut self, val: T) {
        let seq = self.seq.checked_add(1).unwrap();
        self.seq = seq;
        self.heap.insert((val, seq));
    }
}

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

struct OverrideState {}

#[derive(Debug)]
struct DataSmart {
    ds: StableGraph<GraphItem, ()>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    expand_state: RefCell<Option<ExpansionState>>,
    active_overrides: RefCell<Option<IndexSet<String>>>,
    inside_compute_overrides: RefCell<()>,
}

// TODO: need to support more than 64 overrides?
fn score_override(
    active_overrides: &Option<IndexSet<String>>,
    candidate_overrides: &IndexSet<String>,
) -> Option<u64> {
    let mut ret = 0;

    // Reject this override if it contains terms not in active override set
    // TODO: change this to not clone
    let temp_cloned_active_overrides = active_overrides.clone().unwrap_or_default();
    if !candidate_overrides.is_subset(&temp_cloned_active_overrides) {
        return None;
    }

    if let Some(active_overrides) = active_overrides {
        for (i, active_override) in active_overrides.iter().enumerate() {
            if candidate_overrides.contains(active_override) {
                ret |= 1 << i;
            }
        }
    }

    Some(ret)
}

fn split_overrides<S: AsRef<str>>(input: S) -> IndexSet<String> {
    split_filter_empty(input.as_ref(), ":")
        .map(String::from)
        .collect::<IndexSet<String>>()
}

#[derive(Debug, Clone)]
enum OverridesData {
    Operation {
        lhs: IndexSet<String>,
        kind: OverrideOperation,
        rhs: IndexSet<String>,
        score: u64,
    },
    PureOverride {
        overrides: IndexSet<String>,
        score: u64,
    },
}

impl OverridesData {
    fn score(&self) -> u64 {
        match self {
            OverridesData::Operation { score, .. } => *score,
            OverridesData::PureOverride { score, .. } => *score,
        }
    }

    fn override_filter(&self) -> IndexSet<String> {
        match self {
            OverridesData::Operation { lhs, .. } => lhs.clone(),
            OverridesData::PureOverride { overrides, .. } => overrides.clone(),
        }
    }

    fn is_valid_for_filter(&self, override_filter: &IndexSet<String>) -> bool {
        // This is sensitive to order.
        // TODO: add example bitbake code to demonstrate
        match self {
            OverridesData::Operation { lhs, .. } => {
                lhs.is_empty() || lhs.as_slice() == override_filter.as_slice()
            }
            OverridesData::PureOverride { overrides, .. } => {
                eprintln!("checking {:?} == {:?}", overrides, override_filter);
                overrides.is_empty() || overrides.as_slice() == override_filter.as_slice()
            }
        }
    }

    fn is_active(&self, active_overrides: &Option<IndexSet<String>>) -> bool {
        active_overrides
            .as_ref()
            .map_or(false, |active_overrides| match self {
                OverridesData::Operation { rhs, .. } => rhs.is_subset(active_overrides),
                OverridesData::PureOverride { overrides, .. } => {
                    overrides.is_subset(active_overrides)
                }
            })
    }
}

#[derive(Debug, Clone)]
struct PreprocessedOperationData {
    override_data: Option<OverridesData>,
    // TODO: fold with above
    full_override: String,
    rhs: String,
    op_type: StmtKind,
    stmt_index: NodeIndex,
}

impl PreprocessedOperationData {
    fn is_override_operation(&self) -> bool {
        match self.override_data {
            None => false,
            Some(OverridesData::PureOverride { .. }) => false,
            Some(OverridesData::Operation { .. }) => true,
        }
    }

    fn override_operation(&self) -> Option<OverrideOperation> {
        if let Some(OverridesData::Operation { kind, ..}) = &self.override_data {
            return Some(*kind);
        }

        None
    }

    fn override_score(&self) -> u64 {
        match &self.override_data {
            None => 0,
            Some(od) => od.score(),
        }
    }
}

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

    fn apply_removes(&self, input: &String, removes: &HashSet<String>, level: usize) -> String {
        // TODO only only content flag and if not parsing
        let mut expanded_removes = HashMap::new();
        for r in removes.iter() {
            expanded_removes.insert(
                r.clone(),
                self.expand(r, level + 1)
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

    pub fn set_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        let var = var.into();
        let value = value.into();

        let var_parts = var.split_once(':');
        let base = var_parts.map_or(var.as_str(), |parts| parts.0);
        let override_str = var_parts.map(|parts| parts.1);

        let keyword_match = override_str.and_then(|s| KEYWORD_REGEX.captures(s));

        let stmt_kind = match keyword_match.and_then(|m| m.name("keyword").map(|k| k.as_str())) {
            None => StmtKind::Assign,
            Some("append") => StmtKind::Append,
            Some("prepend") => StmtKind::Prepend,
            Some("remove") => StmtKind::Remove,
            Some(_) => unreachable!(),
        };

        let stmt_idx = self.ds.add_node(GraphItem::StmtNode(StmtNode {
            lhs: override_str.map(String::from),
            kind: stmt_kind,
            rhs: value,
        }));

        let var_entry = self
            .vars
            .entry(base.to_string())
            .or_insert_with(|| self.ds.add_node(GraphItem::new_variable(base)));

        let var_data = self.ds.node_weight_mut(*var_entry).unwrap().variable_mut();

        var_data.operations.push(VariableOperation {
            op_type: stmt_kind,
            idx: stmt_idx,
        });

        self.ds.add_edge(*var_entry, stmt_idx, ());
    }

    pub fn expand<S: AsRef<str>>(&self, value: S, level: usize) -> DataSmartResult<String> {
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
            println!("{}EXPAND: {}", " ".repeat(level), value);
            let new_value = replace_all(
                &VAR_EXPANSION_REGEX,
                value.as_str(),
                |caps: &Captures| -> DataSmartResult<String> {
                    let match_str = caps.get(0).unwrap().as_str();
                    let referenced_var = &match_str[2..match_str.len() - 1];

                    println!("{} expand: {}", " ".repeat(level), referenced_var);
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
                        .get_var(referenced_var, level + 1)
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

    fn compute_overrides(&self, level: usize) -> DataSmartResult<()> {
        if let Ok(_) = RefCell::try_borrow_mut(&self.inside_compute_overrides) {
            if RefCell::borrow(&self.active_overrides).is_some() {
                return Ok(());
            }

            for i in 0..5 {
                eprintln!("{}+ override iteration {}", " ".repeat(level), i);
                let s = split_filter_empty(
                    &self.get_var("OVERRIDES", level + 1).unwrap_or_default(),
                    ":",
                )
                .map(String::from)
                .collect::<IndexSet<String>>();

                eprintln!("{} set overides = {:?}", " ".repeat(level), s);
                *RefCell::borrow_mut(&self.active_overrides) = Some(s);

                let s2 = split_filter_empty(
                    &self.get_var("OVERRIDES", level + 1).unwrap_or_default(),
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

    pub fn get_var<S: AsRef<str>>(&self, var: S, level: usize) -> Option<String> {
        let var = var.as_ref();

        // TODO: handle override syntax, e.g. getVar("A:pn-waves")
        let var_entry = self.vars.get(var)?;
        println!("{}get_var = {}", " ".repeat(level), var);

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
        self.compute_overrides(level + 1).unwrap();

        let override_state = &*RefCell::borrow(&self.active_overrides);

        let mut starting_value: Option<PreprocessedOperationData> = None;

        let mut preprocessed2 = var_data
            .operations
            .heap
            .iter()
            .filter_map(|op| {
                let mut overrides_data: Option<OverridesData> = None;
                let statement = self.ds.node_weight(op.0.idx).unwrap().statement();
                let original_override = statement.lhs.clone().unwrap_or_default();
                let expanded_lhs = statement
                    .lhs
                    .as_ref()
                    .map(|s| self.expand(s, level + 1))
                    .transpose()
                    .unwrap();

                if let Some(expanded_lhs) = expanded_lhs {
                    let mut locs = KEYWORD_REGEX.capture_locations();

                    if let Some(keyword_match) =
                        KEYWORD_REGEX.captures_read(&mut locs, &expanded_lhs)
                    {
                        let c = locs.get(1).unwrap();

                        let operation_kind = match &expanded_lhs[c.0..c.1] {
                            "append" => OverrideOperation::Append,
                            "prepend" => OverrideOperation::Prepend,
                            "remove" => OverrideOperation::Remove,
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

                        overrides_data = Some(OverridesData::Operation {
                            kind: operation_kind,
                            lhs: override_lhs,
                            rhs: split_overrides(override_rhs),
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

                        overrides_data = Some(OverridesData::PureOverride {
                            overrides,
                            score: override_score,
                        });
                    }
                }

                let ret = PreprocessedOperationData {
                    override_data: overrides_data,
                    rhs: statement.rhs.clone(),
                    full_override: original_override,
                    op_type: op.0.op_type,
                    stmt_index: op.0.idx,
                };

                if starting_value.as_ref().map_or(true, |sv| {
                    ret.op_type != StmtKind::Remove
                        && ret.override_operation() != Some(OverrideOperation::Remove)
                        && ret.override_score() >= sv.override_score()
                }) {
                    starting_value = Some(ret.clone());
                }

                Some(ret)
                // TODO: place expanded LHS in the assignment cache?
            })
            .collect::<Vec<_>>();

        let Some(starting_value) = starting_value else {
            return None;
        };

        eprintln!("selected starting value: {:?}", starting_value);

        let mut ret: String = starting_value.rhs.clone();

        // TODO: just filter in loop below
        preprocessed2.retain(|op| {
            op.override_score() >= starting_value.override_score()
                && op.stmt_index != starting_value.stmt_index
                // TODO: correct?
                && (op.op_type != StmtKind::Assign || op.is_override_operation())
        });

        let preprocessed: IndexMap<StmtKind, Vec<PreprocessedOperationData>> = preprocessed2
            .iter()
            .map(Clone::clone)
            .into_index_map_by(|op| op.op_type)
            .into_iter()
            .collect();

        eprintln!("data: {:#?}", preprocessed);

        let rhs_filter: IndexSet<String> = starting_value
            .override_data
            .as_ref()
            .map(|od| od.override_filter())
            .unwrap_or_default();

        // Use a map because in this code example, the append is applied once:
        //   TEST:${A} = "a"
        //   TEST:${A} = "a"
        //   A = "append"
        let mut deferred_appends: IndexMap<String, String> = IndexMap::new();
        let mut deferred_prepends: IndexMap<String, String> = IndexMap::new();

        for op_group in &preprocessed {
            match *op_group.0 {
                StmtKind::Assign => {
                    // Partition assignments into those that were resolved as operations and those that are pure assignments
                    let partition: (Vec<_>, Vec<_>) =
                        op_group.1.iter().partition(|a| a.is_override_operation());

                    assert!(partition.1.is_empty(),"{:?}", partition.1);

                    // let winning_assignment = partition
                    //     .1
                    //     .iter()
                    //     .sorted_by_cached_key(|o| o.override_data.as_ref().map_or(0, |d| d.score()))
                    //     .last()
                    //     .unwrap();
                    //
                    // eprintln!("{:?}", partition.0);
                    //
                    // ret = winning_assignment.rhs.clone().into();
                    // if let Some(od) = winning_assignment.override_data.as_ref() {
                    //     rhs_filter = od.override_filter();
                    // }

                    // Now process operations
                    for op in partition.0.iter() {
                        if op.override_data.as_ref().map_or(true, |od| {
                            od.is_active(override_state) && od.is_valid_for_filter(&rhs_filter)
                        }) {
                            match op.override_data.as_ref() {
                                Some(OverridesData::Operation { kind, .. }) => match kind {
                                    OverrideOperation::Remove => {
                                        let removes: HashSet<String> =
                                            HashSet::from([op.rhs.clone()]);
                                        let new_ret = self.apply_removes(&ret, &removes, level + 1);
                                        ret = new_ret;
                                    }
                                    OverrideOperation::Append => {
                                        deferred_appends
                                            .insert(op.full_override.clone(), op.rhs.clone());
                                    }
                                    OverrideOperation::Prepend => {
                                        deferred_prepends
                                            .insert(op.full_override.clone(), op.rhs.clone());
                                    }
                                },
                                _ => unreachable!(),
                            }
                        }
                    }
                }
                StmtKind::Remove => {
                    let mut removes: HashSet<String> = HashSet::new();
                    for remove in op_group.1 {
                        if remove.override_data.as_ref().map_or(true, |od| {
                            od.is_active(override_state) && od.is_valid_for_filter(&rhs_filter)
                        }) {
                            removes.insert(remove.rhs.clone());
                        }
                    }

                    let new_ret = self.apply_removes(&ret, &removes, level + 1);
                    ret = new_ret;
                }
                StmtKind::Append => {
                    for append in op_group.1 {
                        if append.override_data.as_ref().map_or(true, |od| {
                            od.is_active(override_state) && od.is_valid_for_filter(&rhs_filter)
                        }) {
                            ret += &append.rhs.clone();
                        }
                    }
                }
                StmtKind::Prepend => {
                    for prepend in op_group.1 {
                        ret = format!("{}{}", prepend.rhs.clone(), ret);
                    }
                }
                _ => panic!("unimplemented"),
            }
        }

        for (_, append) in deferred_appends.drain(..) {
            ret += &append.clone();
        }

        for (_, prepend) in deferred_prepends.drain(..) {
            ret = format!("{}{}", prepend.clone(), ret);
        }

        ret = self.expand(ret, level + 1).unwrap();
        //*cached = ret.clone();

        Some(ret)
    }
}
/// Return an `IndexMap` of keys mapped to a list of their corresponding values.
///
/// Code based on [`.into_group_map()`](crate::Itertools::into_group_map)
pub fn into_index_map<I, K, V>(iter: I) -> IndexMap<K, Vec<V>>
where
    I: Iterator<Item = (K, V)>,
    K: Hash + Eq,
{
    let mut lookup = IndexMap::new();

    iter.for_each(|(key, val)| {
        lookup.entry(key).or_insert_with(Vec::new).push(val);
    });

    lookup
}

pub fn into_index_map_by<I, K, V>(iter: I, f: impl Fn(&V) -> K) -> IndexMap<K, Vec<V>>
where
    I: Iterator<Item = V>,
    K: Hash + Eq,
{
    into_index_map(iter.map(|v| (f(&v), v)))
}

trait IntoIndexMap: Iterator {
    fn into_index_map_by<K, V, F>(self, f: F) -> IndexMap<K, Vec<V>>
    where
        Self: Iterator<Item = V> + Sized,
        K: Hash + Eq,
        F: Fn(&V) -> K,
    {
        into_index_map_by(self, f)
    }
}

impl<T> IntoIndexMap for T where T: Iterator + ?Sized {}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>,
    cached_value: RefCell<Option<String>>,
    // TODO: iterative cache for OVERRIDES
}

#[derive(Debug)]
enum OverrideSpec {
    Split { lhs: String, rhs: String },
    Single(String),
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
    use crate::DataSmart;

    #[test]
    fn none() {
        let mut d = DataSmart::new();
        assert_eq!(d.get_var("NOT_EXIST", 0), None);
    }

    #[test]
    fn override_score() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");

        assert_eq!(d.get_var("TEST", 0), Some("1".into()));
    }

    #[test]
    fn override_score_2() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("OVERRIDES", "more");

        assert_eq!(d.get_var("TEST", 0), Some("2".into()));
    }

    #[test]
    fn override_score_3() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("OVERRIDES", "more:specific");

        assert_eq!(d.get_var("TEST", 0), Some("3".into()));
    }

    #[test]
    fn override_score_4() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:more", "2");
        d.set_var("TEST:more:specific", "3");
        d.set_var("TEST:more:specific", "4");
        d.set_var("OVERRIDES", "more:specific");

        assert_eq!(d.get_var("TEST", 0), Some("4".into()));
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

        assert_eq!(d.get_var("TEST", 0), Some("6".into()));
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

        assert_eq!(d.get_var("TEST", 0), Some("1".into()));
    }

    #[test]
    fn override_score_7() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST:append", "2");

        assert_eq!(d.get_var("TEST", 0), Some("12".into()));
    }

    #[test]
    fn override_score_8() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");

        assert_eq!(d.get_var("TEST", 0), Some("23".into()));
    }

    #[test]
    fn override_score_9() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");

        assert_eq!(d.get_var("TEST", 0), Some("234".into()));
    }

    #[test]
    fn override_score_10() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1");
        d.set_var("TEST", "2");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");
        d.set_var("TEST:append:a", "NO");

        assert_eq!(d.get_var("TEST", 0), Some("234".into()));
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

        assert_eq!(d.get_var("TEST", 0), Some("BASE34".into()));
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

        assert_eq!(d.get_var("TEST", 0), Some("OH YESBASE34".into()));
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

        assert_eq!(d.get_var("TEST", 0), Some("WHAT34".into()));
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

        assert_eq!(d.get_var("TEST", 0), Some("WHAT!34".into()));
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

        assert_eq!(d.get_var("TEST", 0), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_score_16() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "10");
        d.set_var("TEST:append", "3");
        d.set_var("TEST:append", "4");

        assert_eq!(d.get_var("TEST", 0), Some("34".into()));
    }

    #[test]
    fn override_score_17() {
        let mut d = DataSmart::new();
        d.set_var("TEST:append", "why?");
        d.set_var("TEST:a:b:append", "first");
        d.set_var("TEST:a:b:${OP}", "OP");
        d.set_var("OP", "append");
        d.set_var("OVERRIDES", "a:b");

        assert_eq!(d.get_var("TEST", 0), Some("firstOPwhy?".into()));
    }

    #[test]
    fn order_of_operations() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");
        d.set_var("TEST:${B}", "2");
        d.set_var("TEST:append", " 4");
        d.set_var("B", "remove");

        assert_eq!(d.get_var("TEST", 0), Some("1  3 4".into()));
    }

    #[test]
    fn indirection_1() {
        let mut d = DataSmart::new();
        d.set_var("TEST", "1 2 3");
        d.set_var("TEST:${${B}}", "2");
        d.set_var("B", "${W}");
        d.set_var("W", "Q");
        d.set_var("Q", "remove");

        assert_eq!(d.expand("TEST:${${B}}", 0).unwrap(), "TEST:remove");
        assert_eq!(d.get_var("TEST", 0), Some("1  3".into()));
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

        assert_eq!(d.expand("TEST:${${B}}", 0).unwrap(), "TEST:append");
        assert_eq!(d.get_var("TEST", 0), Some("1 2 3 5  4 ".into()));
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

        assert_eq!(d.expand("TEST:${${B}}", 0).unwrap(), "TEST:append");
        assert_eq!(d.get_var("TEST", 0), Some("OK 5  4".into()));
    }

    #[test]
    fn wat() {
        let mut d = DataSmart::new();

        d.set_var("TEST", "a b c");
        d.set_var("TEST:${A}", "b");
        d.set_var("A", "remove");

        println!("\n");
        //println!("\nOVERRIDES = {:?}\n", d.get_var("OVERRIDES"));
        println!("TEST = {:?}\n", d.get_var("TEST", 0));
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
    println!("TEST = {:?}\n", d.get_var("TEST", 0));
    //
    // println!("{:?}", Dot::with_config(&d.ds, &[]));
}
