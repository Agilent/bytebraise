use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap, HashSet};

use fxhash::FxHashMap;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use once_cell::sync::Lazy;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::DefaultIx;
use regex::{CaptureLocations, Captures, Regex};
use scopeguard::{defer, guard, ScopeGuard};

use bytebraise::data_smart::errors::{DataSmartError, DataSmartResult};
use bytebraise::data_smart::utils::{replace_all, split_filter_empty, split_keep};

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());

static PYTHON_EXPANSION_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\{@.+?}").unwrap());

static KEYWORD_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"(?P<all>(?P<keyword>:append|:prepend|:remove)(?:$|:))").unwrap());

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
//    BUT thankfully we don't need to factor this into execution operation of statements. i.e.
//    our binary heap approach still works :)
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

#[derive(Debug)]
enum OverrideOperation {
    Remove, Prepend, Append
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
fn score_override(active_overrides: &Option<IndexSet<String>>, candidate_overrides: &IndexSet<String>) -> Option<u64> {
    let mut ret = 0;

    // Reject this override if it contains terms not in active override set
    // TODO: change this to not clone
    let temp_cloned_active_overrides = active_overrides.as_ref().map(|a| a.clone()).unwrap_or_default();
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
        .map(|s| String::from(s))
        .collect::<IndexSet<String>>()
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

    pub fn set_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        let var = var.into();
        let value = value.into();

        let var_parts = var.split_once(':');
        let base = var_parts.map_or(var.as_str(), |parts| parts.0);
        let override_str = var_parts.map(|parts| parts.1);

        let keyword_match = override_str.and_then(|s| KEYWORD_REGEX.captures(s));

        let stmt_kind = match keyword_match.and_then(|m| m.name("keyword").map(|k| k.as_str())) {
            None => StmtKind::Assign,
            Some(":append") => StmtKind::Append,
            Some(":prepend") => StmtKind::Prepend,
            Some(":remove") => StmtKind::Remove,
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

    fn expand<S: AsRef<str>>(&self, value: S, level: usize) -> DataSmartResult<String> {
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
                &*VAR_EXPANSION_REGEX,
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
                let s = split_filter_empty(&self.get_var("OVERRIDES", level + 1).unwrap(), ":")
                    .map(|s| String::from(s))
                    .collect::<IndexSet<String>>();

                eprintln!("{} set overides = {:?}", " ".repeat(level), s);
                *RefCell::borrow_mut(&self.active_overrides) = Some(s);

                let s2 = split_filter_empty(&self.get_var("OVERRIDES", level + 1).unwrap(), ":")
                    .map(|s| String::from(s))
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

        let mut ret: Option<String> = None;

        let w = self.ds.node_weight(*var_entry).unwrap();
        let var_data = w.variable();

        if var != "OVERRIDES" {
            let mut cached = RefCell::borrow_mut(&var_data.cached_value);
            if cached.is_some() {
                return cached.clone();
            }
        }

        // TODO: only do this if needed
        self.compute_overrides(level + 1).unwrap();

        let override_state = &*RefCell::borrow(&self.active_overrides);

        #[derive(Debug)]
        enum OverridesData {
            Operation {
                lhs: IndexSet<String>,
                kind: OverrideOperation,
                rhs: IndexSet<String>,
                score: u64,
            },
            PureOverride  {
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
                    OverridesData::Operation { lhs, .. } => lhs.as_slice() == override_filter.as_slice(),
                    OverridesData::PureOverride { overrides, .. } => overrides.as_slice() == override_filter.as_slice(),
                }
            }

            fn is_active(&self, active_overrides: &Option<IndexSet<String>>) -> bool {
                active_overrides.as_ref().map_or(false, |active_overrides| {
                    match self {
                        OverridesData::Operation { rhs, .. } => rhs.is_subset(active_overrides),
                        OverridesData::PureOverride { overrides, .. } => overrides.is_subset(active_overrides),
                    }
                })
            }
        }

        #[derive(Debug)]
        struct PreprocessedOperationData {
            override_data: Option<OverridesData>,
            rhs: String,
        }

        let mut preprocessed: IndexMap<StmtKind, Vec<PreprocessedOperationData>> = IndexMap::new();

        // Pre-process operations in the priority heap
        for op_group in &var_data.operations.heap.iter().group_by(|o| o.0.op_type) {
            let mut preprocessed_ops = vec![];

            for op in op_group.1 {
                let mut overrides_data: Option<OverridesData> = None;
                let assign_stmt = self.ds.node_weight(op.0.idx).unwrap().statement();
                let expanded_lhs = assign_stmt.lhs.as_ref().map(|s| self.expand(s, level + 1)).transpose().unwrap();

                if let Some(expanded_lhs) = expanded_lhs {
                    let mut locs = KEYWORD_REGEX.capture_locations();

                    if let Some(keyword_match) = KEYWORD_REGEX.captures_read(&mut locs, &expanded_lhs) {
                        let c = locs.get(1).unwrap();

                        let operation_kind = match &expanded_lhs[c.0..c.1] {
                            ":append" => OverrideOperation::Append,
                            ":prepend" => OverrideOperation::Prepend,
                            ":remove" => OverrideOperation::Remove,
                            _ => unreachable!(),
                        };

                        let override_lhs = &expanded_lhs[0..c.0];
                        let override_rhs = split_overrides(&expanded_lhs[c.1..]);
                        let Some(override_score) = score_override(override_state, &override_rhs) else { continue };

                        overrides_data = Some(OverridesData::Operation {
                            kind: operation_kind,
                            lhs: split_overrides(override_lhs),
                            rhs: override_rhs,
                            score: override_score,
                        });
                    } else {
                        let overrides = split_overrides(expanded_lhs);
                        let Some(override_score) = score_override(override_state, &overrides) else { continue };

                        overrides_data = Some(OverridesData::PureOverride {
                            overrides,
                            score: override_score,
                        });
                    }
                }

                preprocessed_ops.push(PreprocessedOperationData {
                    override_data: overrides_data,
                    rhs: assign_stmt.rhs.clone(),
                })
                // TODO: place expanded LHS in the assignment cache?
            }

            preprocessed.insert(op_group.0, preprocessed_ops);
        }

        eprintln!("{:#?}", preprocessed);

        let mut rhs_filter: IndexSet<String> = IndexSet::new();

        for op_group in &preprocessed {
            match *op_group.0 {
                StmtKind::Assign => {
                    // TODO: handle overrides in rhs of override data
                    let winning_assignment = op_group.1.iter().sorted_by_cached_key(|o| {
                        o.override_data.as_ref().map_or(0, |d| d.score())
                    }).last().unwrap();

                    if let Some(od) = winning_assignment.override_data.as_ref() {
                        rhs_filter = od.override_filter();
                    }

                    ret = winning_assignment.rhs.clone().into();
                },
                StmtKind::Remove => {
                    let mut removes: HashSet<String> = HashSet::new();
                    for remove in op_group.1 {
                        if remove.override_data.as_ref().map_or(true, |od| od.is_active(override_state) && od.is_valid_for_filter(&rhs_filter)) {
                            removes.insert(remove.rhs.clone());
                        }
                    }

                    // TODO only only content flag and if not parsing
                    let mut expanded_removes = HashMap::new();
                    for r in &removes {
                        expanded_removes.insert(
                            r.clone(),
                            self.expand(r, level + 1)
                                .unwrap()
                                .split_whitespace()
                                .map(|v| v.to_string())
                                .collect::<Vec<_>>(),
                        );
                    }

                    if let Some(ret) = &mut ret {
                        let mut val = String::new();
                        for v in split_keep(&WHITESPACE_REGEX, ret) {
                            let mut skip = false;
                            for r in &removes {
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
                        *ret = val;
                    }
                }
                _ => panic!("unimplemented"),
            }
        }

        // // Iterate over operations in the priority heap, aggregated by operation type
        // for op_group in &var_data.operations.heap.iter().group_by(|o| o.0.op_type) {
        //     if op_group.0 == StmtKind::Assign {
        //         let scored_ops = op_group
        //             .1
        //             .sorted_by_cached_key(|op2| {
        //                 let assign_stmt = self.ds.node_weight(op2.0.idx).unwrap().statement();
        //
        //                 // TODO: cache LHS?
        //                 assign_stmt.lhs.as_ref().map_or(0, |s| {
        //                     let expanded_override_string = self.expand(&s, level + 1).unwrap();
        //                     let operation_overrides =
        //                         split_filter_empty(&expanded_override_string, ":")
        //                             .map(|s| String::from(s))
        //                             .collect::<IndexSet<String>>();
        //                     score_override(override_state, &operation_overrides)
        //                 })
        //             })
        //             .collect::<Vec<_>>();
        //
        //         let winning_op = scored_ops.last().unwrap();
        //         let q = self.ds.node_weight(winning_op.0.idx).unwrap().statement();
        //         ret = q.rhs.clone().into();
        //
        //         eprintln!("{:?}", scored_ops);
        //     } else {
        //         for op in op_group.1 {
        //             let index = op.0.idx;
        //             let q = self.ds.node_weight(index).unwrap().statement();
        //
        //             let mut run = false;
        //             match &q.lhs {
        //                 None => run = true,
        //                 Some(override_str) => {
        //                     let expanded_lhs = self.expand(&override_str, level + 1).unwrap();
        //                     eprintln!("{:?}", expanded_lhs);
        //                     let operation_overrides = split_filter_empty(&expanded_lhs, ":")
        //                         .map(|s| String::from(s))
        //                         .collect::<IndexSet<String>>();
        //                     if let Some(overrides) = override_state {
        //                         if operation_overrides.is_subset(overrides) {
        //                             // TODO: record fact that an override was applied and add an edge to OVERRIDES on this statement node.
        //                             run = true;
        //                         }
        //                     }
        //                 }
        //             }
        //
        //             match op_group.0 {
        //                 StmtKind::Append => {
        //                     if run {
        //                         ret = ret.map(|s| s + " " + &*q.rhs.clone());
        //                     }
        //                 }
        //                 StmtKind::Assign => {
        //                     if run {
        //                         eprintln!("= {}, {}", q.rhs, op.1);
        //                         ret = q.rhs.clone().into();
        //                     }
        //                 }
        //                 _ => unimplemented!(),
        //             }
        //         }
        //     }
        // }

        ret = ret.map(|s| self.expand(s, level + 1).unwrap());
        //*cached = ret.clone();

        ret
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

    d.set_var("A:append:${B}", "C");
    d.set_var("A:${B}", "D");*/

    d.set_var("TEST:bar", "testvalue2");
    d.set_var("TEST:foo", "testvalue4");
    d.set_var("TEST:some_val", "testvalue3 testvalue5");
    d.set_var("TEST:bar:foo:some_val:c", "winner");
    d.set_var("OVERRIDES", "foo:bar:some_val");

    //parse_value("${${M}}");

    println!("\n");
    //println!("\nOVERRIDES = {:?}\n", d.get_var("OVERRIDES"));
    println!("TEST = {:?}\n", d.get_var("TEST", 0));

    println!("{:?}", Dot::with_config(&d.ds, &[]));
}
