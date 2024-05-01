use std::borrow::Cow;
use std::cell::{RefCell};
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashSet};
use std::ops::Deref;

use anyhow::Context;
use fxhash::FxHashMap;
use indexmap::IndexSet;
use itertools::Itertools;
use once_cell::sync::Lazy;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::DefaultIx;
use regex::{Captures, Regex};
use scopeguard::{defer, guard, ScopeGuard};
use bytebraise::data_smart::errors::{DataSmartError, DataSmartResult};
use bytebraise::data_smart::utils::{replace_all, split_filter_empty};

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());

static PYTHON_EXPANSION_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\{@.+?}").unwrap());

static SETVAR_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(?P<base>.*?)(?P<keyword>:append|:prepend|:remove)?(?P<add>:.*)?$")
        .unwrap()
});
static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\s").unwrap());


#[derive(Clone, Debug)]
pub struct FifoHeap<T> {
    seq: usize,
    heap: BTreeSet<(T, usize)>,
}

// TODO: An 'assign' may actually act as an 'append' (or others) e.g.
//  P = ""
//  P:test = "append"
//  A:${P:test} = "OK2"
//    BUT thankfully we don't need to factor this into execution operation of statements. i.e.
//    our binary heap approach still works :)
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum StmtKind {
    WeakDefault,
    Default,
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
    fn order_value(&self) -> u8 {
        match self {
            StmtKind::WeakDefault => 1,
            StmtKind::Default => 2,
            StmtKind::Assign | StmtKind::PlusEqual | StmtKind::EqualPlus
            | StmtKind::DotEqual | StmtKind::EqualDot => 3,
            StmtKind::Append => 4,
            StmtKind::Prepend => 5,
            StmtKind::Remove => 6,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
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

        let regex_match = SETVAR_REGEX.captures(&var).unwrap();
        let base = regex_match.name("base").unwrap().as_str();

        //println!("{:?}", regex_match);

        let stmt_kind = match regex_match.name("keyword").map(|m| m.as_str()) {
            None => StmtKind::Assign,
            Some(":append") => StmtKind::Append,
            Some(":prepend") => StmtKind::Prepend,
            Some(":remove") => StmtKind::Remove,
            Some(_) => unreachable!(),
        };

        let var_without_keyword = regex_match.name("add").map(|m| m.as_str().to_string()).unwrap_or_default();

        let stmt_idx = self.ds.add_node(GraphItem::StmtNode(StmtNode {
            lhs: var_without_keyword,
            kind: stmt_kind,
            rhs: value,
        }));

        let var_entry = self.vars.entry(base.to_string()).or_insert_with(|| {
            self.ds.add_node(GraphItem::new_variable(base))
        });

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
            let new_value = replace_all(&*VAR_EXPANSION_REGEX, value.as_str(), |caps: &Captures| -> DataSmartResult<String> {
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

                Ok(self.get_var(referenced_var, level + 1).unwrap_or(match_str.to_string()))
            })?;

            if value == new_value {
                break;
            }
            value = new_value.to_string();
        }

        Ok(value)
    }

    fn compute_overrides(&self, level: usize) -> DataSmartResult<()> {
        if let Ok(_) = RefCell::try_borrow_mut(&self.inside_compute_overrides) {
            if RefCell::borrow(&self.active_overrides)
                .is_some()
            {
                return Ok(());
            }

            for i in 0..5 {
                eprintln!("{}+ override iteration {}", " ".repeat(level), i);
                let s = split_filter_empty(&self.get_var("OVERRIDES", level + 1).unwrap(), ":").map(|s| String::from(s)).collect::<IndexSet<String>>();

                eprintln!("{} set overides = {:?}", " ".repeat(level), s);
                *RefCell::borrow_mut(&self.active_overrides) = Some(s);

                let s2 = split_filter_empty(&self.get_var("OVERRIDES", level + 1).unwrap(), ":").map(|s| String::from(s)).collect::<IndexSet<String>>();

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

        self.compute_overrides(level + 1).unwrap();

        let override_state = &*RefCell::borrow(&self.active_overrides);

        //let overrides = self.get_var("OVERRIDES");

        for op in var_data.operations.heap.iter() {
            eprintln!("||||| {:?}", op);
            let index = op.0.idx;
            let q = self.ds.node_weight(index).unwrap().statement();

            let mut run = false;
            if q.lhs.is_empty() {
                run = true;
            } else {
                let expanded_lhs = self.expand(&q.lhs, level + 1).unwrap();
                eprintln!("{:?}", expanded_lhs);
                let operation_overrides = split_filter_empty(&expanded_lhs, ":").map(|s| String::from(s)).collect::<IndexSet<String>>();
                if let Some(overrides) = override_state {
                    if operation_overrides.is_subset(overrides) {
                        // TODO: record fact that an override was applied and add an edge to OVERRIDES on this statement node.
                        run = true;
                    }
                }
            }

            match op.0.op_type {
                StmtKind::Append => {
                    if run {
                        ret = ret.map(|s| {
                            s + " " + &*q.rhs.clone()
                        });
                    }
                }
                StmtKind::Assign => {
                    if run {
                        eprintln!("= {}, {}", q.rhs, op.1);
                        ret = q.rhs.clone().into();
                    }
                }
                _ => unimplemented!()
            }
        }

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
struct StmtNode {
    kind: StmtKind,
    lhs: String,
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

    d.set_var("TARGET_ARCH", "x86_64");
    d.set_var("PN", "test-${TARGET_ARCH}");
    d.set_var("VERSION", "1");
    d.set_var("VERSION:pn-test-${TARGET_ARCH}", "2");
    d.set_var("OVERRIDES", "pn-${PN}");

    //parse_value("${${M}}");

    println!("\n");
    //println!("\nOVERRIDES = {:?}\n", d.get_var("OVERRIDES"));
    println!("VERSION = {:?}\n", d.get_var("VERSION", 0));

    println!("{:?}", Dot::with_config(&d.ds, &[]));
}