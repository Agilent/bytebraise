use std::borrow::Cow;
use std::cell::{RefCell};
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashSet};

use anyhow::Context;
use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::DefaultIx;
use regex::{Captures, Regex};
use scopeguard::{defer, guard, ScopeGuard};
use bytebraise::data_smart::errors::{DataSmartError, DataSmartResult};
use bytebraise::data_smart::utils::ReplaceFallible;

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
            seq: usize::MAX,
            heap: BTreeSet::new(),
        }
    }

    pub fn push(&mut self, val: T) {
        let seq = self.seq.checked_sub(1).unwrap();
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

#[derive(Debug)]
struct DataSmart {
    ds: StableGraph<GraphItem, ()>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    expand_state: RefCell<Option<ExpansionState>>,
}

impl DataSmart {
    pub fn new() -> DataSmart {
        DataSmart {
            ds: StableGraph::new(),
            vars: FxHashMap::default(),
            expand_state: RefCell::new(None),
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

    fn expand<S: AsRef<str>>(&self, value: S) -> DataSmartResult<String> {
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
            let new_value = VAR_EXPANSION_REGEX.replace_fallible(value.as_ref(), |caps: &Captures| -> DataSmartResult<String> {
                let match_str = caps.get(0).unwrap().as_str();
                let referenced_var = &match_str[2..match_str.len() - 1];

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

                Ok(self.get_var(referenced_var).unwrap_or(match_str.to_string()))
            })?;

            if value == new_value {
                break;
            }
            value = new_value.to_string();
        }

        Ok(value)
    }

    pub fn get_var<S: AsRef<str>>(&self, var: S) -> Option<String> {
        let var = var.as_ref();

        let var_entry = self.vars.get(var)?;
        println!("{:?}", self.ds.node_weight(*var_entry));

        let mut ret: Option<String> = None;

        let w = self.ds.node_weight(*var_entry).unwrap();
        let var_data = w.variable();
        let mut cached = RefCell::borrow_mut(&var_data.cached_value);
        if cached.is_some() {
            return cached.clone();
        }

        for op in var_data.operations.heap.iter() {
            let index = op.0.idx;
            let q = self.ds.node_weight(index).unwrap();
            println!("\top = {:?}", op);
            println!("\t{:?}", q);

            match op.0.op_type {
                StmtKind::Append => {
                    ret = ret.map(|s| {
                        s + " " + &*q.statement().rhs.clone()
                    });
                }
                StmtKind::Assign => {
                    ret = q.statement().rhs.clone().into();
                }
                _ => unimplemented!()
            }
        }

        ret = ret.map(|s| self.expand(s).unwrap());
        *cached = ret.clone();

        ret
    }
}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>,
    cached_value: RefCell<Option<String>>,
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

enum ToyNode {
    GetVariable(String),
    Indirection(Box<ToyNode>),
    Concatenate(Vec<Box<ToyNode>>),
    Constant(String),
    Python(String),
}


fn parse_value<S: Into<String>>(val: S) -> ToyNode {
    let input = val.into();

    let mut result = Vec::new();
    let mut last_end = 0;

    // Iterate over all matches
    for mat in VAR_EXPANSION_REGEX.find_iter(&input) {
        let (start, end) = (mat.start(), mat.end());

        // Check if there's a non-matching portion before the current match
        if start > last_end {
            // Add the non-matching portion to the result
            result.push((input[last_end..start].to_string(), false));
        }

        // Add the current match to the result
        result.push((mat.as_str().to_string(), true));

        last_end = end;
    }

    // Check if there's any remaining non-matching portion at the end
    if last_end < input.len() {
        result.push((input[last_end..].to_string(), false));
    }

    dbg!(result);

    panic!();
    //return ToyNode::Concatenate(result);
}

fn main() {
    let mut d = DataSmart::new();

    d.set_var("OVERRIDES", "${TARGET_OS}:${TRANSLATED_TARGET_ARCH}:pn-${PN}:layer-${FILE_LAYERNAME}:${MACHINEOVERRIDES}:${DISTROOVERRIDES}:${CLASSOVERRIDE}${LIBCOVERRIDE}:forcevariable");
    d.set_var("A:append:${B}", "C");
    d.set_var("A:${B}", "D");

    //parse_value("${${M}}");

    println!("\n");
    let ret = d.get_var("OVERRIDES");
    println!("\n");

    println!("ret = {:?}\n", ret);

    println!("{:?}", Dot::with_config(&d.ds, &[]));
}