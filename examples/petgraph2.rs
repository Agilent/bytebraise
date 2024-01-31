use std::cmp::Ordering;
use fxhash::FxHashMap;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::DefaultIx;
use std::collections::BinaryHeap;
use once_cell::sync::Lazy;
use petgraph::dot::{Config, Dot};
use petgraph::prelude::StableGraph;
use regex::Regex;

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());
static SETVAR_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(?P<base>.*?)(?P<keyword>:append|:prepend|:remove)(?::(?P<add>[^A-Z]*))?$")
        .unwrap()
});
static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\s").unwrap());


#[derive(Clone, Debug)]
pub struct FifoHeap<T> {
    seq: usize,
    heap: BinaryHeap<(T, usize)>,
}

// TODO: varflags as separate variables?

#[derive(Eq, PartialEq, Debug)]
enum VariableOperationType {
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

impl Ord for VariableOperationType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.order_value().cmp(&other.order_value())
    }
}

impl PartialOrd for VariableOperationType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl VariableOperationType {
    fn order_value(&self) -> u8 {
        match self {
            VariableOperationType::WeakDefault => 1,
            VariableOperationType::Default => 2,
            VariableOperationType::Assign | VariableOperationType::PlusEqual | VariableOperationType::EqualPlus
            | VariableOperationType::DotEqual | VariableOperationType::EqualDot => 3,
            VariableOperationType::Append => 4,
            VariableOperationType::Prepend => 5,
            VariableOperationType::Remove => 6,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
struct VariableOperation {
    op_type: VariableOperationType,
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
            heap: BinaryHeap::new(),
        }
    }

    pub fn push(&mut self, val: T) {
        let seq = self.seq.checked_sub(1).unwrap();
        self.seq = seq;
        self.heap.push((val, seq));
    }

    pub fn pop(&mut self) -> Option<T> {
        let (val, _) = self.heap.pop()?;
        Some(val)
    }
}

#[derive(Debug)]
struct DataSmart {
    ds: StableGraph<GraphItem, ()>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
}

impl DataSmart {
    pub fn new() -> DataSmart {
        DataSmart {
            ds: StableGraph::new(),
            vars: FxHashMap::default(),
        }
    }

    pub fn set_var<T: Into<String>, S: Into<String>>(&mut self, var: T, value: S) {
        let var = var.into();
        let value = value.into();

        // Check for append/prepend/remove operation
        if let Some(regex_match) = SETVAR_REGEX.captures(&var) {
            // Base variable name, possibly with its own overrides. For example:
            // P:class-target:append:arm yields:
            //      base: P:class-target
            //      keyword: :append
            //      overrides: arm
            let base = regex_match.name("base").unwrap().as_str();
            let keyword = regex_match.name("keyword").unwrap().as_str();
            let overridestr = regex_match.name("add").map(|o| o.as_str().to_string());

            let base_variable_index = self._get_or_create_var(var_name.clone());
            let override_ = overridestr.map(|s| self._intern_expression(s));
            let value = self._intern_expression(value.clone());
            let base_variable_data = self.ds.node_weight_mut(base_variable_index).unwrap();

            let GraphItem::Variable(var) = base_variable_data else { panic!(); };
            match keyword {
                "_append" => {
                    var.appends.push(Apr { override_, value });
                }
                "_prepend" => {
                    var.prepends.push(Apr { override_, value });
                }
                "_remove" => {
                    var.removes.push(Apr { override_, value });
                }
                _ => unreachable!()
            }
        }
    }
}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>,
}

#[derive(Debug)]
enum ExpressionNode {
    Assign,
    LHS(String),
    Concatenate,
    GetVariable,
    Constant(String),
}

#[derive(Debug)]
enum GraphItem {
    Variable(Variable),
    ExpressionNode(ExpressionNode),
}

impl GraphItem {
    fn variable_mut(&mut self) -> &mut Variable {
        match self {
            GraphItem::Variable(v) => v,
            _ => panic!("Expected GraphItem::Variable"),
        }
    }
}

impl GraphItem {
    fn new_variable<T: Into<String>>(name: T) -> GraphItem {
        GraphItem::Variable(Variable {
            name: name.into(),
            operations: FifoHeap::new(),
        })
    }
}

fn main() {
    let mut d = DataSmart::new();

    let a = d.ds.add_node(GraphItem::new_variable("A"));
    d.vars.insert("A".into(), a);

    let op = d.ds.add_node(GraphItem::ExpressionNode(ExpressionNode::Constant("value".into())));

    let v = d.ds.node_weight_mut(a).unwrap();
    v.variable_mut().operations.push(VariableOperation {
        idx: op,
        op_type: VariableOperationType::Assign,
    });

    println!("{:?}", Dot::with_config(&d.ds, &[]));
}