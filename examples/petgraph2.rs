use std::cmp::Ordering;
use fxhash::FxHashMap;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::DefaultIx;
use std::collections::BinaryHeap;
use petgraph::prelude::StableGraph;

#[derive(Clone, Debug)]
pub struct FifoHeap<T> {
    seq: usize,
    heap: BinaryHeap<(T,usize)>
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

impl<T:Ord> FifoHeap<T> {
    pub fn new()->Self {
        FifoHeap {
            seq: usize::MAX,
            heap: BinaryHeap::new()
        }
    }

    pub fn push(&mut self, val:T) {
        let seq = self.seq.checked_sub(1).unwrap();
        self.seq = seq;
        self.heap.push((val, seq));
    }

    pub fn pop(&mut self)->Option<T> {
        let (val,_) = self.heap.pop()?;
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
}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>
}

#[derive(Debug)]
enum ExpressionNode {
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
    if let GraphItem::Variable(v) = v {
        v.operations.push(VariableOperation {
            idx: op,
            op_type: VariableOperationType::Assign,
        })
    }
}