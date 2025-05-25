use std::cmp::Ordering;

use petgraph::graph::NodeIndex;
use petgraph::stable_graph::DefaultIx;

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum StmtKind {
    WeakDefault,
    Default,
    Assign,
    PlusEqual,
    EqualPlus,
    DotEqual,
    EqualDot,
    // TODO: you can have :append +=, etc. so maybe maybe 'append', 'prepend', 'remove' modifiers
    //  instead of their own kind?
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
            // ?=
            StmtKind::Default => 2,
            // ??=
            StmtKind::WeakDefault => 3,
            // :remove
            StmtKind::Append => 4,
            // :prepend
            StmtKind::Prepend => 5,
            // :remove
            StmtKind::Remove => 6,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct VariableOperation {
    pub(crate) op_type: StmtKind,
    pub(crate) idx: NodeIndex<DefaultIx>,
    // TODO: also store edge index maybe?
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
