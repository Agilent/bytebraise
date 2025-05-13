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

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum VariableOperationKind {
    WeakDefault,
    Default,
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

impl From<StmtKind> for VariableOperationKind {
    fn from(value: StmtKind) -> Self {
        match value {
            StmtKind::Assign => Self::Assign,
            StmtKind::Append => Self::Append,
            StmtKind::WeakDefault => Self::WeakDefault,
            StmtKind::PlusEqual => Self::PlusEqual,
            StmtKind::EqualPlus => Self::EqualPlus,
            StmtKind::DotEqual => Self::DotEqual,
            StmtKind::EqualDot => Self::EqualDot,
            StmtKind::Prepend => Self::Prepend,
            StmtKind::Remove => Self::Remove,
            StmtKind::Default => Self::Default,
        }
    }
}

impl Ord for VariableOperationKind {
    fn cmp(&self, other: &Self) -> Ordering {
        self.order_value().cmp(&other.order_value())
    }
}

impl PartialOrd for VariableOperationKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl VariableOperationKind {
    // Default (?=) is handled at parse time
    fn order_value(&self) -> u8 {
        match self {
            VariableOperationKind::Assign
            | VariableOperationKind::PlusEqual
            | VariableOperationKind::EqualPlus
            | VariableOperationKind::DotEqual
            | VariableOperationKind::EqualDot => 1,
            // ?=
            VariableOperationKind::Default => 2,
            // ??=
            VariableOperationKind::WeakDefault => 3,
            // :remove
            VariableOperationKind::Append => 4,
            VariableOperationKind::SynthesizedAppend => 5,
            // :prepend
            VariableOperationKind::Prepend => 6,
            VariableOperationKind::SynthesizedPrepend => 7,
            // :remove
            VariableOperationKind::Remove => 8,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct VariableOperation {
    pub(crate) op_type: StmtKind,
    pub(crate) idx: NodeIndex<DefaultIx>,
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