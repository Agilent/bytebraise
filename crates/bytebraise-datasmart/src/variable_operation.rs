use derive_more::From;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::DefaultIx;
use std::cmp::Ordering;

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum NormalOperator {
    WeakDefault,
    Default,
    Assign,
    PlusEqual,
    EqualPlus,
    DotEqual,
    EqualDot,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum OverrideOperator {
    Append,
    Prepend,
    Remove,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash, From)]
pub enum Operator {
    Normal(NormalOperator),
    Override(OverrideOperator),
}

impl Operator {
    pub fn is_override_operator(&self) -> bool {
        matches!(self, Self::Override(_))
    }

    fn order_value(&self) -> u8 {
        match self {
            Self::Normal(normal) => match normal {
                NormalOperator::Assign
                | NormalOperator::PlusEqual
                | NormalOperator::EqualPlus
                | NormalOperator::DotEqual
                | NormalOperator::EqualDot => 1,
                // ?=
                NormalOperator::Default => 2,
                // ??=
                NormalOperator::WeakDefault => 3,
            },
            Self::Override(op) => match op {
                // :remove
                OverrideOperator::Append => 4,
                // :prepend
                OverrideOperator::Prepend => 5,
                // :remove
                OverrideOperator::Remove => 6,
            },
        }
    }
}

impl Ord for Operator {
    fn cmp(&self, other: &Self) -> Ordering {
        self.order_value().cmp(&other.order_value())
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct VariableOperation {
    pub(crate) op_type: Operator,
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
