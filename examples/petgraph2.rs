use std::cmp::Ordering;
use fxhash::FxHashMap;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::DefaultIx;
use std::collections::BinaryHeap;
use once_cell::sync::Lazy;
use petgraph::dot::{Config, Dot};
use petgraph::prelude::StableGraph;
use regex::{Match, Regex};

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());
static SETVAR_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(?P<base>.*?)(?P<keyword>:append|:prepend|:remove)?(?P<add>:.*)?$")
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

    fn internalize_expression<S: Into<String>>(&mut self, value: S) -> NodeIndex<DefaultIx> {
        let value = value.into();

        self.ds.add_node(GraphItem::ExpressionNode(ExpressionNode::Concatenate))
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

        let var_without_keyword = format!("{}{}", base, regex_match.name("add").map(|m| m.as_str()).unwrap_or_default());

        let stmt_idx = self.ds.add_node(GraphItem::StmtNode(StmtNode {
            lhs: var_without_keyword,
            kind: stmt_kind
        }));

        // TODO: parse expression
        let expr_idx = self.internalize_expression(value);

        let var_entry = self.vars.entry(base.to_string()).or_insert_with(|| {
            self.ds.add_node(GraphItem::new_variable(base))
        });

        let var_data = self.ds.node_weight_mut(*var_entry).unwrap().variable_mut();

        var_data.operations.push(VariableOperation {
            op_type: StmtKind::Assign,
            idx: stmt_idx,
        });

        self.ds.add_edge(stmt_idx, expr_idx, ());
        self.ds.add_edge(*var_entry, stmt_idx, ());

        //
        // // Check for append/prepend/remove operation
        // if let Some(regex_match) = SETVAR_REGEX.captures(&var) {
        //     // Base variable name, possibly with its own overrides. For example:
        //     // P:class-target:append:arm yields:
        //     //      base: P:class-target
        //     //      keyword: :append
        //     //      overrides: arm
        //     let base = regex_match.name("base").unwrap().as_str();
        //     let keyword = regex_match.name("keyword").unwrap().as_str();
        //     let overridestr = regex_match.name("add").map(|o| o.as_str().to_string());
        //
        //     let base_variable_index = self._get_or_create_var(var_name.clone());
        //     let override_ = overridestr.map(|s| self._intern_expression(s));
        //     let value = self._intern_expression(value.clone());
        //     let base_variable_data = self.ds.node_weight_mut(base_variable_index).unwrap();
        //
        //     let GraphItem::Variable(var) = base_variable_data else { panic!(); };
        //     match keyword {
        //         "_append" => {
        //             var.appends.push(Apr { override_, value });
        //         }
        //         "_prepend" => {
        //             var.prepends.push(Apr { override_, value });
        //         }
        //         "_remove" => {
        //             var.removes.push(Apr { override_, value });
        //         }
        //         _ => unreachable!()
        //     }
        // }
    }
}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>,
}

#[derive(Debug)]
enum ExpressionNode {
    Concatenate,
    GetVariable(String),
    Constant(String),
}

#[derive(Debug)]
struct StmtNode {
    kind: StmtKind,
    lhs: String,
}

#[derive(Debug)]
enum GraphItem {
    Variable(Variable),
    ExpressionNode(ExpressionNode),
    StmtNode(StmtNode),
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

    d.set_var("FILES:append:lib${BPN}z", "${libdir}/ok.so");

    println!("{:?}", Dot::with_config(&d.ds, &[]));
}