use std::cmp::Ordering;
use std::collections::BinaryHeap;

use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableGraph;
use petgraph::stable_graph::DefaultIx;
use regex::Regex;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub value); // synthesized by LALRPOP


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
    heap: BinaryHeap<(T, usize)>,
}

// TODO: An 'assign' may actually act as an 'append' (or others) e.g.
//  P = ""
//  P:test = "append"
//  A:${P:test} = "OK2"
//
//  BUT thankfully we don't need to factor this into execution operation of statements. i.e.
//  our binary heap approach still works :)
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

        let caps = VAR_EXPANSION_REGEX.captures_iter(&value);
        for cap in caps {
            //println!("{:?}", cap);
        }

        let left = self.ds.add_node(GraphItem::ExpressionNode(ExpressionNode::GetVariable("${libdir}".into())));
        let right = self.ds.add_node(GraphItem::ExpressionNode(ExpressionNode::Constant("/ok.so".into())));

        let concat_node = self.ds.add_node(GraphItem::ExpressionNode(ExpressionNode::Concatenate(vec![left, right])));

        self.ds.add_edge(concat_node, left, ());
        self.ds.add_edge(concat_node, right, ());

        concat_node
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
        }));

        // TODO: parse expression
        let expr_idx = self.internalize_expression(value);

        let var_entry = self.vars.entry(base.to_string()).or_insert_with(|| {
            self.ds.add_node(GraphItem::new_variable(base))
        });

        let var_data = self.ds.node_weight_mut(*var_entry).unwrap().variable_mut();

        var_data.operations.push(VariableOperation {
            op_type: stmt_kind,
            idx: stmt_idx,
        });

        self.ds.add_edge(stmt_idx, expr_idx, ());
        self.ds.add_edge(*var_entry, stmt_idx, ());
    }

    pub fn get_var<S: AsRef<str>>(&self, var: S) -> Option<String> {
        let var = var.as_ref();

        let var_entry = self.vars.get(var)?;

        None
    }
}

#[derive(Debug)]
struct Variable {
    name: String,
    operations: FifoHeap<VariableOperation>,
}

#[derive(Debug)]
enum ExpressionNode {
    Concatenate(Vec<NodeIndex<DefaultIx>>),
    GetVariable(String),
    Constant(String),
    Python(String),
    // e.g. ${${A}}
    Indirection(NodeIndex<DefaultIx>),
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

use crate::value::ExprParser;

fn main() {
    let mut d = DataSmart::new();

    d.set_var("FILES", "");
    d.set_var("FILES:append:lib${BPN}z", "${libdir}/ok.so");

    //println!("{:?}", d.get_var("FILES"));
    let expr = ExprParser::new()
        .parse("22 * 44 + 66")
        .unwrap();
    eprintln!("{:#?}", expr);
    assert_eq!(&format!("{:?}", expr), "((22 * 44) + 66)");

    println!("{:?}", Dot::with_config(&d.ds, &[]));
}