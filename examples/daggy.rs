use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use petgraph::stable_graph::{DefaultIx, NodeIndex};
use regex::Regex;

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());
static SETVAR_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(?P<base>.*?)(?P<keyword>_append|_prepend|_remove)(?:_(?P<add>[^A-Z]*))?$")
        .unwrap()
});
static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\s").unwrap());

#[derive(Debug)]
struct Cell {
    cached_value: Option<String>,
    value: String,
    var_name: String,
    flags: FxHashMap<String, NodeIndex<DefaultIx>>,
}

impl Cell {
    fn new(var_name: impl Into<String> + Clone, value: impl Into<String>) -> Self {
        Self { cached_value: None, value: value.into(), var_name: var_name.into(), flags: FxHashMap::default() }
    }
}

#[derive(Debug)]
struct DataSmart {
    ds: petgraph::stable_graph::StableGraph<Cell, u8>,
    nodes: FxHashMap<String, NodeIndex<DefaultIx>>,
}

impl DataSmart {
    fn new() -> Self {
        Self {
            ds: petgraph::stable_graph::StableGraph::new(),
            nodes: FxHashMap::default(),
        }
    }

    fn set_var(&mut self, var_name: impl Into<String> + Clone, value: impl Into<String>) {
        let value = value.into();
        let var_name = var_name.into();

        // TODO lookup existing
        assert!(!self.nodes.contains_key(var_name.as_str()));
        let node = self.ds.add_node(Cell::new(var_name.clone(), value.clone()));
        self.nodes.insert(var_name.into(), node);

        self.add_variable_edges(node, value);
    }

    fn set_var_flag(&mut self, var_name: impl Into<String> + Clone, flag: impl Into<String>, value: impl Into<String>) {}

    fn add_variable_edges(&mut self, node: NodeIndex<DefaultIx>, value: impl Into<String>) {
        let value = value.into();
        for caps in VAR_EXPANSION_REGEX.captures_iter(value.as_str()) {
            let match_str = caps.get(0).unwrap().as_str();
            let referenced_var = &match_str[2..match_str.len() - 1];
            dbg!(referenced_var);

            if let Some(referenced_node) = self.nodes.get(referenced_var) {
                self.ds.add_edge(*referenced_node, node, 0);
            } else {
                let referenced_node = self.ds.add_node(Cell::new(referenced_var, ""));
                self.nodes.insert(referenced_var.to_string(), referenced_node);
                self.ds.add_edge(referenced_node, node, 0);
            }
        }
    }
}

fn main() {
    let mut ds = DataSmart::new();
    ds.set_var("B", "C");
    ds.set_var("A", "${B}");
    ds.set_var("Q", "${D} ${P} ok");

    dbg!(ds);
}
