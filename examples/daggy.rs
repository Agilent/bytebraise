use fxhash::FxHashMap;
use once_cell::sync::Lazy;
use petgraph::Graph;
use petgraph::stable_graph::{DefaultIx, NodeIndex};
use regex::Regex;

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());
static SETVAR_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(?P<base>.*?)(?P<keyword>_append|_prepend|_remove)(?:_(?P<add>[^A-Z]*))?$")
        .unwrap()
});
static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\s").unwrap());

type Index = NodeIndex<DefaultIx>;

#[derive(Debug)]
struct Apr {
    override_: Option<Index>,
    value: Index,
}

#[derive(Debug)]
struct Variable {
    cached_value: Option<String>,
    value: String,
    flags: FxHashMap<String, NodeIndex<DefaultIx>>,
    appends: Vec<Apr>,
    prepends: Vec<Apr>,
    removes: Vec<Apr>,
}

#[derive(Debug)]
struct FlagOrOverloadValue {
    cached_value: Option<String>,
    value: String,
}

impl FlagOrOverloadValue {
    fn new(value: impl Into<String>) -> Self {
        Self {
            cached_value: None,
            value: value.into(),
        }
    }
}

impl Variable {
    fn new(value: impl Into<String>) -> Self {
        Self {
            cached_value: None,
            value: value.into(),
            flags: FxHashMap::default(),
            appends: Default::default(),
            prepends: Default::default(),
            removes: Default::default(),
        }
    }
}

#[derive(Debug)]
struct DataSmart {
    ds: petgraph::stable_graph::StableGraph<GraphItem, u8>,
    vars: FxHashMap<String, NodeIndex<DefaultIx>>,
    expressions: FxHashMap<String, NodeIndex<DefaultIx>>,
}

#[derive(Debug)]
enum GraphItem {
    Variable(Variable),
    FlagOrOverloadValue(FlagOrOverloadValue),
}

impl DataSmart {
    fn new() -> Self {
        Self {
            ds: petgraph::stable_graph::StableGraph::new(),
            vars: FxHashMap::default(),
            expressions: FxHashMap::default(),
        }
    }

    fn _get_or_create_var(&mut self, var_name: String) -> Index {
        if let Some(index) = self.vars.get(&var_name) {
            *index
        } else {
            let index = self.ds.add_node(GraphItem::Variable(Variable::new("")));
            self.vars.insert(var_name, index);
            index
        }
    }

    #[inline]
    fn _intern_expression(&mut self, value: String) -> Index {
        if let Some(index) = self.expressions.get(&value) {
            *index
        } else {
            let index = self.ds.add_node(GraphItem::FlagOrOverloadValue(FlagOrOverloadValue::new(value.clone())));
            self.expressions.insert(value, index);
            index
        }
    }

    fn set_var(&mut self, var_name: impl Into<String> + Clone, value: impl Into<String>) {
        let var_name = var_name.into();
        let value = value.into();

        if let Some(regex_match) = SETVAR_REGEX.captures(&var_name) {
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
                    var.removes.push( Apr { override_, value });
                }
                _ => unreachable!()
            }

        } else {
            let node = self.ds.add_node(GraphItem::Variable(Variable::new(value.clone())));
            self.vars.insert(var_name.into(), node);
            self.add_variable_edges(node, value);
        }
    }

    fn set_var_flag(&mut self, var_name: impl Into<String> + Clone, flag: impl Into<String>, value: impl Into<String>) {
        let var_name = var_name.into();
        let value = value.into();
        let flag = flag.into();


    }

    fn add_variable_edges(&mut self, node: NodeIndex<DefaultIx>, value: impl Into<String>) {
        let value = value.into();
        for caps in VAR_EXPANSION_REGEX.captures_iter(value.as_str()) {
            let match_str = caps.get(0).unwrap().as_str();
            let referenced_var = &match_str[2..match_str.len() - 1];
            dbg!(referenced_var);

            if let Some(referenced_node) = self.vars.get(referenced_var) {
                self.ds.add_edge(*referenced_node, node, 0);
            } else {
                let referenced_node = self.ds.add_node(GraphItem::Variable(Variable::new("")));
                self.vars.insert(referenced_var.to_string(), referenced_node);
                self.ds.add_edge(referenced_node, node, 0);
            }
        }
    }
}

fn main() {
    let mut ds = DataSmart::new();
    ds.set_var("B", "C");
    ds.set_var("B_append", "OK");

    dbg!(ds);
}
