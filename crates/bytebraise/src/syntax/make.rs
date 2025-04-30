// This module adapted from Rust Analyzer (https://github.com/rust-analyzer/rust-analyzer), which
// is under MIT license.

use crate::parser::parse_bitbake_from_str;
use crate::syntax::ast::nodes::Assignment;
use crate::syntax::ast::quoted_value::QuotedValue;
use crate::syntax::ast::AstNode;
use itertools::Itertools;

pub fn ast_from_text<N: AstNode>(text: &str) -> N {
    let parse = parse_bitbake_from_str(text);

    let node = match parse.syntax().descendants().find_map(N::cast) {
        Some(it) => it,
        None => {
            panic!(
                "Failed to make ast node `{}` from text {}",
                std::any::type_name::<N>(),
                text
            )
        }
    };
    let node = node.clone_subtree();
    assert_eq!(node.syntax().text_range().start(), 0.into());
    node
}

pub fn assignment(var: String, op: String, value: String) -> Assignment {
    ast_from_text::<Assignment>(&format!("{var} {op} {value}")).clone_for_update()
}

pub fn quoted_value(text: String) -> QuotedValue {
    // TODO: quote character
    let input = format!("A = \"{text}\"");
    let assignment: Assignment = ast_from_text(&input);
    assignment.clone_for_update().right()
}

// TODO: format
pub fn quoted_value_from_slice(vals: &[String]) -> QuotedValue {
    match vals.len() {
        0 => quoted_value(String::new()),
        1 => quoted_value(vals[0].clone()),
        _ => {
            let text = format!(
                " \\\n{} \\\n",
                vals.iter().map(|v| format!("    {v}")).join(" \\\n")
            );
            quoted_value(text)
        }
    }
}
