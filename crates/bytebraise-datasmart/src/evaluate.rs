use crate::errors::DataSmartResult;
use crate::petgraph2::DataSmart;
use bytebraise_syntax::parser::parse_bitbake_from_str;
use bytebraise_syntax::syntax::ast::AstToken;
use bytebraise_syntax::syntax::ast::nodes::{Assignment, Root, RootItem};
use bytebraise_syntax::syntax::syntax_kind::SyntaxKind;

pub fn eval<D: AsRef<str>>(data: D) -> DataSmart {
    let data = data.as_ref();
    let parsed = parse_bitbake_from_str(data);

    println!("{:#?}", &parsed);

    let mut d = DataSmart::new();
    parsed.evaluate(&mut d).unwrap();
    d
}

pub trait Evaluate {
    fn evaluate(&self, d: &mut DataSmart) -> DataSmartResult<()>;
}

impl Evaluate for Root {
    fn evaluate(&self, d: &mut DataSmart) -> DataSmartResult<()> {
        for item in self.items() {
            item.evaluate(d)?;
        }

        Ok(())
    }
}

impl Evaluate for RootItem {
    fn evaluate(&self, d: &mut DataSmart) -> DataSmartResult<()> {
        match self {
            RootItem::Assignment(e) => e.evaluate(d),
            _ => unimplemented!("{:?}", self),
        }
    }
}

impl Evaluate for Assignment {
    fn evaluate(&self, d: &mut DataSmart) -> DataSmartResult<()> {
        evaluate_assignment_expression(d, self, false)
    }
}

fn evaluate_assignment_expression(
    data: &mut DataSmart,
    expr: &Assignment,
    is_exported: bool,
) -> DataSmartResult<()> {
    let ident = expr.left();
    let key = ident.identifier().syntax().text().to_string();
    let varflag_text = ident.varflag().map(|v| v.value().to_string());
    if varflag_text.is_some() {
        unimplemented!();
    }

    let assigned_value = expr.right().value().to_string();

    if is_exported {
        unimplemented!();
        // TODO literal 1, not string "1"
        // data.set_var_flag(&key, "export", "1")?;
    }

    match expr.op().syntax().kind() {
        SyntaxKind::Equals => data.set_var(key, assigned_value),
        SyntaxKind::WeakEquals => data.weak_default_var(key, assigned_value),
        SyntaxKind::DefaultEquals => data.default_var(key, assigned_value),
        SyntaxKind::EqualsDot => data.equals_dot_var(key, assigned_value),
        SyntaxKind::DotEquals => data.dot_equals_var(key, assigned_value),
        SyntaxKind::PlusEquals => data.plus_equals_var(key, assigned_value),
        SyntaxKind::EqualsPlus => data.equals_plus_var(key, assigned_value),
        SyntaxKind::ColonEquals => {
            unimplemented!("TODO :=");
            //let current_value = data.get_var(&key);
            //data.set_var(key, current_value);
        }
        _ => unreachable!(),
    };

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::evaluate::eval;
    use crate::macros::get_var;

    #[test]
    fn test_eval_basic() {
        let d = eval(
            r#"
A = "a"
B = "c"
        "#,
        );

        assert_eq!(get_var!(&d, "A").unwrap(), "a");
    }

    #[test]
    fn test_operators() {
        let d = eval(
            r#"
A ?= "a"
B ?= "c"
A = "c"
A:append = "1"
        "#,
        );

        assert_eq!(get_var!(&d, "A").unwrap(), "c1");
    }
}
