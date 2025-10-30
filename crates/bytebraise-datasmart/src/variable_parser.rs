use crate::variable_operation::{NormalOperator, Operator, OverrideOperator};
use crate::variable_parser::VariableExpressionKind::{Assignment, OverrideOperation};
use indexmap::IndexSet;
use itertools::Itertools;
use regex::Regex;
use std::sync::LazyLock;

static BITBAKE_OVERRIDE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[a-z0-9]+$").unwrap());

static OVERRIDE_STR_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[^A-Z]*$").unwrap());

static KEYWORD_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?:^|:)(?P<keyword>append|prepend|remove)(?:$|:)").unwrap());

/// Statement parsing resolves the kind of variable expression (LHS), which also has an effect on the
/// recorded value (RHS), depending on combinations of operators.
/// TODO: don't bother modifying the value during parsing - instead handle it in get_var?
pub(crate) struct ParsedStatement {
    var_base: String,
    statement: StatementNode2,
}

pub(crate) struct StatementNode2 {
    pub(crate) lhs: VariableExpression,
    pub(crate) operator: Operator,
    /// The value assigned in the statement
    pub(crate) rhs: String,
}

#[derive(Eq, PartialEq, Debug)]
pub(crate) struct VariableExpression {
    var_base: String,
    kind: VariableExpressionKind,
}

#[derive(Eq, PartialEq, Debug)]
pub(crate) enum VariableExpressionKind {
    /// e.g. B:append:a = "V"
    OverrideOperation {
        scope: Vec<String>,
        operator: OverrideOperator,
        // Sequence conforming to [^A-Z]*
        filter: IndexSet<String>,
    },
    /// e.g. B = "V"
    ///      B:a = "V"
    Assignment {
        // Sequence of override strings, conforming to [a-z0-9]+
        scope: Vec<String>,
    },
}

// pub(crate) struct DataStatement {
//     var_base: String,
//     kind: DataStatementKind,
// }
//
// pub(crate) enum DataStatementKind {
//     OverrideOperation {
//         scope: Vec<String>,
//         operator: OverrideOperator,
//         // Sequence conforming to [^A-Z]*
//         filter: IndexSet<String>,
//     },
//     Assignment {
//         operator: NormalOperator,
//         scope: Vec<String>,
//     }
// }
//

pub(crate) fn parse_variable<V: AsRef<str>>(var: V) -> VariableExpression {
    let var = var.as_ref();
    eprintln!("input: {var}");

    let mut parts = var.split(':');

    // Base has at least one part. It has more parts in cases like:
    //   A:B = "V"
    // since :B is not a valid override str.
    //
    let mut i = 0;
    let base_parts = parts
        .take_while_ref(|part| {
            let ret = i == 0 || !BITBAKE_OVERRIDE_REGEX.is_match(part);
            i += 1;
            ret
        })
        .join(":");

    // See if an override operator is amongst the remainder
    let mut remainder = parts.clone();
    if let Some(operator) =
        remainder.position(|part| matches!(part, "remove" | "append" | "prepend"))
    {
        // Check whether all parts after the operator are valid
        if remainder.all(|part| OVERRIDE_STR_REGEX.is_match(part)) {
            // Consume the scope
            let mut scope = parts
                .by_ref()
                .take(operator)
                .map(String::from)
                .collect_vec();

            eprintln!("    valid operator");
            eprintln!("    scope: {}", scope.join(":"));

            // Consume operator
            let mut operator = parts.next().unwrap();
            eprintln!("    operator: {operator}");

            // Consume filter
            let filter = parts.map(String::from).collect();
            eprintln!("    filter: {filter:?}");
            return VariableExpression {
                var_base: base_parts,
                kind: OverrideOperation {
                    scope,
                    operator: match operator {
                        "append" => OverrideOperator::Append,
                        "prepend" => OverrideOperator::Prepend,
                        "remove" => OverrideOperator::Remove,
                        _ => unreachable!(),
                    },
                    filter,
                },
            };
        }
    }

    // For leftover parts, iterate backwards and take override strings as we can. Whatever is left
    // after that is tacked onto the base.
    let mut rparts = parts.rev();
    let scope = rparts
        .take_while_ref(|part| BITBAKE_OVERRIDE_REGEX.is_match(part))
        .map(String::from)
        .collect_vec();
    eprintln!("    scope: {scope:?}");

    let remainder = rparts.rev().join(":");
    eprintln!("    remainder: {remainder}");

    let var_base = if remainder.is_empty() {
        base_parts
    } else {
        format!("{base_parts}:{remainder}")
    };

    VariableExpression {
        var_base,
        kind: Assignment { scope },
    }
}

#[cfg(test)]
mod test {
    use crate::variable_operation::OverrideOperator;
    use crate::variable_parser::{VariableExpression, VariableExpressionKind, parse_variable};
    use indexmap::IndexSet;
    use pretty_assertions::{assert_eq, assert_ne};
    use bytebraise_util::split::split_filter_empty;

    macro_rules! v_operator {
        (append) => {
            OverrideOperator::Append
        };
        (prepend) => {
            OverrideOperator::Prepend
        };
        (remove) => {
            OverrideOperator::Remove
        };
    }

    macro_rules! v {
        ($var:expr) => {
            VariableExpression {
                var_base: String::from($var),
                kind: VariableExpressionKind::Assignment { scope: vec![] },
            }
        };
        ($var:expr, scope=$scope:expr) => {
            VariableExpression {
                var_base: String::from($var),
                kind: VariableExpressionKind::Assignment {
                    scope: split_filter_empty($scope, ":").map(String::from).collect(),
                },
            }
        };
        ($var:expr, scope=$scope:expr, $op:tt) => {
            v!($var, scope = $scope, $op, filter = "")
        };
        ($var:expr, $op:tt, filter=$filter:expr) => {
            v!($var, scope = "", $op, filter = $filter)
        };
        ($var:expr, scope=$scope:expr, $op:tt, filter=$filter:expr) => {
            VariableExpression {
                var_base: String::from($var),
                kind: VariableExpressionKind::OverrideOperation {
                    operator: v_operator!($op),
                    filter: split_filter_empty($filter, ":").map(String::from).collect(),
                    scope: split_filter_empty($scope, ":").map(String::from).collect(),
                },
            }
        };
    }

    #[test]
    fn test1() {
        assert_eq!(parse_variable("A"), v! {"A"});
        assert_eq!(parse_variable("A:A"), v! {"A:A"});
        assert_eq!(parse_variable("A:a"), v!("A", scope = "a"));
        assert_eq!(parse_variable("A:A:a"), v!("A:A", scope = "a"));
        assert_eq!(
            parse_variable("A:A:append:a"),
            v!("A:A", append, filter = "a")
        );
        assert_eq!(
            parse_variable("A:A:a:append:a"),
            v!("A:A", scope = "a", append, filter = "a")
        );
        assert_eq!(
            parse_variable("A:A:a:b:append:a"),
            v!("A:A", scope = "a:b", append, filter = "a")
        );
        assert_eq!(
            parse_variable("A:A:a:b:append:a:b"),
            v!("A:A", scope = "a:b", append, filter = "a:b")
        );
        assert_eq!(parse_variable("A:append:a"), v!("A", append, filter = "a"));
        //
        // assert_eq!(parse_variable("A:A:t:p:${P}:t:p"), "A:A:t:p:${P}");
        // assert_eq!(parse_variable("A:A:t:p:${p}:t:p"), "A:A:t:p:${p}");
    }
}
