use crate::variable_operation::{NormalOperator, Operator, OverrideOperator};
use crate::variable_parser::VariableExpressionKind::{Assignment, OverrideOperation};
use indexmap::IndexSet;
use itertools::Itertools;
use regex::Regex;
use std::borrow::Cow;
use std::sync::LazyLock;

static BITBAKE_OVERRIDE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[a-z0-9]+").unwrap());

static SETVAR_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(?P<base>.*?)(?P<keyword>:append|:prepend|:remove)(?::(?P<add>[^A-Z]*))?$")
        .unwrap()
});

/// Statement parsing resolves the kind of variable expression (LHS), which also has an effect on the
/// recorded value (RHS), depending on combinations of operators.
#[derive(Debug, Eq, PartialEq)]
pub(crate) struct StatementNode2 {
    pub(crate) lhs: VariableExpression,
    pub(crate) operator: NormalOperator,
    /// Value assigned in the statement, possibly modified depending on combinations of operators.
    pub(crate) rhs: String,
    /// Raw value, without taking into account operators
    pub(crate) raw_rhs: String,
}

impl StatementNode2 {
    // The operator, used for fifo heap ordering
    pub(crate) fn resolved_operator(&self) -> Operator {
        match &self.lhs.kind {
            OverrideOperation { operator, .. } => (*operator).into(),
            Assignment { .. } => self.operator.into(),
        }
    }

    pub(crate) fn is_override_operation(&self) -> bool {
        matches!(&self.lhs.kind, OverrideOperation { .. })
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) struct VariableExpression {
    pub(crate) var_base: String,
    pub(crate) kind: VariableExpressionKind,
}

impl VariableExpression {
    pub(crate) fn override_string(&self) -> String {
        self.kind.override_string()
    }

    pub(crate) fn override_scope(&self) -> &[String] {
        self.kind.override_scope()
    }

    pub(crate) fn replace_assignment_prefix(
        &self,
        old: &VariableExpression,
        new: &VariableExpression,
    ) -> Option<VariableExpression> {
        let Assignment { scope: old_scope } = &old.kind else {
            return None;
        };
        if self.var_base != old.var_base || !self.override_scope().starts_with(old_scope) {
            return None;
        }

        let trailing_scope = &self.override_scope()[old_scope.len()..];
        let kind = match (&self.kind, &new.kind) {
            (Assignment { .. }, Assignment { scope }) => Assignment {
                scope: scope.iter().chain(trailing_scope).cloned().collect(),
            },
            (
                Assignment { .. },
                OverrideOperation {
                    scope,
                    operator,
                    filter,
                },
            ) => OverrideOperation {
                scope: scope.clone(),
                operator: *operator,
                filter: filter.iter().chain(trailing_scope).cloned().collect(),
            },
            (
                OverrideOperation {
                    operator, filter, ..
                },
                Assignment { scope },
            ) => OverrideOperation {
                scope: scope.iter().chain(trailing_scope).cloned().collect(),
                operator: *operator,
                filter: filter.clone(),
            },
            (
                OverrideOperation {
                    operator, filter, ..
                },
                OverrideOperation { scope, .. },
            ) => OverrideOperation {
                scope: scope.iter().chain(trailing_scope).cloned().collect(),
                operator: *operator,
                filter: filter.clone(),
            },
        };

        Some(VariableExpression {
            var_base: new.var_base.clone(),
            kind,
        })
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum VariableExpressionKind {
    /// e.g. B:scope:append:filter = "V"
    OverrideOperation {
        scope: Vec<String>,
        /// The override operator embedded in the variable expression - different from the
        /// operator of the parent statement, which is =, +=, =+, etc.
        operator: OverrideOperator,
        /// Sequence conforming to [^A-Z]*
        /// In real bitbake, this is called the 'add' group in __setvar_regexp__.
        filter: IndexSet<String>,
    },
    /// e.g. B = "V"
    ///      B:scope = "V"
    // TODO: rename to normal override or something? It is also an override
    Assignment {
        // Sequence of override strings, conforming to [a-z0-9]+
        scope: Vec<String>,
    },
}

impl VariableExpressionKind {
    pub(crate) fn is_active(
        &self,
        override_selection_context: &Cow<IndexSet<String>>,
        active_overrides: &Cow<IndexSet<String>>,
    ) -> bool {
        match &self {
            OverrideOperation { filter, scope, .. } => {
                let scope_set: IndexSet<String> = scope.iter().cloned().collect();

                // For scope, consider selection context (active set + direct variant lookup)
                let lhs_valid = scope_set.is_subset(override_selection_context);

                // For filter, consider active override set
                let rhs_valid = filter.is_subset(active_overrides);

                rhs_valid && lhs_valid
            }
            Assignment { scope } => {
                let scope_set: IndexSet<String> = scope.iter().cloned().collect();
                scope_set.is_subset(override_selection_context)
            }
        }
    }

    pub(crate) fn override_scope(&self) -> &[String] {
        match self {
            OverrideOperation { scope, .. } => scope.as_slice(),
            Assignment { scope } => scope.as_slice(),
        }
    }

    pub(crate) fn override_string(&self) -> String {
        match self {
            OverrideOperation {
                scope,
                operator,
                filter,
            } => {
                let mut parts: Vec<String> = vec![];
                parts.extend(scope.iter().cloned());
                match operator {
                    OverrideOperator::Append => parts.push("append".to_string()),
                    OverrideOperator::Prepend => parts.push("prepend".to_string()),
                    OverrideOperator::Remove => parts.push("remove".to_string()),
                }
                parts.extend(filter.iter().cloned());
                parts.iter().join(":")
            }
            Assignment { scope } => scope.join(":"),
        }
    }
}

pub(crate) fn parse_variable<V: AsRef<str>>(var: V) -> VariableExpression {
    let var = var.as_ref();

    if let Some(captures) = SETVAR_REGEX.captures(var) {
        let base = captures.name("base").unwrap().as_str();
        let parts = base.split(':').collect_vec();
        let scope_start = parts
            .iter()
            .enumerate()
            .skip(1)
            .find_map(|(index, part)| BITBAKE_OVERRIDE_REGEX.is_match(part).then_some(index))
            .unwrap_or(parts.len());
        let operator = match captures.name("keyword").unwrap().as_str() {
            ":append" => OverrideOperator::Append,
            ":prepend" => OverrideOperator::Prepend,
            ":remove" => OverrideOperator::Remove,
            _ => unreachable!(),
        };
        let filter = captures
            .name("add")
            .into_iter()
            .flat_map(|value| value.as_str().split(':'))
            .filter(|part| !part.is_empty())
            .map(String::from)
            .collect();

        return VariableExpression {
            var_base: parts[..scope_start].join(":"),
            kind: OverrideOperation {
                scope: parts[scope_start..]
                    .iter()
                    .map(|part| String::from(*part))
                    .collect(),
                operator,
                filter,
            },
        };
    }

    let parts = var.split(':').collect_vec();
    let scope_start = parts[1..]
        .iter()
        .rposition(|part| !BITBAKE_OVERRIDE_REGEX.is_match(part))
        .map_or(1, |index| index + 2);

    VariableExpression {
        var_base: parts[..scope_start].join(":"),
        kind: Assignment {
            scope: parts[scope_start..]
                .iter()
                .map(|part| String::from(*part))
                .collect(),
        },
    }
}

pub(crate) fn parse_statement<V: AsRef<str>>(
    var: V,
    normal_operator: NormalOperator,
    value: String,
) -> Option<StatementNode2> {
    let variable_expression = parse_variable(var);
    let mut cooked_value = value.clone();

    // Need to cook the value (RHS) for certain combinations of operators
    if let OverrideOperation {
        operator: override_operator,
        ..
    } = &variable_expression.kind
    {
        // TODO: BitBake gives a warning when mixing these operators
        match normal_operator {
            NormalOperator::WeakDefault => {
                // In BitBake, remove, append, prepend are implemented as varflags. However, ??=
                // is implemented with the _defaultval varflag. So using ??= causes assignment
                // to the '_defaultval' varflag of the var name, e.g. TEST:remove. This is not
                // observable, since you can't do `getVar("TEST:remove")`. So just ignore.
                return None;
            }
            NormalOperator::PlusEqual if *override_operator != OverrideOperator::Remove => {
                // 'remove' is whitespace delimited, so don't bother adding space.
                cooked_value = format!(" {value}");
            }
            NormalOperator::EqualPlus if *override_operator != OverrideOperator::Remove => {
                // 'remove' is whitespace delimited, so don't bother adding space.
                cooked_value = format!("{value} ");
            }
            _ => { /* everything else is handled no differently */ }
        }
    }

    Some(StatementNode2 {
        lhs: variable_expression,
        rhs: cooked_value,
        operator: normal_operator,
        raw_rhs: value,
    })
}

#[cfg(test)]
mod test {
    use crate::variable_operation::OverrideOperator;
    use crate::variable_parser::{VariableExpression, VariableExpressionKind, parse_variable};
    use bytebraise_util::split::split_filter_empty;
    use pretty_assertions::assert_eq;

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
        // Assignment
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
        // Override operation
        ($var:expr, scope=$scope:expr, $op:tt) => {
            v!($var, scope = $scope, $op, filter = "")
        };
        ($var:expr, $op:tt, filter=$filter:expr) => {
            v!($var, scope = "", $op, filter = $filter)
        };
        ($var:expr, $op:tt) => {
            v!($var, scope = "", $op, filter = "")
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
            parse_variable("ALTERNATIVE:ncurses-tools:class-target"),
            v!("ALTERNATIVE", scope = "ncurses-tools:class-target")
        );
        assert_eq!(
            parse_variable("TEST:some_val"),
            v!("TEST", scope = "some_val")
        );
        assert_eq!(
            parse_variable("VERSION:pn-${PN}"),
            v!("VERSION", scope = "pn-${PN}")
        );
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
        assert_eq!(
            parse_variable("A:append:X:prepend:y"),
            v!("A", scope = "append:X", prepend, filter = "y")
        );
        assert_eq!(parse_variable("A:append:"), v!("A", append));
        assert_eq!(
            parse_variable("TEST:${PN}:append:pn-gizmo-${MACHINE}"),
            v!("TEST:${PN}", scope = "append:pn-gizmo-${MACHINE}")
        );
        assert_eq!(
            parse_variable("TEST:gizmo:append:pn-gizmo-qemux86"),
            v!("TEST", scope = "gizmo", append, filter = "pn-gizmo-qemux86")
        );

        assert_eq!(
            parse_variable("A:A:t:p:${P}:t:p"),
            v!("A:A:t:p:${P}", scope = "t:p")
        );
        assert_eq!(
            parse_variable("A:A:t:p:${p}:t:p"),
            v!("A:A:t:p:${p}", scope = "t:p")
        );

        assert_eq!(
            parse_variable("B:a:${Q}:append:${P}"),
            v!("B:a:${Q}:append:${P}")
        );
        assert_eq!(
            parse_variable("B:a:${q}:append:${P}"),
            v!("B:a:${q}:append:${P}")
        );
        assert_eq!(
            parse_variable("B:a:${q}:t:append:${P}:p"),
            v!("B:a:${q}:t:append:${P}", scope = "p")
        );

        assert_eq!(
            parse_variable("B:a:${q}:t:append:${p}:p"),
            v!("B", scope = "a:${q}:t", append, filter = "${p}:p")
        );
    }

    #[test]
    fn replace_assignment_prefix() {
        let old = parse_variable("TEST:a");

        assert_eq!(
            parse_variable("TEST:a:b").replace_assignment_prefix(&old, &parse_variable("WAT:t")),
            Some(parse_variable("WAT:t:b"))
        );
        assert_eq!(
            parse_variable("TEST:a:b")
                .replace_assignment_prefix(&old, &parse_variable("WAT:t:append:q")),
            Some(parse_variable("WAT:t:append:q:b"))
        );
        assert_eq!(
            parse_variable("TEST:a:b:prepend:q")
                .replace_assignment_prefix(&old, &parse_variable("WAT:t")),
            Some(parse_variable("WAT:t:b:prepend:q"))
        );
        assert_eq!(
            parse_variable("OTHER:a:b").replace_assignment_prefix(&old, &parse_variable("WAT:t")),
            None
        );
    }
}
