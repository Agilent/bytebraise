/*
use crate::ast::assignment::{AssignmentExpression, AssignmentOperator, ExportStatement};
use crate::ast::include::{Include, InheritDirective};
use crate::ast::unit::AddTaskDirective;
use crate::ast::{ExpressionNode, Identifier, IdentifierExpression, NodeList};
use crate::parser::mock::{assert_units, Mock};

#[test]
fn basic_assignment() {
    let m = Mock::new();

    assert_units(
        r#"
A = "test"
        "#,
        [m.node(
            1,
            11,
            AssignmentExpression {
                is_exported: false,
                left: m.node(
                    1,
                    2,
                    IdentifierExpression {
                        varflag: None,
                        ident: m.node(1, 2, "A"),
                    },
                ),
                operator: m.node(3, 4, AssignmentOperator::Equals),
                right: m.node(5, 11, "test"),
            },
        )],
    );
}

#[test]
fn basic_assignment_with_varflag() {
    let m = Mock::new();

    assert_units(
        r#"
A[my_flag] = "test value"
        "#,
        [m.node(
            1,
            26,
            AssignmentExpression {
                is_exported: false,
                left: m.node(
                    1,
                    11,
                    IdentifierExpression {
                        varflag: m.node(2, 11, "my_flag"),
                        ident: m.node(1, 2, "A"),
                    },
                ),
                operator: m.node(12, 13, AssignmentOperator::Equals),
                right: m.node(14, 26, "test value"),
            },
        )],
    );
}

#[test]
fn export_keyword() {
    let m = Mock::new();

    assert_units(
        r#"
export WAT_DO_YOU_SAY
export BUDDY
        "#,
        [
            m.node(
                1,
                22,
                ExportStatement {
                    ident: m.node(
                        8,
                        22,
                        IdentifierExpression {
                            varflag: None,
                            ident: m.node(8, 22, "WAT_DO_YOU_SAY"),
                        },
                    ),
                },
            ),
            m.node(
                23,
                35,
                ExportStatement {
                    ident: m.node(
                        30,
                        35,
                        IdentifierExpression {
                            varflag: None,
                            ident: m.node(30, 35, "BUDDY"),
                        },
                    ),
                },
            ),
        ],
    );
}

#[test]
fn export_assignment() {
    let m = Mock::new();

    assert_units(
        r##"
export WAT_DO_YOU_SAY = "contents ${"buddy"}"
include OK buddy then
        "##,
        [
            m.node(
                1,
                46,
                AssignmentExpression {
                    is_exported: true,
                    left: m.node(
                        8,
                        22,
                        IdentifierExpression {
                            varflag: None,
                            ident: m.node(8, 22, "WAT_DO_YOU_SAY"),
                        },
                    ),
                    operator: m.node(23, 24, AssignmentOperator::Equals),
                    right: m.node(25, 46, "contents ${\"buddy\"}"),
                },
            ),
            m.node(
                47,
                77,
                Include {
                    right: m.node(55, 68, "OK buddy then"),
                    is_require: false,
                },
            ),
        ],
    );
}

#[test]
fn addtask() {
    let m = Mock::new();

    assert_units(
        "addtask do_compile",
        [m.node(
            0,
            18,
            AddTaskDirective {
                task_name: m.node(8, 18, "do_compile"),
                after: None,
                before: None,
            },
        )],
    );

    assert_units(
        "addtask do_compile after do_configure",
        [m.node(
            0,
            37,
            AddTaskDirective {
                task_name: m.node(8, 18, "do_compile"),
                after: Some(m.list([m.node(25, 37, "do_configure")])),
                before: None,
            },
        )],
    );

    assert_units(
        "addtask do_patch before do_build",
        [m.node(
            0,
            32,
            AddTaskDirective {
                task_name: m.node(8, 16, "do_patch"),
                after: None,
                before: Some(m.list([m.node(24, 32, "do_build")])),
            },
        )],
    );

    assert_units(
        "addtask do_compile after do_fetch do_unpack before do_install do_populate_sysroot",
        [m.node(
            0,
            81,
            AddTaskDirective {
                task_name: m.node(8, 18, "do_compile"),
                after: Some(m.list([m.node(25, 33, "do_fetch"), m.node(34, 43, "do_unpack")])),
                before: Some(m.list([
                    m.node(51, 61, "do_install"),
                    m.node(62, 81, "do_populate_sysroot"),
                ])),
            },
        )],
    );
}

#[test]
fn addtask_line_continuations() {
    let m = Mock::new();

    assert_units(
        r#"addtask \
    do_compile"#,
        [m.node(
            0,
            24,
            AddTaskDirective {
                task_name: m.node(14, 24, "do_compile"),
                after: None,
                before: None,
            },
        )],
    );

    assert_units(
        r#"addtask do_compile after \
do_unpack"#,
        [m.node(
            0,
            36,
            AddTaskDirective {
                task_name: m.node(8, 18, "do_compile"),
                after: Some(m.list([m.node(27, 36, "do_unpack")])),
                before: None,
            },
        )],
    );
}

#[test]
fn inherit_directive() {
    let m = Mock::new();

    assert_units(
        r##"
inherit cmake patch
        "##,
        [m.node(
            1,
            20,
            InheritDirective {
                classes: m.node(9, 20, "cmake patch"),
            },
        )],
    );
}

#[test]
fn parse_maintainers_inc() {
    let m = Mock::new();

    let input = r##"
RECIPE_MAINTAINER_pn-acl = "Chen Qi <Qi.Chen@windriver.com>"
RECIPE_MAINTAINER_pn-acpica = "Ross Burton <ross.burton@intel.com>"
RECIPE_MAINTAINER_pn-acpid = "Ross Burton <ross.burton@intel.com>"
RECIPE_MAINTAINER_pn-adwaita-icon-theme = "Ross Burton <ross.burton@intel.com>"
"##;

    assert_units(
        input,
        [
            m.node(
                1,
                61,
                AssignmentExpression {
                    is_exported: false,
                    left: m.node(
                        1,
                        25,
                        IdentifierExpression {
                            varflag: None,
                            ident: m.node(1, 25, "RECIPE_MAINTAINER_pn-acl"),
                        },
                    ),
                    operator: m.node(26, 27, AssignmentOperator::Equals),
                    right: m.node(28, 61, "Chen Qi <Qi.Chen@windriver.com>"),
                },
            ),
            m.node(
                62,
                129,
                AssignmentExpression {
                    is_exported: false,
                    left: m.node(
                        62,
                        89,
                        IdentifierExpression {
                            varflag: None,
                            ident: m.node(62, 89, "RECIPE_MAINTAINER_pn-acpica"),
                        },
                    ),
                    operator: m.node(90, 91, AssignmentOperator::Equals),
                    right: m.node(92, 129, "Ross Burton <ross.burton@intel.com>"),
                },
            ),
            m.node(
                130,
                196,
                AssignmentExpression {
                    is_exported: false,
                    left: m.node(
                        130,
                        156,
                        IdentifierExpression {
                            varflag: None,
                            ident: m.node(130, 156, "RECIPE_MAINTAINER_pn-acpid"),
                        },
                    ),
                    operator: m.node(157, 158, AssignmentOperator::Equals),
                    right: m.node(159, 196, "Ross Burton <ross.burton@intel.com>"),
                },
            ),
            m.node(
                197,
                276,
                AssignmentExpression {
                    is_exported: false,
                    left: m.node(
                        197,
                        236,
                        IdentifierExpression {
                            varflag: None,
                            ident: m.node(197, 236, "RECIPE_MAINTAINER_pn-adwaita-icon-theme"),
                        },
                    ),
                    operator: m.node(237, 238, AssignmentOperator::Equals),
                    right: m.node(239, 276, "Ross Burton <ross.burton@intel.com>"),
                },
            ),
        ],
    );
}
*/
