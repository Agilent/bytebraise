use crate::evaluate::eval;

// TODO: bitbake stores TEST:${B} as its own variable, whereas we store TEST with a ${B} operation.
//   Do we need to replicate bitbake's behavior internally?
//

#[test]
fn basic_keys_1() {
    let mut d = eval(
        r#"
TEST:${B} = "WAT"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["TEST:${B}"]);
    todo!();
    // todo check keys
}

#[test]
fn decompose_overrides_1() {
    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q} = "test"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:c:${Q}", "B:c:d"]);

    // When overrides are active, the story changes:
    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q} = "test"
OVERRIDES = "c:d"
"#,
    );

    let keys = d.get_all_keys();
    // This is because both "B:c" and "B" have values
    assert_eq!(keys, vec!["B", "B:c", "B:c:${Q}", "B:c:d", "OVERRIDES"]);

    // Also:
    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q} = "test"
OVERRIDES = "d"
"#,
    );

    let keys = d.get_all_keys();
    // B:c has a value, but not B (since 'c' is not active)
    assert_eq!(keys, vec!["B:c", "B:c:${Q}", "B:c:d", "OVERRIDES"]);

    // And:
    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q} = "test"
OVERRIDES = "c"
"#,
    );

    let keys = d.get_all_keys();
    // Neither B nor B:c has a value, since 'd' is not active
    assert_eq!(keys, vec!["B:c:${Q}", "B:c:d", "OVERRIDES"]);
}

#[test]
fn decompose_overrides_2() {
    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q} = "test"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:c:${Q}", "B:c:d"]);

    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q} = "test"
OVERRIDES = "c"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:c:${Q}", "B:c:d", "OVERRIDES"]);

    let mut d = eval(
        r#"
B:c:d = "test"
# In BitBake, overrides internally are purely [A-Z] characters, so :${Q} is not recorded as an override
B:c:${Q} = "test"
# tricky...
OVERRIDES = "${Q}"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:c:${Q}", "B:c:d", "OVERRIDES"]);
}

#[test]
fn decompose_overrides_3() {
    // But ${Q} as override doesn't result in B:c being valid:
    // TODO explain why
    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q} = "test"
OVERRIDES = "c:${Q}"
"#,
    );

    let keys = d.get_all_keys();
    // Neither B nor B:c has a value, since 'd' is not active
    assert_eq!(keys, vec!["B:c:${Q}", "B:c:d", "OVERRIDES"]);
}
