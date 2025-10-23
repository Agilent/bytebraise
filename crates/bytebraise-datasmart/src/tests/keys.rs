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
}

#[test]
fn basic_keys_2() {
    let mut d = eval(
        r#"
TEST:${B} = "WAT"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["TEST:${B}"]);
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

    let mut d = eval(
        r#"
B:c:d = "test"
B:c:${Q}:p:d = "test"
OVERRIDES = "d"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(
        keys,
        vec!["B:c", "B:c:${Q}:p", "B:c:${Q}:p:d", "B:c:d", "OVERRIDES"]
    );
}

#[test]
fn operators_1() {
    let mut d = eval(
        r#"
B:append = "A"
B = "Q"
B:append:q = "T"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B"]);
}

#[test]
fn operators_2() {
    let mut d = eval(
        r#"
B = "A"
B:a = "Q"
B:a:b:append = "T"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B", "B:a", "B:a:b"]);
}

#[test]
fn operators_3() {
    let mut d = eval(
        r#"
B = "A"
B:a:${Q}:append = "Q"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B", "B:a:${Q}"]);
}

#[test]
fn operators_4() {
    let mut d = eval(
        r#"
B = "A"
B:a:${Q}:append = "Q"
B:a:${Q}:append:t = "T"
B:a:${Q}:t:g:b:append:t:p = "D"
OVERRIDES = "b"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(
        keys,
        vec![
            "B",
            "B:a:${Q}",
            "B:a:${Q}:t:g",
            "B:a:${Q}:t:g:b",
            "OVERRIDES"
        ]
    );

    let mut d = eval(
        r#"
B = "A"
B:a:${Q}:append = "Q"
B:a:${Q}:append:t = "T"
B:a:${Q}:t:g:b:append:t:p = "D"
OVERRIDES = "b:g"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(
        keys,
        vec![
            "B",
            "B:a:${Q}",
            "B:a:${Q}:t",
            "B:a:${Q}:t:g",
            "B:a:${Q}:t:g:b",
            "OVERRIDES"
        ]
    );
}

#[test]
fn decompose_operators_1() {
    let mut d = eval(
        r#"
B = "A"
B:a:${Q}:append:${P} = "Q"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B", "B:a:${Q}:append:${P}"]);
}
