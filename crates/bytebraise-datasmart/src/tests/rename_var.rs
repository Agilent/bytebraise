#[cfg(test)]
use crate::{evaluate::eval, macros::get_var};

#[test]
fn basic_1() {
    let mut d = eval(
        r#"
TEST = "A"
    "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "A");

    d.rename_var("TEST", "NEW").unwrap();
    assert_eq!(get_var!(&d, "TEST"), None);
    assert_eq!(get_var!(&d, "NEW").unwrap(), "A");
}

#[test]
fn operator_sources_are_not_renamed() {
    for old in [
        "TEST:append",
        "TEST:prepend",
        "TEST:remove",
        "TEST:a:append:b",
        "TEST:a:prepend:b",
        "TEST:a:remove:b",
    ] {
        let mut d = eval(
            r#"
TEST = "one two"
TEST:append = " three"
TEST:prepend = "zero "
TEST:remove = "two"
TEST:a = "scoped"
TEST:a:append:b = " appended"
TEST:a:prepend:b = "prepended "
TEST:a:remove:b = "scoped"
    "#,
        );

        let value = get_var!(&d, "TEST");
        let keys = d.get_all_keys();

        d.rename_var(old, "NEW").unwrap();

        assert_eq!(get_var!(&d, "TEST"), value);
        assert_eq!(get_var!(&d, "NEW"), None);
        assert_eq!(d.get_all_keys(), keys);
    }
}

#[test]
fn rename_preserves_matching_text_in_scope() {
    let mut d = eval(
        r#"
foo:foo = "value"
    "#,
    );

    d.rename_var("foo", "bar").unwrap();

    // BitBake's global str.replace produces bar:bar. Bytebraise only renames the
    // parsed variable base, leaving the semantically separate override scope intact.
    assert_eq!(d.get_all_keys(), vec!["bar:foo"]);
    assert_eq!(get_var!(&d, "bar:foo").unwrap(), "value");
}

#[test]
fn rename_preserves_matching_text_in_operation_scope_and_filter() {
    let mut d = eval(
        r#"
foo = "base"
foo:foo:append:foo = " appended"
OVERRIDES = "foo"
    "#,
    );

    d.rename_var("foo", "bar").unwrap();

    // Unlike BitBake's textual rename, keep the operation's parsed scope and filter;
    // they describe when the operation applies and are not part of the variable name.
    assert_eq!(get_var!(&d, "bar").unwrap(), " appended");
    assert_eq!(d.get_all_keys(), vec!["OVERRIDES", "bar", "bar:foo"]);
}

#[test]
fn rename_to_operation_treats_trailing_scope_as_filter() {
    for (operator, value, inactive, active) in [
        ("append", " three", "one two", "one two three"),
        ("prepend", "zero ", "one two", "zero one two"),
        ("remove", "two", "one two", "one "),
    ] {
        for (overrides, expected) in [("", inactive), ("b", active)] {
            let mut d = eval(format!(
                r#"
TEST:a = "one two"
TEST:a:b = "{value}"
OVERRIDES = "{overrides}"
    "#
            ));

            d.rename_var("TEST:a", format!("WAT:{operator}")).unwrap();

            assert_eq!(get_var!(&d, "WAT").unwrap(), expected);
            // The descendant is filtered by global OVERRIDES; a direct variant lookup
            // must not activate it by treating b as an operation scope.
            assert_eq!(get_var!(&d, "WAT:b"), None);
        }
    }
}

#[test_log::test]
fn basic_2() {
    let mut d = eval(
        r#"
TEST:a:b:c = "1"
TEST:a:b = "2"
TEST:a = "3"
    "#,
    );

    d.rename_var("TEST:a:b", "WAT").unwrap();

    assert_eq!(get_var!(&d, "WAT").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a:b:c"), None);
    assert_eq!(get_var!(&d, "TEST:a:b"), None);
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "3");
    assert_eq!(get_var!(&d, "TEST"), None);

    assert_eq!(get_var!(&d, "WAT:c").unwrap(), "1");
}

#[test_log::test]
fn basic_3() {
    let mut d = eval(
        r#"
TEST:a:b:c = "1"
TEST:a:b = "2"
TEST:a = "3"
    "#,
    );

    d.rename_var("TEST:a", "WAT:a").unwrap();

    let p = d.get_all_keys();
    assert_eq!(p, vec!["WAT:a", "WAT:a:b", "WAT:a:b:c"]);

    assert!(get_var!(&d, "TEST").is_none());
    assert!(get_var!(&d, "TEST:a:b:c").is_none());
    assert!(get_var!(&d, "TEST:a:b").is_none());
    assert!(get_var!(&d, "TEST:a").is_none());
    assert!(get_var!(&d, "WAT").is_none());

    assert_eq!(get_var!(&d, "WAT:a:b:c").unwrap(), "1");
    assert_eq!(get_var!(&d, "WAT:a:b").unwrap(), "2");
    assert_eq!(get_var!(&d, "WAT:a").unwrap(), "3");
}

#[test_log::test]
fn basic_4() {
    let mut d = eval(
        r#"
TEST:a:b:c = "1"
TEST:a:b = "2"
TEST:a = "3"
OVERRIDES = "a:b"
    "#,
    );

    d.rename_var("TEST", "WAT").unwrap();

    let p = d.get_all_keys();
    assert_eq!(p, vec!["OVERRIDES", "WAT", "WAT:a", "WAT:a:b", "WAT:a:b:c"]);

    assert!(get_var!(&d, "TEST").is_none());
    assert!(get_var!(&d, "TEST:a:b:c").is_none());
    assert!(get_var!(&d, "TEST:a:b").is_none());
    assert!(get_var!(&d, "TEST:a").is_none());

    assert_eq!(get_var!(&d, "WAT").unwrap(), "2");
    assert_eq!(get_var!(&d, "WAT:a:b:c").unwrap(), "1");
    assert_eq!(get_var!(&d, "WAT:a:b").unwrap(), "2");
    assert_eq!(get_var!(&d, "WAT:a").unwrap(), "3");
}

#[test_log::test]
fn basic_5() {
    let mut d = eval(
        r#"
TEST:a:b:c = "1"
TEST:a:b = "2"
TEST:a = "3"
    "#,
    );
    // TODO: expected keys: ['TEST:a:b:c', 'TEST:a', 'TEST:a:b']

    d.rename_var("TEST:a", "WAT:t").unwrap();
    // TODO: expected keys: ['TEST:a:b:c', 'WAT:t', 'TEST:a:b']

    assert_eq!(get_var!(&d, "WAT:t").unwrap(), "3");
}

#[test_log::test]
fn basic_6() {
    let mut d = eval(
        r#"
TES${TT} = "WAT"
TT = "T"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "WAT");
}

#[test]
fn rename_append_1() {
    let mut d = eval(
        r#"
TEST:${A} = "A"
    "#,
    );

    d.rename_var("TEST:${A}", "TEST:append").unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "A");
}

#[test]
fn rename_append_2() {
    let mut d = eval(
        r#"
TEST = "b"
TES${A} = "A"
A = "T:append"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "bA");
}

#[test]
fn rename_append_3() {
    let mut d = eval(
        r#"
TEST = "b"
TES${A} = "A"
TE${B}d = "C"
A = "T:append"
B = "ST:appen"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "bCA");
}

#[test_log::test]
fn rename_append_4() {
    let mut d = eval(
        r#"
TEST = "b"
TEST:${A} = "A"
A = "append"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "bA");
}

#[test]
fn rename_append_5() {
    let mut d = eval(
        r#"
TEST = "b"
TEST:${A} = "A"
TES${B} = "B"
TE${C}d = "C"
A = "append"
B = "T:append"
C = "ST:appen"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "bCBA");
}

#[test]
fn rename_append_6() {
    let mut d = eval(
        r#"
TEST = "b"
TES${T} = "base"
TEST:${A} = "A"
TES${B} = "B"
TE${C}d = "C"
A = "append"
B = "T:append"
C = "ST:appen"
T = "T"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "baseCBA");
}

#[test]
fn rename_append_7() {
    let mut d = eval(
        r#"
TEST = "b"
TES${T} = "base"
TEST:${A} = "A"
TES${B} = "B"
TE${C}d = "C"
TE${C}d = "C"
A = "append"
B = "T:append"
C = "ST:appen"
T = "T"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "baseCBA");
}

#[test]
fn rename_append_8() {
    let mut d = eval(
        r#"
TEST = "b"
TES${T}:append = "2"
T = "T"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "b2");
}

#[test_log::test]
fn rename_1() {
    let mut d = eval(
        r#"
TEST = "b"
TEST:${A}:append = "2"
A = "a"
OVERRIDES = "a"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "2");
}

#[test_log::test]
fn rename_2() {
    let mut d = eval(
        r#"
TEST:${A}:append = "2"
A = "a"
OVERRIDES = "a"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "2");
}

#[test_log::test]
fn rename_3() {
    let mut d = eval(
        r#"
TEST:${A}:append = "2"
A = "q"
TEST:${A}:q:a:q = "P"
OVERRIDES = "a"
    "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST:q").unwrap(), "2");
}
