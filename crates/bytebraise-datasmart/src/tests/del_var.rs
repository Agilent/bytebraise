#[cfg(test)]
use crate::{evaluate::eval, macros::get_var};

#[test]
fn del_var_1() {
    let mut d = eval(
        r#"
TEST = "1"
TEST:a = "2"
TEST:a:b = "3"
"#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
    d.del_var("TEST:a").unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a"), None);
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
}

#[test]
fn del_var_2() {
    let mut d = eval(
        r#"
TEST = "1"
TEST:a = "2"
TEST:a:b = "3"
"#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
    d.del_var("TEST").unwrap();

    assert_eq!(get_var!(&d, "TEST"), None);
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
}

#[test]
fn del_var_3() {
    let mut d = eval(
        r#"
TEST = "1"
TEST:a = "2"
TEST:a:b = "3"
"#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
    d.del_var("TEST:a").unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a"), None);
    assert_eq!(get_var!(&d, "TEST:a:b").unwrap(), "3");
}

#[test]
fn del_var_operators_1() {
    let mut d = eval(
        r#"
TEST = "1"
TEST:append = "2"
"#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "12");
    assert_eq!(get_var!(&d, "TEST:append"), None);
    // Should have no effect
    d.del_var("TEST:append").unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "12");
}

#[test]
fn del_var_operators_2() {
    let mut d = eval(
        r#"
TEST = "1"
TEST:a = "2"
OVERRIDES = "a"
"#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "2");
    // Should have no effect
    d.del_var("TEST:a:append").unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "2");
}

#[test_log::test]
fn del_var_unexpanded() {
    let mut d = eval(
        r#"
TEST = "1"
TEST:${T} = "2"
"#,
    );

    d.dump();

    d.del_var("TEST").unwrap();

    assert_eq!(get_var!(&d, "TEST"), None);
    assert_eq!(get_var!(&d, "TEST:${T}").unwrap(), "2");
}
