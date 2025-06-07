#[cfg(test)]
use crate::{evaluate::eval, macros::get_var};

#[test]
fn remove_default_1() {
    let d = eval(
        r#"
TEST = "test"
TEST:remove ?= "test"
        "#,
    );

    assert!(get_var!(&d, "TEST:remove").is_none());
    assert_eq!(get_var!(&d, "TEST").unwrap(), "");
}

#[test]
fn remove_default_2() {
    let d = eval(
        r#"
TEST = "test two three"
TEST:remove ?= "test"
TEST:remove ?= "two"
TEST:remove ?= "three"
        "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "  ");
}

#[test]
fn remove_parts() {
    let d = eval(
        r#"
TEST = "test"
TEST:remove ?= "t"
TEST:remove ?= "est"
        "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "test");
}

#[test]
fn remove_weak_default() {
    let d = eval(
        r#"
TEST = "test"
TEST:remove ??= "test"
        "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "test");
}

#[test]
fn remove_weak_default_2() {}

#[test]
fn remove_dot() {
    let d = eval(
        r#"
TEST = "test three four"
TEST:remove ??= "test"

TEST:remove .= "four"
TEST:remove =. "three"
        "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "test  ");
}

#[test]
fn remove_plus() {
    // In BitBake, this would be like adding ' test', ' four', and 'three ' to remove.
    // But since 'remove' is whitespace delimited, it is equivalent to just using =.
    let d = eval(
        r#"
TEST = "test three four"
TEST:remove += "test"
TEST:remove += "four"
TEST:remove =+ "three"
        "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "  ");
}

#[test]
fn plus_equals() {
    let d = eval(
        r#"
TEST = "base"
TEST:append += "test"
TEST:prepend += "four"
        "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), " fourbase test");
}

#[test]
fn equals_plus() {
    let d = eval(
        r#"
TEST = "base"
TEST:append =+ "test"
TEST:prepend =+ "four"
        "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "four basetest ");
}
