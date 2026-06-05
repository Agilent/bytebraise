#[cfg(test)]
use crate::{evaluate::eval, macros::get_var};

#[test]
fn basic_1() {
    let d = eval(
        r#"
A = "${B} friend!"
B = "${C}"
C = "hello"
    "#,
    );

    assert_eq!(get_var!(&d, "A").unwrap(), "hello friend!");
}

#[test]
fn basic_2() {
    let d = eval(
        r#"
A = "${B} friend!"
B = "${C}"
C = "hello"
C:append = " good"
    "#,
    );

    assert_eq!(get_var!(&d, "A").unwrap(), "hello good friend!");
}

#[test]
fn basic_3() {
    let d = eval(
        r#"
C = "hello"
C:append = " friend"
C = "see ya"
    "#,
    );

    assert_eq!(get_var!(&d, "C").unwrap(), "see ya friend");
}

#[test]
fn basic_4() {
    let d = eval(
        r#"
C ?= "weak!"
"#,
    );

    assert_eq!(get_var!(&d, "C").unwrap(), "weak!");

    let d = eval(
        r#"
C ?= "weak!"
C = "value"
"#,
    );

    assert_eq!(get_var!(&d, "C").unwrap(), "value");

    let d = eval(
        r#"
C ?= "weak!"
C ?= "value"
"#,
    );

    assert_eq!(get_var!(&d, "C").unwrap(), "weak!");
}

#[test]
fn basic_5() {
    let d = eval(
        r#"
C ??= "weak!"
C = "value"
"#,
    );

    assert_eq!(get_var!(&d, "C").unwrap(), "value");

    let d = eval(
        r#"
C ??= "weak!"
C ??= "weaker!"
"#,
    );

    assert_eq!(get_var!(&d, "C").unwrap(), "weaker!");
}
