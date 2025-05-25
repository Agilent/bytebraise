#[cfg(test)]
use crate::{
    evaluate::eval,
    macros::get_var
};

#[test]
fn basic_1() {
    let mut d = eval(
        r#"
TEST = "A"
    "#,
    );

    assert_eq!(get_var!(&d, "TEST").unwrap(), "A");

    d.rename_var("TEST", "NEW").unwrap();

    assert_eq!(get_var!(&d, "NEW").unwrap(), "A");
}

#[test]
fn rename_append_1() {
    let mut d = eval(
        r#"
TEST:${A} = "A"
    "#,
    );

    dbg!(&d);

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

    dbg!(&d);

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

    dbg!(&d);

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "bCA");
}

#[test]
fn rename_append_4() {
    let mut d = eval(
        r#"
TEST = "b"
TEST:${A} = "A"
A = "append"
    "#,
    );

    dbg!(&d);

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

    dbg!(&d);

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

    dbg!(&d);

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

    dbg!(&d);

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

    dbg!(&d);

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "b2");
}
