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
    assert_eq!(get_var!(&d, "TEST:a:b:c").unwrap(), "1");
    assert_eq!(get_var!(&d, "TEST:a:b"), None);
    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "3");
    assert_eq!(get_var!(&d, "TEST"), None);
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

    eprintln!();
    d.rename_var("TEST", "WAT").unwrap();
    eprintln!();
    d.dump();

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
    "#,
    );
    // TODO: expected keys: ['TEST:a:b:c', 'TEST:a', 'TEST:a:b']

    d.rename_var("TEST:a", "WAT:t").unwrap();
    // TODO: expected keys: ['TEST:a:b:c', 'WAT:t', 'TEST:a:b']

    assert_eq!(get_var!(&d, "WAT:t").unwrap(), "3");
}

#[test_log::test]
fn basic_5() {
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

#[test_log::test]
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

    d.dump();

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

    dbg!(&d);

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
    d.dump();

    assert_eq!(get_var!(&d, "TEST:q").unwrap(), "P");
    panic!();
}
