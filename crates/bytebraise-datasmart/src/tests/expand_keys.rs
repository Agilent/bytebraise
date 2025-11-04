#[cfg(test)]
use crate::{evaluate::eval, macros::get_var};

#[test]
fn expand_keys_1() {
    let mut d = eval(
        r#"
TEST = "a"
TES${B} += "b"
TES${B} += "c"
B = "T"
        "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), " b c");
}

#[test]
fn expand_keys_2() {
    let mut d = eval(
        r#"
TEST = "a"
TES${B}:append = "b"
TES${B}:append = "c"
B = "T"
        "#,
    );

    assert_eq!(d.get_all_keys(), vec!["B", "TES${B}", "TEST"]);

    // TODO: give 'replaces key' warning
    let todolist = d.expand_keys().unwrap();
    assert_eq!(todolist, vec!["TES${B}"]);

    assert_eq!(get_var!(&d, "TEST").unwrap(), "abc");
}

#[test]
fn expand_keys_3() {
    let mut d = eval(
        r#"
TEST = "a"
TES${B}ppend = "b"
TES${B}ppend = "c"
B = "T:a"
        "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "ac");
}

#[test]
fn expand_keys_4() {
    let mut d = eval(
        r#"
TEST = "a"
TES${X}ppend = "b"
TES${Y}pend = "c"
X = "T:a"
Y = "T:ap"
        "#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "TEST").unwrap(), "abc");
}

#[test]
fn expand_keys_5() {
    let mut d = eval(
        r#"
TEST = "a"
TES${T} = "base"
TES${X}ppend = "b"
TES${Y}pend = "c"
X = "T:a"
Y = "T:ap"
T = "T"
        "#,
    );
    d.expand_keys().unwrap();
    assert_eq!(get_var!(&d, "TEST").unwrap(), "basebc");
}

#[test]
fn expand_keys_6() {
    let mut d = eval(
        r#"
TEST = "a"

TES${T} = "base"
TES${Y}pend = "c"
TES${X}ppend = "b"
TES${T}:append = "d"
TES${T}:append = "e"

# At this point (before expansion, these variables exist in the datastore:
#   - TEST
#   - TES${T}
#   - TES${Y}pend
#   - TES${X}ppend

X = "T:a"
Y = "T:ap"
T = "T"
        "#,
    );
    d.expand_keys().unwrap();

    // expand_keys should give this warning:
    // Variable key TES${T} (basede) replaces original key TEST (a).

    //

    assert_eq!(get_var!(&d, "TEST").unwrap(), "basedebc");
}

#[test]
fn expand_keys_7() {
    let mut d = eval(
        r#"
TEST = "base"
TES${Q}:append = "a"
TES${Q}:append = "b"
Q = "T"
P = "T:append"
TES${P} = "Q"
        "#,
    );
    // TODO expected keys: ['P', 'TEST', 'TES${P}', 'TES${Q}', 'Q']

    let todolist = d.expand_keys().unwrap();
    // TODO expected keys: ['Q', 'P', 'TEST']

    assert_eq!(todolist, vec!["TES${P}", "TES${Q}"]);

    // expand_keys should give this warning:
    // Variable key TES${T} (basede) replaces original key TEST (a).

    //

    assert_eq!(get_var!(&d, "TEST").unwrap(), "baseQab");
}

#[test]
fn expand_keys_8() {
    let mut d = eval(
        r#"
TEST = "base"
TES${Q}:append = "a"
TES${Q}:append = "b"
TES${Q}:append:${Q} = "b"
Q = "T"
P = "T:append"
TES${P} = "Q"
        "#,
    );
    // TODO expected keys: ['TEST', 'TES${P}', 'P', 'TES${Q}', 'Q', 'TES${Q}:append:${Q}']

    let todolist = d.expand_keys().unwrap();
    // TODO expected keys: ['TEST', 'P', 'TEST:append:T', 'Q']

    assert_eq!(todolist, vec!["TES${P}", "TES${Q}", "TES${Q}:append:${Q}"]);

    // TODO: expand_keys should give this warning:
    //  Variable key TES${Q} (ab) replaces original key TEST (baseQ)
}

#[test_log::test]
fn expand_keys_9() {
    let mut d = eval(
        r#"
        TEST:a:${C}b:c = "1"
        TEST:a:${C}:b = "2"
        TEST:a = "3"
        C = "p"
"#,
    );

    // TODO expected keys: ['TEST:a:${C}b:c', 'TEST:a', 'C', 'TEST:a:${C}:b']
    let todolist = d.expand_keys().unwrap();
    // TODO expected keys: ['TEST:a:p:b', 'TEST:a', 'C', 'TEST:a:pb:c']

    assert_eq!(todolist, vec!["TEST:a:${C}:b", "TEST:a:${C}b:c"]);

    assert_eq!(get_var!(&d, "TEST:a").unwrap(), "3");
    assert!(get_var!(&d, "TEST:a:${C}b:c").is_none());
    assert!(get_var!(&d, "TEST:a:${C}:b").is_none());
    assert!(get_var!(&d, "TEST").is_none());

    assert_eq!(get_var!(&d, "TEST:a:p:b").unwrap(), "2");
    assert_eq!(get_var!(&d, "TEST:a:pb:c").unwrap(), "1");
}
