use crate::evaluate::eval;
use crate::macros::get_var;

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

    d.expand_keys().unwrap();

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

    let todolist = d.expand_keys().unwrap();

    assert_eq!(todolist, vec!["TES${P}", "TES${Q}"]);

    // expand_keys should give this warning:
    // Variable key TES${T} (basede) replaces original key TEST (a).

    //

    assert_eq!(get_var!(&d, "TEST").unwrap(), "baseQab");
}
