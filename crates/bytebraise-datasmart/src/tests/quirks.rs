#[cfg(test)]
use crate::evaluate::eval;
#[cfg(test)]
use crate::macros::get_var;

#[test]
fn override_names_with_non_alphanumeric_suffixes() {
    let d = eval(
        r#"
TEST = "base"
TEST:some_val = "underscore"
TEST2 = "base"
TEST2:class-target = "hyphen"
OVERRIDES = "some_val:class-target"
"#,
    );

    assert_eq!(get_var!(&d, "TEST"), Some("underscore".into()));
    assert_eq!(get_var!(&d, "TEST2"), Some("hyphen".into()));
}

#[test]
fn override_names_resolved_by_key_expansion() {
    let mut d = eval(
        r#"
TARGET_ARCH = "x86_64"
PN = "gizmo-${TARGET_ARCH}"
VERSION = "1"
VERSION:pn-${PN} = "2"
TEST:${PN} = "base"
TEST:${PN}:append:pn-gizmo-${MACHINE} = " appended"
MACHINE = "qemux86"
OVERRIDES = "gizmo-x86_64:pn-gizmo-x86_64:pn-gizmo-qemux86"
"#,
    );

    d.expand_keys().unwrap();

    assert_eq!(get_var!(&d, "VERSION"), Some("2".into()));
    assert_eq!(get_var!(&d, "TEST"), Some("base appended".into()));
}

#[test]
fn override_operator_filter_casing() {
    // In BitBake, __setvar_regexp__ is:
    //   r'(?P<base>.*?)(?P<keyword>:append|:prepend|:remove)(:(?P<add>[^A-Z]*))?$'
    // which means that the bit after the operator (append/prepend/remove) can't contain
    // uppercase letters. If it does, then instead of creating a variable "B" with an 'append'
    // operation, we end up with a variable called "B:append:A".
    let d = eval(
        r#"
B:append:A = "Q"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:append:A"]);

    // This also means you can't have variable refs, unless the variables are lowercase
    let d = eval(
        r#"
B:append:${A} = "Q"
C:append:${a} = "T"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:append:${A}", "C"]);
}

#[test]
fn override_operator_get_keys_bitbake_bug() {
    let d = eval(
        r#"
B = "A"
B:a:${Q}:append:${P} = "Q"

# The presence of ${P} here means this assignment is not an append operation.
# But we still need to handle the :b:t part.
B:a:${Q}:append:${P}:b:t = "Q"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(
        keys,
        vec!["B", "B:a:${Q}:append:${P}", "B:a:${Q}:append:${P}:b:t"]
    );
}
