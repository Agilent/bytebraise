use crate::evaluate::eval;

#[test]
fn override_operator_filter_casing() {
    // In BitBake, __setvar_regexp__ is:
    //   r'(?P<base>.*?)(?P<keyword>:append|:prepend|:remove)(:(?P<add>[^A-Z]*))?$'
    // which means that the bit after the operator (append/prepend/remove) can't contain
    // uppercase letters. If it does, then instead of creating a variable "B" with an 'append'
    // operation, we end up with a variable called "B:append:A".
    let mut d = eval(
        r#"
B:append:A = "Q"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:append:A"]);

    // This also means you can't have variable refs, unless the variables are lowercase
    let mut d = eval(
        r#"
B:append:${A} = "Q"
C:append:${a} = "T"
"#,
    );

    let keys = d.get_all_keys();
    assert_eq!(keys, vec!["B:append:${A}", "C"]);
}
