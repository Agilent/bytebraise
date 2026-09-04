mod del_var;
mod expand_keys;
mod keys;
mod mixed_operators;
mod quirks;
mod rename_var;
mod wat;

#[cfg(test)]
mod test {
    use crate::evaluate::eval;
    use crate::macros::get_var;
    use crate::petgraph2::{DataSmart, score_override};
    use indexmap::IndexSet;
    use std::borrow::Cow;

    fn score<S: AsRef<str>>(input: S) -> (Vec<usize>, usize, usize) {
        let input = input.as_ref().replace(':', "");
        let active_overrides: IndexSet<String> =
            IndexSet::from(["a".into(), "b".into(), "c".into()]);

        let candidate: Vec<String> = input.chars().map(String::from).collect();
        let ret = score_override(&Cow::Borrowed(&active_overrides), &candidate).unwrap();

        eprintln!("{input} => {ret:?}");

        ret
    }

    #[test]
    fn doc_examples() {
        let d = eval(
            r#"
A = "1"
A:append = "2"
A:append = "3"
A += "4"
A .= "5"
        "#,
        );

        assert_eq!(get_var!(&d, "A").unwrap(), "1 4523");
    }

    #[test]
    fn none() {
        let d = DataSmart::new();
        assert_eq!(get_var!(&d, "NOT_EXIST"), None);
    }

    #[test]
    fn multiple_append() {
        let d = eval(
            r#"
TEST = "1"
TEST:append = "2"
TEST:append = "3"
        "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("123".into()));
    }

    #[test]
    fn override_score() {
        let d = eval(
            r#"
TEST = "1"
TEST:more = "2"
TEST:more:specific = "3"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_2() {
        let d = eval(
            r#"
TEST = "1"
TEST:more = "2"
TEST:more:specific = "3"
OVERRIDES = "more"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn override_score_3() {
        let d = eval(
            r#"
TEST = "1"
TEST:more = "2"
TEST:more:specific = "3"
OVERRIDES = "more:specific"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_score_4() {
        let d = eval(
            r#"
TEST = "1"
TEST:more:append = "2"
TEST:more:specific = "3"
TEST:more:specific = "4"
OVERRIDES = "more:specific"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn override_score_5() {
        let d = eval(
            r#"
TEST = "1"
TEST:more = "2"
TEST:more:specific = "3"
TEST:more:specific = "4"
TEST:more = "5"
TEST:more = "6"
OVERRIDES = "more"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("6".into()));
    }

    #[test]
    fn override_score_6() {
        let d = eval(
            r#"
TEST = "1"
TEST:more = "2"
TEST:more:specific = "3"
TEST:more:specific = "4"
TEST:more = "5"
TEST:more = "6"
OVERRIDES = ""
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_7() {
        let d = eval(
            r#"
TEST = "1"
TEST:append = "2"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("12".into()));
    }

    #[test]
    fn override_score_8() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("23".into()));
    }

    #[test]
    fn override_score_9() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("234".into()));
    }

    #[test]
    fn override_score_10() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
TEST:append:a = "NO"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("234".into()));
    }

    #[test]
    fn override_score_11() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
TEST:b:append = "BASE"
OVERRIDES = "b"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("BASE34".into()));
    }

    #[test]
    fn override_score_12() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
TEST:b:append = "BASE"
TEST:b = "OH YES"
OVERRIDES = "b"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("OH YESBASE34".into()));
    }

    #[test]
    fn override_score_13() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
TEST:b:append = "BASE"
TEST:b = "OH YES"
TEST:c = "WHAT"
OVERRIDES = "b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("WHAT34".into()));
    }

    #[test]
    fn override_score_14() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
TEST:b:append = "BASE"
TEST:b = "OH YES"
TEST:c = "WHAT"
TEST:c:append = "!"
OVERRIDES = "b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("WHAT!34".into()));
    }

    #[test]
    fn override_score_15() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
TEST:b:append = "BASE"
TEST:b = "OH YES"
TEST:c:prepend = "Q"
TEST:c = "WHAT"
TEST:c:append = "!"
OVERRIDES = "b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_score_16() {
        let d = eval(
            r#"
TEST = "10"
TEST:append = "3"
TEST:append = "4"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("1034".into()));
    }

    #[test]
    fn override_score_17() {
        let mut d = eval(
            r#"
TEST:append = "why?"
TEST:a:b:append = "first"
TEST:a:b:${OP} = "OP"
OP = "append"
OVERRIDES = "a:b"
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some("firstOPwhy?".into()));
    }

    #[test]
    fn override_score_18() {
        let d = eval(
            r#"
TEST = "1"
TEST = "2"
TEST:append = "3"
TEST:append = "4"
TEST:b:append = "base"
TEST:b = "OH YES"
TEST:c:prepend = "Q"
TEST:c = "WHAT"
TEST:c:append = "!"
OVERRIDES = "b:c:"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_priority_order() {
        let d = eval(
            r#"
TEST = "1"
TEST:a = "2"
TEST:b = "3"
TEST:b:a = "6"
TEST:a:b = "5"
OVERRIDES = "a:b:c"
            "#,
        );

        score("");
        score("a");
        score("b");
        score("b:a");
        score("a:b");

        assert_eq!(get_var!(&d, "TEST"), Some("5".into()));
    }

    #[test]
    fn override_priority_order_2() {
        let d = eval(
            r#"
TEST = "1"
TEST:a = "2"
TEST:b = "3"
TEST:a:b = "5"
TEST:b:a = "6"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("5".into()));
    }

    #[test]
    fn override_priority_order_3() {
        let d = eval(
            r#"
TEST = "1"
TEST:c:b:a = "2"
TEST:a:b:c = "3"
TEST:c:a:b = "4"
TEST:b:a:c = "5"
TEST:b:c:a = "6"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_priority_order_4() {
        let d = eval(
            r#"
TEST = "1"
TEST:a = "2"
TEST:a:a = "3"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }
    #[test]
    fn override_selection_order_sensitivity() {
        let d = eval(
            r#"
TEST = "1"
TEST:b:a:append = "2"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_2() {
        let d = eval(
            r#"
TEST = "1"
TEST:b:a:append = "2"
TEST:a:b:append = "3"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_3() {
        let d = eval(
            r#"
TEST = "1"
TEST:a:b:append = "3"
TEST:b:a:append = "2"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_4() {
        let d = eval(
            r#"
TEST = "1"
TEST:a:b:append = "3"
TEST:b:a:append = "2"
TEST:a:b:a:append = "4"
OVERRIDES = "a:b:c"
            "#,
        );

        score("ab");
        score("ba");
        score("aba");
        score("bab");
        score("aabb");
        score("abab");
        score("baba");
        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn tricky_1() {
        let d = eval(
            r#"
TEST = "1"
TEST:c:b = "2"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_2() {
        let d = eval(
            r#"
TEST = "1"
TEST:c:b:a:b:c = "2"
TEST:a:b:c = "3"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));

        let d = eval(
            r#"
TEST = "1"
TEST:a:b:c = "3"
TEST:c:b:a:b:c = "2"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_3() {
        let d = eval(
            r#"
TEST = "1"
TEST:a:b:c = "3"
TEST:a:b:c:a = "4"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));

        let d = eval(
            r#"
TEST = "1"
TEST:a:b:c:a = "4"
TEST:a:b:c = "3"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn filter_order_sensitivity() {
        let d = eval(
            r#"
TEST = "1"
TEST:append:b:a = "2"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("12".into()));
    }

    #[test]
    fn order_of_operations() {
        let mut d = eval(
            r#"
TEST = "1 2 3"
TEST:${B} = "2"
TEST:append = " 4"
B = "remove"
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some("1  3 4".into()));
    }

    #[test]
    fn indirection_1() {
        let mut d = eval(
            r#"
TEST = "1 2 3"
TEST:${${B}} = "2"
B = "${W}"
W = "Q"
Q = "remove"
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:remove");
        assert_eq!(get_var!(&d, "TEST"), Some("1  3".into()));
    }

    #[test]
    fn indirection_and_order() {
        let mut d = eval(
            r#"
TEST = "1 2 3"
TEST:${${B}} = " 4 "
B = "${W}"
W = "Q"
Q = "append"
TEST:append = " 5 "
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:append");
        assert_eq!(get_var!(&d, "TEST"), Some("1 2 3 5  4 ".into()));
    }

    #[test]
    fn indirection_and_order_and_score() {
        let mut d = eval(
            r#"
TEST = "1 2 3"
TEST:${${B}} = " 4 "
B = "${W}"
W = "Q"
Q = "append"
TEST:append = " 5 "
TEST:b:append = "OK"
OVERRIDES = "b"
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:append");
        assert_eq!(get_var!(&d, "TEST"), Some("OK 5  4 ".into()));
    }

    #[test]
    fn dumb() {
        let mut d = eval(
            r#"
TEST = "base"
TEST:append = "1"
TEST${B} = " wat"
TEST:append = "2"

B = ":append"

        "#,
        );

        d.expand_keys().unwrap();
        assert_eq!(get_var!(&d, "TEST").unwrap(), "base12 wat");
    }

    #[test]
    fn synthesized_appends() {
        // The behavior of appends vs synthesized appends is different.
        // Normal appends stack:
        // let d = eval(
        //     r#"
        // TEST = "10"
        // TEST:append = "1"
        // TEST:append = "2"
        //     "#,
        // );
        // assert_eq!(get_var!(&d, "TEST"), Some("1012".into()));

        // But synthesized appends only take the last one:
        let mut d = eval(
            r#"
TEST = "10"
TEST:${A} = "1"
TEST:${A} = "2"
A = "append"
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some("102".into()));
    }

    #[test]
    fn plus_equals() {
        let d = eval(
            r#"
TEST = "base"
TEST += "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("base 2".into()));
    }

    #[test]
    fn plus_equals_no_base() {
        let d = eval(
            r#"
TEST += "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some(" 2".into()));
    }

    #[test]
    fn dot_equals() {
        let d = eval(
            r#"
TEST = "base"
TEST .= "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("base2".into()));
    }

    #[test]
    fn dot_equals_no_base() {
        let d = eval(
            r#"
TEST .= "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn equals_plus() {
        let d = eval(
            r#"
TEST = "base"
TEST =+ "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("2 base".into()));
    }

    #[test]
    fn equals_plus_no_base() {
        let d = eval(
            r#"
TEST =+ "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("2 ".into()));
    }

    #[test]
    fn equals_dot() {
        let d = eval(
            r#"
TEST = "base"
TEST =. "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("2base".into()));
    }

    #[test]
    fn equals_dot_no_base() {
        let d = eval(
            r#"
TEST =. "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default() {
        let d = eval(
            r#"
TEST ??= "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default_2() {
        let d = eval(
            r#"
TEST ??= "2"
TEST ??= "3"
TEST ??= "4"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_doc_example() {
        let d = eval(
            r#"
W ??= "x"
W += "y"
            "#,
        );
        assert_eq!(get_var!(&d, "W"), Some(" y".into()));

        let d = eval(
            r#"
W ??= "x"
W:append = "y"
            "#,
        );
        assert_eq!(get_var!(&d, "W"), Some("xy".into()));
    }

    #[test]
    fn weak_default_priority() {
        let d = eval(
            r#"
TEST ??= "2"
TEST:a ??= "3"
TEST:a:b ??= "4"
TEST:b ??= "5"
OVERRIDES = "a:b"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_append() {
        let d = eval(
            r#"
TEST = ""
TEST:append = "wat"
TEST:a ??= "OK"
OVERRIDES = "a:b"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("OKwat".into()));
    }

    #[test]
    fn append_interactions() {
        let d = eval(
            r#"
TEST = "1"
TEST:a:b = "2"
TEST:a:b:a:append = "3"
TEST:a:b:a += "5"
TEST:a:b += "6"
OVERRIDES = "a:b"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some(" 53".into()));
    }

    #[test]
    fn more_synthesized_appends() {
        let mut d = eval(
            r#"
TEST = "1"
TEST:a:b = "2"
TEST:a:b:a:append = "3"
TEST:a:b:a += "5"
A = "a"
TEST:${A}:b:a:append = "7"
TEST:${A}:b:a:append = "7"
OVERRIDES = "a:b:c"
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some(" 5377".into()));
    }

    #[test]
    fn more_synthesized_appends_2() {
        let mut d = eval(
            r#"
TEST = "1"
TEST:a:b = "2"
TEST:a:b:a:append = "3"
TEST:a:b:a += "5"
TEST:a:b += "6"
OP = "append"
TEST:a:b:${OP} = "Q"
A = "a"
TEST:${A}:b:a:${OP} = "7"
TEST:${A}:b:a:${OP} = "7"
TEST:a:append:${B} = "10"
OVERRIDES = "a:b:c"
            "#,
        );

        d.get_all_keys();
        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some(" 537".into()));
    }

    #[test]
    fn default_var() {
        let d = eval(
            r#"
TEST ?= "1"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));

        let d = eval(
            r#"
TEST ?= "1"
TEST ?= "2"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn default_precedence() {
        let d = eval(
            r#"
TEST ??= "2"
TEST ?= "1"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn weak_default_precedence() {
        let d = eval(
            r#"
TEST:a ??= "2"
TEST ?= "1"
OVERRIDES = "a:b:c"
            "#,
        );

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn finalization() {
        let mut d = eval(
            r#"
A${B} = "X"
B = "2"
A2 = "Y"
            "#,
        );

        d.expand_keys().unwrap();
        assert_eq!(get_var!(&d, "A2"), Some("X".into()));
    }

    #[test]
    fn key_expansion() {
        let mut d = eval(
            r#"
TEST${A} = "1"
TEST2 = "2"
A = "2"
            "#,
        );
        assert_eq!(get_var!(&d, "TEST${A}"), Some("1".into()));

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST2"), Some("1".into()));
    }

    // #[test]
    // fn get_var_varflag_operations() {
    //     let d = eval(
    //         r#"
    // P = ""
    // P:append = "append!"
    //         "#,
    //     );
    //     assert!(get_var!(&d, "P:append").is_none());
    // }

    #[test_log::test]
    fn test_wat() {
        let mut d = eval(
            r#"
P = ""
P:a = "append"
Q = "base "
Q:${P} = "OK2"
Q:append = "me first"
OVERRIDES = "a"
            "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "Q").unwrap(), "base me firstOK2");
    }

    #[test]
    fn variable_roots_1() {
        let d = eval(
            r#"
P:inactive = ":)"
            "#,
        );
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_2() {
        let d = eval(
            r#"
P = "p"
P:inactive = ":)"
            "#,
        );
        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_3() {
        let d = eval(
            r#"
P = "p"
P:t = "t"
P:inactive = ":)"
OVERRIDES = "t"
            "#,
        );
        assert_eq!(get_var!(&d, "P").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:t").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_4() {
        let d = eval(
            r#"
P = "p"
P:t = "t"
P:inactive = ":)"
P:append = "base"
OVERRIDES = "t"
            "#,
        );

        assert_eq!(get_var!(&d, "P").unwrap(), "tbase");

        // selected start values is P:t, so P:append doesn't apply
        assert_eq!(get_var!(&d, "P:t").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_5() {
        let d = eval(
            r#"
P = "p"
P:inactive = ":)"
P:inactive:append = "!"
            "#,
        );

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)!");
    }

    #[test]
    fn variable_roots_6() {
        let d = eval(
            r#"
P = "p"
P:inactive = ":)"
P:append:inactive = "!"
            "#,
        );
        // this :append only applies when 'inactive' is in override set, which it's not

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_7() {
        let d = eval(
            r#"
P = "p"
P:inactive = ":)"
P:inactive:append = "@"
P:append:inactive = "!"
P:inactive:append:inactive = "?"
            "#,
        );
        // applies if P:inactive is selected as starting value
        // this :append only applies when 'inactive' is in override set, which it's not
        // applies if P:inactive is selected as starting value AND 'inactive' is in override set

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)@");
    }

    #[test]
    fn variable_roots_8() {
        let d = eval(
            r#"
P = "p"
P:O = "t"
P:O:append = "!"
OVERRIDES = "O"
            "#,
        );
        assert!(get_var!(&d, "P:notexist").is_none());
        assert_eq!(get_var!(&d, "P:O").unwrap(), "t!");
    }

    #[test]
    fn variable_roots_9() {
        let mut d = eval(
            r#"
Q = "q"
Q:${IN} = "t"
IN = "please"
            "#,
        );
        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "Q:please").unwrap(), "t");
    }

    #[test]
    fn variable_roots_10() {
        let d = eval(
            r#"
Q = "q"
Q:a:b = "ab"
Q:b:a = "ba"
            "#,
        );
        assert_eq!(get_var!(&d, "Q:a:b").unwrap(), "ab");
        assert_eq!(get_var!(&d, "Q:b:a").unwrap(), "ba");
    }

    #[test]
    fn get_var_append() {
        let d = eval(
            r#"
TEST = "a"
TEST:append = "b"
        "#,
        );

        assert_eq!(get_var!(&d, "TEST:append"), None);
    }

    #[test]
    fn doccc() {
        let d = eval(
            r#"
MY_VAR = "base"
MY_VAR:a = "different"
MY_VAR:a:append = "!"
MY_VAR:append:a = "?"
        "#,
        );

        assert_eq!(get_var!(&d, "MY_VAR").unwrap(), "base");
        assert_eq!(get_var!(&d, "MY_VAR:a").unwrap(), "different!");
    }

    #[test]
    fn doccc2() {
        let d = eval(
            r#"
MY_VAR = "base"
MY_VAR:a = "different"
MY_VAR:a:append = "!"
MY_VAR:append:a = "?"
OVERRIDES = "a:b:c"
        "#,
        );

        assert_eq!(get_var!(&d, "MY_VAR").unwrap(), "different!?");
        assert_eq!(get_var!(&d, "MY_VAR:a").unwrap(), "different!");
    }

    #[test]
    fn override_score_trickery() {
        let mut d = eval(
            r#"
MY_VAR:a = "1"
MY_VAR:a:b = "2"
MY_VAR:a:b:a:b = "3"
MY_VAR:b:a:b:a = "4"
MY_VAR:a:a:b:b = "5"
MY_VAR:a:append:${B}:b = "7"
B = "a"
OVERRIDES = "a:b:c"
        "#,
        );

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "MY_VAR").unwrap(), "5");
    }
}
