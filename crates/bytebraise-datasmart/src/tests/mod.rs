mod del_var;
mod expand_keys;
mod keys;
mod mixed_operators;
mod quirks;
mod rename_var;

#[cfg(test)]
mod test {
    use crate::evaluate::eval;
    use crate::macros::{get_var, set_var};
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
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:append", "2");
        set_var!(&mut d, "TEST:append", "2");

        assert_eq!(get_var!(&d, "TEST"), Some("122".into()));
    }

    #[test]
    fn override_score() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:more", "2");
        set_var!(&mut d, "TEST:more:specific", "3");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_2() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:more", "2");
        set_var!(&mut d, "TEST:more:specific", "3");
        set_var!(&mut d, "OVERRIDES", "more");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn override_score_3() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:more", "2");
        set_var!(&mut d, "TEST:more:specific", "3");
        set_var!(&mut d, "OVERRIDES", "more:specific");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_score_4() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:more:append", "2");

        // All applicable overrides (at the same score level) are applied.
        set_var!(&mut d, "TEST:more:specific", "3");
        set_var!(&mut d, "TEST:more:specific", "4");
        set_var!(&mut d, "OVERRIDES", "more:specific");

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn override_score_5() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:more", "2");
        set_var!(&mut d, "TEST:more:specific", "3");
        set_var!(&mut d, "TEST:more:specific", "4");
        set_var!(&mut d, "TEST:more", "5");
        set_var!(&mut d, "TEST:more", "6");
        set_var!(&mut d, "OVERRIDES", "more");

        assert_eq!(get_var!(&d, "TEST"), Some("6".into()));
    }

    #[test]
    fn override_score_6() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:more", "2");
        set_var!(&mut d, "TEST:more:specific", "3");
        set_var!(&mut d, "TEST:more:specific", "4");
        set_var!(&mut d, "TEST:more", "5");
        set_var!(&mut d, "TEST:more", "6");
        set_var!(&mut d, "OVERRIDES", "");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn override_score_7() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:append", "2");

        assert_eq!(get_var!(&d, "TEST"), Some("12".into()));
    }

    #[test]
    fn override_score_8() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");

        assert_eq!(get_var!(&d, "TEST"), Some("23".into()));
    }

    #[test]
    fn override_score_9() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");

        assert_eq!(get_var!(&d, "TEST"), Some("234".into()));
    }

    #[test]
    fn override_score_10() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");
        set_var!(&mut d, "TEST:append:a", "NO");

        assert_eq!(get_var!(&d, "TEST"), Some("234".into()));
    }

    #[test]
    fn override_score_11() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");
        set_var!(&mut d, "TEST:b:append", "BASE");
        set_var!(&mut d, "OVERRIDES", "b");

        assert_eq!(get_var!(&d, "TEST"), Some("BASE34".into()));
    }

    #[test]
    fn override_score_12() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");
        set_var!(&mut d, "TEST:b:append", "BASE");
        set_var!(&mut d, "TEST:b", "OH YES");
        set_var!(&mut d, "OVERRIDES", "b");

        assert_eq!(get_var!(&d, "TEST"), Some("OH YESBASE34".into()));
    }

    #[test]
    fn override_score_13() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");
        set_var!(&mut d, "TEST:b:append", "BASE");
        set_var!(&mut d, "TEST:b", "OH YES");
        set_var!(&mut d, "TEST:c", "WHAT");
        set_var!(&mut d, "OVERRIDES", "b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("WHAT34".into()));
    }

    #[test]
    fn override_score_14() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");
        set_var!(&mut d, "TEST:b:append", "BASE");
        set_var!(&mut d, "TEST:b", "OH YES");
        set_var!(&mut d, "TEST:c", "WHAT");
        set_var!(&mut d, "TEST:c:append", "!");
        set_var!(&mut d, "OVERRIDES", "b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("WHAT!34".into()));
    }

    #[test]
    fn override_score_15() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");
        set_var!(&mut d, "TEST:b:append", "BASE");
        set_var!(&mut d, "TEST:b", "OH YES");
        set_var!(&mut d, "TEST:c:prepend", "Q");
        set_var!(&mut d, "TEST:c", "WHAT");
        set_var!(&mut d, "TEST:c:append", "!");
        set_var!(&mut d, "OVERRIDES", "b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_score_16() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "10");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");

        assert_eq!(get_var!(&d, "TEST"), Some("1034".into()));
    }

    #[test]
    fn override_score_17() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST:append", "why?");
        set_var!(&mut d, "TEST:a:b:append", "first");
        set_var!(&mut d, "TEST:a:b:${OP}", "OP");
        set_var!(&mut d, "OP", "append");
        set_var!(&mut d, "OVERRIDES", "a:b");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some("firstOPwhy?".into()));
    }

    #[test]
    fn override_score_18() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST", "2");
        set_var!(&mut d, "TEST:append", "3");
        set_var!(&mut d, "TEST:append", "4");
        set_var!(&mut d, "TEST:b:append", "base");
        set_var!(&mut d, "TEST:b", "OH YES");
        set_var!(&mut d, "TEST:c:prepend", "Q");
        set_var!(&mut d, "TEST:c", "WHAT");
        set_var!(&mut d, "TEST:c:append", "!");
        set_var!(&mut d, "OVERRIDES", "b:c:");

        assert_eq!(get_var!(&d, "TEST"), Some("QWHAT!34".into()));
    }

    #[test]
    fn override_priority_order() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a", "2");
        set_var!(&mut d, "TEST:b", "3");
        set_var!(&mut d, "TEST:b:a", "6");
        set_var!(&mut d, "TEST:a:b", "5");

        score("");
        score("a");
        score("b");
        score("b:a");
        score("a:b");

        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("5".into()));
    }

    #[test]
    fn override_priority_order_2() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a", "2");
        set_var!(&mut d, "TEST:b", "3");
        set_var!(&mut d, "TEST:a:b", "5");
        set_var!(&mut d, "TEST:b:a", "6");

        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("5".into()));
    }

    fn override_priority_order_3() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:c:b:a", "2");
        set_var!(&mut d, "TEST:a:b:c", "3");
        set_var!(&mut d, "TEST:c:a:b", "4");
        set_var!(&mut d, "TEST:b:a:c", "5");
        set_var!(&mut d, "TEST:b:c:a", "6");

        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    fn override_priority_order_4() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a", "2");
        set_var!(&mut d, "TEST:a:a", "3");

        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }
    #[test]
    fn override_selection_order_sensitivity() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:b:a:append", "2");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_2() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:b:a:append", "2");
        set_var!(&mut d, "TEST:a:b:append", "3");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_3() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b:append", "3");
        set_var!(&mut d, "TEST:b:a:append", "2");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("3".into()));
    }

    #[test]
    fn override_selection_order_sensitivity_4() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b:append", "3");
        set_var!(&mut d, "TEST:b:a:append", "2");
        set_var!(&mut d, "TEST:a:b:a:append", "4");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

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
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:c:b", "2");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_2() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:c:b:a:b:c", "2");
        set_var!(&mut d, "TEST:a:b:c", "3");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));

        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b:c", "3");
        set_var!(&mut d, "TEST:c:b:a:b:c", "2");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn tricky_3() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b:c", "3");
        set_var!(&mut d, "TEST:a:b:c:a", "4");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));

        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b:c:a", "4");
        set_var!(&mut d, "TEST:a:b:c", "3");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn filter_order_sensitivity() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:append:b:a", "2");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("12".into()));
    }

    #[test]
    fn order_of_operations() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1 2 3");
        set_var!(&mut d, "TEST:${B}", "2");
        set_var!(&mut d, "TEST:append", " 4");
        set_var!(&mut d, "B", "remove");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some("1  3 4".into()));
    }

    #[test]
    fn indirection_1() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1 2 3");
        set_var!(&mut d, "TEST:${${B}}", "2");
        set_var!(&mut d, "B", "${W}");
        set_var!(&mut d, "W", "Q");
        set_var!(&mut d, "Q", "remove");

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:remove");
        assert_eq!(get_var!(&d, "TEST"), Some("1  3".into()));
    }

    #[test]
    fn indirection_and_order() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1 2 3");

        set_var!(&mut d, "TEST:${${B}}", " 4 ");
        set_var!(&mut d, "B", "${W}");
        set_var!(&mut d, "W", "Q");
        set_var!(&mut d, "Q", "append");

        set_var!(&mut d, "TEST:append", " 5 ");

        d.expand_keys().unwrap();

        assert_eq!(d.expand("TEST:${${B}}").unwrap(), "TEST:append");
        assert_eq!(get_var!(&d, "TEST"), Some("1 2 3 5  4 ".into()));
    }

    #[test]
    fn indirection_and_order_and_score() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "1 2 3");

        set_var!(&mut d, "TEST:${${B}}", " 4 ");
        set_var!(&mut d, "B", "${W}");
        set_var!(&mut d, "W", "Q");
        set_var!(&mut d, "Q", "append");

        set_var!(&mut d, "TEST:append", " 5 ");

        set_var!(&mut d, "TEST:b:append", "OK");
        set_var!(&mut d, "OVERRIDES", "b");

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
        // let mut d = DataSmart::new();
        // set_var!(&mut d, "TEST", "10");
        // set_var!(&mut d, "TEST:append", "1");
        // set_var!(&mut d, "TEST:append", "2");
        // assert_eq!(get_var!(&d, "TEST"), Some("1012".into()));

        // But synthesized appends only take the last one:
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "10");
        set_var!(&mut d, "TEST:${A}", "1");
        set_var!(&mut d, "TEST:${A}", "2");
        set_var!(&mut d, "A", "append");
        let p = d.expand_keys().unwrap();
        assert_eq!(get_var!(&d, "TEST"), Some("102".into()));
    }

    #[test]
    fn plus_equals() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "base");
        d.plus_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("base 2".into()));
    }

    #[test]
    fn plus_equals_no_base() {
        let mut d = DataSmart::new();
        d.plus_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some(" 2".into()));
    }

    #[test]
    fn dot_equals() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "base");
        d.dot_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("base2".into()));
    }

    #[test]
    fn dot_equals_no_base() {
        let mut d = DataSmart::new();
        d.dot_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn equals_plus() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "base");
        d.equals_plus_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2 base".into()));
    }

    #[test]
    fn equals_plus_no_base() {
        let mut d = DataSmart::new();
        d.equals_plus_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2 ".into()));
    }

    #[test]
    fn equals_dot() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "TEST", "base");
        d.equals_dot_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2base".into()));
    }

    #[test]
    fn equals_dot_no_base() {
        let mut d = DataSmart::new();
        d.dot_equals_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn weak_default_2() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        d.weak_default_var("TEST", "3");
        d.weak_default_var("TEST", "4");
        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_doc_example() {
        let mut d = DataSmart::new();
        d.weak_default_var("W", "x");
        d.plus_equals_var("W", "y");
        assert_eq!(get_var!(&d, "W"), Some(" y".into()));

        let mut d = DataSmart::new();
        d.weak_default_var("W", "x");
        set_var!(&mut d, "W:append", "y");
        assert_eq!(get_var!(&d, "W"), Some("xy".into()));
    }

    #[test]
    fn weak_default_priority() {
        let mut d = DataSmart::new();
        d.weak_default_var("TEST", "2");
        d.weak_default_var("TEST:a", "3");
        d.weak_default_var("TEST:a:b", "4");
        d.weak_default_var("TEST:b", "5");

        set_var!(&mut d, "OVERRIDES", "a:b");
        assert_eq!(get_var!(&d, "TEST"), Some("4".into()));
    }

    #[test]
    fn weak_default_append() {
        let mut d = DataSmart::new();

        set_var!(&mut d, "TEST", "");
        set_var!(&mut d, "TEST:append", "wat");
        d.weak_default_var("TEST:a", "OK");
        set_var!(&mut d, "OVERRIDES", "a:b");

        assert_eq!(get_var!(&d, "TEST"), Some("OKwat".into()));
    }

    #[test]
    fn append_interactions() {
        let mut d = DataSmart::new();

        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b", "2");
        set_var!(&mut d, "TEST:a:b:a:append", "3");
        d.plus_equals_var("TEST:a:b:a", "5");
        d.plus_equals_var("TEST:a:b", "6");
        set_var!(&mut d, "OVERRIDES", "a:b");

        assert_eq!(get_var!(&d, "TEST"), Some(" 53".into()));
    }

    #[test]
    fn more_synthesized_appends() {
        let mut d = DataSmart::new();

        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b", "2");
        set_var!(&mut d, "TEST:a:b:a:append", "3");
        d.plus_equals_var("TEST:a:b:a", "5");

        set_var!(&mut d, "A", "a");
        set_var!(&mut d, "TEST:${A}:b:a:append", "7");
        set_var!(&mut d, "TEST:${A}:b:a:append", "7");

        set_var!(&mut d, "OVERRIDES", "a:b:c");

        //assert_eq!(get_var!(&d, "TEST"), Some(" 53".into()));

        let ret = d.expand_keys().unwrap();
        //assert_eq!(ret, Vec::<String>::new());

        // dbg!(&d);

        assert_eq!(get_var!(&d, "TEST"), Some(" 5377".into()));
    }

    #[test]
    fn more_synthesized_appends_2() {
        let mut d = DataSmart::new();

        set_var!(&mut d, "TEST", "1");
        set_var!(&mut d, "TEST:a:b", "2");
        set_var!(&mut d, "TEST:a:b:a:append", "3");
        d.plus_equals_var("TEST:a:b:a", "5");
        d.plus_equals_var("TEST:a:b", "6");

        set_var!(&mut d, "OP", "append");
        set_var!(&mut d, "TEST:a:b:${OP}", "Q");

        set_var!(&mut d, "A", "a");
        set_var!(&mut d, "TEST:${A}:b:a:${OP}", "7");
        set_var!(&mut d, "TEST:${A}:b:a:${OP}", "7");

        set_var!(&mut d, "TEST:a:append:${B}", "10");

        set_var!(&mut d, "OVERRIDES", "a:b:c");

        d.get_all_keys();
        //d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST"), Some(" 537".into()));
    }

    #[test]
    fn default_var() {
        let mut d = DataSmart::new();

        d.default_var("TEST", "1");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));

        let mut d = DataSmart::new();

        d.default_var("TEST", "1");
        d.default_var("TEST", "2");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn default_precedence() {
        let mut d = DataSmart::new();

        d.weak_default_var("TEST", "2");
        d.default_var("TEST", "1");

        assert_eq!(get_var!(&d, "TEST"), Some("1".into()));
    }

    #[test]
    fn weak_default_precedence() {
        let mut d = DataSmart::new();

        d.weak_default_var("TEST:a", "2");
        d.default_var("TEST", "1");
        set_var!(&mut d, "OVERRIDES", "a:b:c");

        assert_eq!(get_var!(&d, "TEST"), Some("2".into()));
    }

    #[test]
    fn finalization() {
        let mut d = DataSmart::new();

        set_var!(&mut d, "A${B}", "X");
        set_var!(&mut d, "B", "2");
        set_var!(&mut d, "A2", "Y");

        d.expand_keys().unwrap();
        assert_eq!(get_var!(&d, "A2"), Some("X".into()));
    }

    #[test]
    fn key_expansion() {
        let mut d = DataSmart::new();

        set_var!(&mut d, "TEST${A}", "1");
        assert_eq!(get_var!(&d, "TEST${A}"), Some("1".into()));

        set_var!(&mut d, "TEST2", "2");
        set_var!(&mut d, "A", "2");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "TEST2"), Some("1".into()));
    }

    // #[test]
    // fn get_var_varflag_operations() {
    //     let mut d = DataSmart::new();
    //     set_var!(&mut d, "P", "");
    //     set_var!(&mut d, "P:append", "append!");
    //     assert!(get_var!(&d, "P:append").is_none());
    // }

    #[test_log::test]
    fn test_wat() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "");
        set_var!(&mut d, "P:a", "append");
        set_var!(&mut d, "Q", "base ");
        set_var!(&mut d, "Q:${P}", "OK2");
        set_var!(&mut d, "Q:append", "me first");
        set_var!(&mut d, "OVERRIDES", "a");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "Q").unwrap(), "base me firstOK2");
    }

    #[test]
    fn variable_roots_1() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P:inactive", ":)");
        //dbg!(&d);
        //assert!(get_var!(&d, "P").is_none());
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_2() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "p");
        set_var!(&mut d, "P:inactive", ":)");
        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_3() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "p");
        set_var!(&mut d, "P:t", "t");
        set_var!(&mut d, "P:inactive", ":)");
        set_var!(&mut d, "OVERRIDES", "t");
        assert_eq!(get_var!(&d, "P").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:t").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_4() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "p");
        set_var!(&mut d, "P:t", "t");
        set_var!(&mut d, "P:inactive", ":)");
        set_var!(&mut d, "P:append", "base");
        set_var!(&mut d, "OVERRIDES", "t");

        assert_eq!(get_var!(&d, "P").unwrap(), "tbase");

        // selected start values is P:t, so P:append doesn't apply
        assert_eq!(get_var!(&d, "P:t").unwrap(), "t");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_5() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "p");
        set_var!(&mut d, "P:inactive", ":)");
        set_var!(&mut d, "P:inactive:append", "!");

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)!");
    }

    #[test]
    fn variable_roots_6() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "p");
        set_var!(&mut d, "P:inactive", ":)");
        // this :append only applies when 'inactive' is in override set, which it's not
        set_var!(&mut d, "P:append:inactive", "!");

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)");
    }

    #[test]
    fn variable_roots_7() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "p");
        set_var!(&mut d, "P:inactive", ":)");
        // applies if P:inactive is selected as starting value
        set_var!(&mut d, "P:inactive:append", "@");
        // this :append only applies when 'inactive' is in override set, which it's not
        set_var!(&mut d, "P:append:inactive", "!");
        // applies if P:inactive is selected as starting value AND 'inactive' is in override set
        set_var!(&mut d, "P:inactive:append:inactive", "?");

        assert_eq!(get_var!(&d, "P").unwrap(), "p");
        assert_eq!(get_var!(&d, "P:inactive").unwrap(), ":)@");
    }

    #[test]
    fn variable_roots_8() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "P", "p");
        set_var!(&mut d, "P:O", "t");
        set_var!(&mut d, "P:O:append", "!");
        set_var!(&mut d, "OVERRIDES", "O");
        //assert_eq!(get_var!(&d, "P").unwrap(), "t!");
        assert!(get_var!(&d, "P:notexist").is_none());
        assert_eq!(get_var!(&d, "P:O").unwrap(), "t!");
    }

    #[test]
    fn variable_roots_9() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "Q", "q");
        set_var!(&mut d, "Q:${IN}", "t");
        set_var!(&mut d, "IN", "please");
        //assert_eq!(get_var!(&d, "Q").unwrap(), "q");

        d.expand_keys().unwrap();

        assert_eq!(get_var!(&d, "Q:please").unwrap(), "t");
    }

    #[test]
    fn variable_roots_10() {
        let mut d = DataSmart::new();
        set_var!(&mut d, "Q", "q");
        set_var!(&mut d, "Q:a:b", "ab");
        set_var!(&mut d, "Q:b:a", "ba");
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
