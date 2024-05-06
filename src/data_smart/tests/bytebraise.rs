#[cfg(test)]
use regex::Captures;

use crate::basic_datasmart_test;
#[cfg(test)]
use crate::data_smart::{DataSmart, DataSmartError, DataSmartResult, VAR_EXPANSION_REGEX};

basic_datasmart_test!(do_something, d, {
    d.set_var("OVERRIDES", "test:local:bar")?;
    d.set_var("A", "B")?;
    d.set_var("A_test", "Q")?;
    d.set_var("A_local_test", "QQQQQ")?;

    let v = d.get_var("A").unwrap().unwrap();
    assert_eq!(v, "QQQQQ");
});

basic_datasmart_test!(do_remove_expand, d, {
    d.set_var("TEST", "1 2 3")?;
    d.set_var("TEST_${Q}", "2")?;
    d.set_var("Q", "remove")?;

    let v = d.get_var("TEST").unwrap().unwrap();
    assert_eq!(v, "QQQQQ");
});

#[test]
fn var_expansion_regex_works() {
    let input = "${${foo}}";

    let m = VAR_EXPANSION_REGEX.replace(input, |_caps: &Captures| "OK");
    assert_eq!(m.into_owned(), String::from("${OK}"));
}

basic_datasmart_test!(parent_stuff, d, {
    d.set_var("A", "B")?;

    let copy = d.create_copy();
    //d.set_var("A", "Q", false);
    let v = copy.get_var("A")?.ok_or(DataSmartError::UnwrapNoneError)?;
    assert_eq!(v, "B");
});

#[cfg(feature = "python")]
basic_datasmart_test!(expansion_stuff, d, {
    d.set_var("A", "B")?;
    d.set_var("Q", "FRIEND")?;
    d.set_var("A_append", "Q")?;
    d.set_var("OVERRIDES_append", "T")?;
    d.set_var("EXP", "${A}${@d.getVar(\"Q\")}")?;
    d.set_var_flag("Q", "wat", "1")?;

    let v = d.expand("${EXP}")?.ok_or(DataSmartError::UnwrapNoneError)?;
    assert_eq!(v, "BQFRIEND");
});
