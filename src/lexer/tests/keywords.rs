#[cfg(test)]
use crate::assert_tokenizes_as;
#[cfg(test)]
use crate::lexer::tests::{TokenMock, T};

#[test]
pub fn addhandler() {
    assert_tokenizes_as!(
        "addhandler my_handler",
        T![addhandler],
        ' ',
        TokenMock::directive_arg("my_handler")
    );
}

#[test]
pub fn basic_inherit() {
    assert_tokenizes_as!(
        "inherit ${@test}",
        T![inherit],
        ' ',
        TokenMock::unquoted("${@test}")
    );
}

#[test]
pub fn unquoted_values() {
    assert_tokenizes_as!(
        r#"inherit ${@test} \
        "#,
        T![inherit],
        ' ',
        TokenMock::unquoted("${@test} \\\n"),
        "        "
    );
}

#[test]
pub fn inherit_value_on_next_line() {
    let input = r###"inherit \
myvalue"###;

    assert_tokenizes_as!(input, T![inherit], ' ', TokenMock::unquoted("\\\nmyvalue"));
}

#[test]
pub fn inherit_value_same_line() {
    assert_tokenizes_as!(
        r##"
inherit cmake patch
        "##,
        '\n',
        T![inherit],
        ' ',
        TokenMock::unquoted("cmake patch"),
        '\n',
        "        "
    );
}

#[test]
pub fn reset_lexer_state_after_unquoted_value() {
    let input = r##"
inherit metadata_scm
inherit logging

OE_EXTRA_IMPORTS ?= ""
    "##;

    assert_tokenizes_as!(
        input,
        '\n',
        T![inherit],
        ' ',
        TokenMock::unquoted("metadata_scm"),
        '\n',
        T![inherit],
        ' ',
        TokenMock::unquoted("logging"),
        '\n',
        '\n',
        TokenMock::identifier("OE_EXTRA_IMPORTS"),
        ' ',
        T![?=],
        ' ',
        TokenMock::double_quote_token(""),
        '\n',
        "    "
    );
}

#[test]
pub fn inherit_value_with_spaces() {
    assert_tokenizes_as!(
        "inherit cmake patch",
        T![inherit],
        ' ',
        TokenMock::unquoted("cmake patch")
    );
}

#[test]
pub fn export_keyword() {
    assert_tokenizes_as!(
        "export VARIABLE",
        T![export],
        ' ',
        TokenMock::identifier("VARIABLE")
    );

    assert_tokenizes_as!(
        r###"export Q="""###,
        T![export],
        ' ',
        TokenMock::identifier("Q"),
        T![=],
        TokenMock::double_quote_token("")
    );
}

#[test]
pub fn keywords_not_followed_by_spaces() {
    assert_tokenizes_as!(
        "addhandler def_my_handler_and_not_a_python_function",
        T![addhandler],
        ' ',
        TokenMock::directive_arg("def_my_handler_and_not_a_python_function")
    );
    assert_tokenizes_as!(
        "before_the_rain = 'value'",
        TokenMock::identifier("before_the_rain"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("value")
    );
    assert_tokenizes_as!(
        "python_snake ??= 'friend'",
        TokenMock::identifier("python_snake"),
        ' ',
        T![??=],
        ' ',
        TokenMock::single_quote_token("friend")
    );
}

#[test]
pub fn unset_keyword() {
    assert_tokenizes_as!(
        "unset VARIABLE",
        T![unset],
        ' ',
        TokenMock::identifier("VARIABLE")
    );
    assert_tokenizes_as!(
        "unset my_var[flag]",
        T![unset],
        ' ',
        TokenMock::identifier("my_var"),
        TokenMock::varflag("flag")
    );
}

#[test]
fn basic_addtask() {
    assert_tokenizes_as!(
        "addtask do_compile",
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile")
    );
    assert_tokenizes_as!(
        "addtask do_compile do_patch",
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile"),
        ' ',
        TokenMock::directive_arg("do_patch")
    );
    assert_tokenizes_as!(
        "addtask do_compile after do_configure",
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile"),
        ' ',
        T![after],
        ' ',
        TokenMock::directive_arg("do_configure")
    );
    assert_tokenizes_as!(
        "addtask do_compile before do_configure",
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile"),
        ' ',
        T![before],
        ' ',
        TokenMock::directive_arg("do_configure")
    );
}

#[test]
pub fn addtask_before_and_after() {
    assert_tokenizes_as!(
        "addtask do_compile after do_patch before do_build",
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile"),
        ' ',
        T![after],
        ' ',
        TokenMock::directive_arg("do_patch"),
        ' ',
        T![before],
        ' ',
        TokenMock::directive_arg("do_build")
    );

    assert_tokenizes_as!(
        "addtask do_compile after do_patch do_configure before do_build",
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile"),
        ' ',
        T![after],
        ' ',
        TokenMock::directive_arg("do_patch"),
        ' ',
        TokenMock::directive_arg("do_configure"),
        ' ',
        T![before],
        ' ',
        TokenMock::directive_arg("do_build")
    );
}

#[test]
pub fn addtask_mixed_identifier() {
    assert_tokenizes_as!(
        r##"
addtask do_compile after do_patch before do_build
VAR = "value"
"##,
        '\n',
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile"),
        ' ',
        T![after],
        ' ',
        TokenMock::directive_arg("do_patch"),
        ' ',
        T![before],
        ' ',
        TokenMock::directive_arg("do_build"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("value"),
        '\n'
    );
}

#[test]
fn addtask_line_continuation() {
    assert_tokenizes_as!(
        r##"
addtask do_compile after \
do_patch"##,
        '\n',
        T![addtask],
        ' ',
        TokenMock::directive_arg("do_compile"),
        ' ',
        T![after],
        ' ',
        TokenMock::escaped_newline(),
        TokenMock::directive_arg("do_patch")
    );
}
