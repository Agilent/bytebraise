#[cfg(test)]
use crate::lexer::token::T;

#[cfg(test)]
use crate::assert_tokenizes_as;
#[cfg(test)]
use crate::lexer::tests::mock::TokenMock;

mod keywords;
mod mock;
mod quoted_values;

#[test]
pub fn basic_comments() {
    assert_tokenizes_as!("# test", TokenMock::comment("# test"));
    assert_tokenizes_as!("# ", TokenMock::comment("# "));
    assert_tokenizes_as!("#", TokenMock::comment("#"));
    assert_tokenizes_as!("##", TokenMock::comment("##"));
}

#[test]
pub fn continued_comment() {
    let input = r###"
# comment that \
# continues :)"###;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::comment("# comment that "),
        TokenMock::escaped_newline(),
        TokenMock::comment("# continues :)")
    );
}

#[test]
pub fn newline_continuation() {
    assert_tokenizes_as!(
        r###"\
"###,
        TokenMock::escaped_newline()
    );
}

#[test]
pub fn more_comments() {
    let input = r###"
# comment

# another :)"###;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::comment("# comment"),
        '\n',
        '\n',
        TokenMock::comment("# another :)")
    );
}

#[test]
pub fn assignment_operators() {
    let input = r###"VAR = "10"
VAR ?= "20"
VAR ??= "300 :)"
VAR += "plus equals"
VAR =+ "equals plus"
VAR := "immediate"
VAR .= "dot equals"
VAR =. "equals dot"
"###;
    assert_tokenizes_as!(
        input,
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("10"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![?=],
        ' ',
        TokenMock::double_quote_token("20"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![??=],
        ' ',
        TokenMock::double_quote_token("300 :)"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![+=],
        ' ',
        TokenMock::double_quote_token("plus equals"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=+],
        ' ',
        TokenMock::double_quote_token("equals plus"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![:=],
        ' ',
        TokenMock::double_quote_token("immediate"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![.=],
        ' ',
        TokenMock::double_quote_token("dot equals"),
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=.],
        ' ',
        TokenMock::double_quote_token("equals dot"),
        '\n'
    );
}

#[test]
pub fn indented_assignments() {
    let input = r###"MY_VAR = " \
    some content \
    "
"###;

    assert_tokenizes_as!(
        input,
        TokenMock::identifier("MY_VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \\\n    some content \\\n    "),
        '\n'
    );
}

#[test]
pub fn varflags() {
    assert_tokenizes_as!(
        "q[a] = 'ok then buddy'",
        TokenMock::identifier("q"),
        TokenMock::varflag("a"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("ok then buddy")
    );

    assert_tokenizes_as!(
        "VAR[underscore_chars_] = 'CONTENTS'",
        TokenMock::identifier("VAR"),
        TokenMock::varflag("underscore_chars_"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("CONTENTS")
    );
}

#[test]
pub fn identifier_pn() {
    assert_tokenizes_as!(
        "RDEPENDS_${PN} += \"dep dep\"",
        TokenMock::identifier("RDEPENDS_${PN}"),
        ' ',
        T![+=],
        ' ',
        TokenMock::double_quote_token("dep dep")
    );

    assert_tokenizes_as!(
        "RDEPENDS_${PN}[flag] = \"why would you do this\"",
        TokenMock::identifier("RDEPENDS_${PN}"),
        TokenMock::varflag("flag"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("why would you do this")
    );
}

#[test]
pub fn def_function() {
    let input = r###"def a():
    print("OK")"###;
    assert_tokenizes_as!(
        input,
        T![def],
        ' ',
        TokenMock::python_def_function_name("a"),
        TokenMock::python_def_function_args("()"),
        T![:],
        TokenMock::python_def_function_body("\n    print(\"OK\")")
    );
}

#[test]
pub fn def_function_termination() {
    let input = r###"def build_epoch(d):
    epoch = d.getVar('SOURCE_DATE_EPOCH') or "-1"
    return '-Dtime-epoch=%d' % int(epoch)
PACKAGECONFIG[set-time-epoch] = "${@build_epoch(d)},-Dtime-epoch=0""###;
    assert_tokenizes_as!(input,
       T![def],
        ' ',
        TokenMock::python_def_function_name("build_epoch"),
        TokenMock::python_def_function_args("(d)"),
        T![:],
        TokenMock::python_def_function_body("\n    epoch = d.getVar('SOURCE_DATE_EPOCH') or \"-1\"\n    return '-Dtime-epoch=%d' % int(epoch)"),
        '\n',
       TokenMock::identifier("PACKAGECONFIG"),
       TokenMock::varflag("set-time-epoch"),
        ' ', T![=], ' ', TokenMock::double_quote_token("${@build_epoch(d)},-Dtime-epoch=0")
        );
}

#[test]
pub fn simple_tasks() {
    let input = r###"do_compile() {
    make || whatever
}"###;
    assert_tokenizes_as!(
        input,
        TokenMock::identifier("do_compile"),
        T!['('],
        T![')'],
        ' ',
        TokenMock::task("{\n    make || whatever\n}")
    );

    let input = r###"
do_compile() {
    make || whatever
}
"###;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("do_compile"),
        T!['('],
        T![')'],
        ' ',
        TokenMock::task("{\n    make || whatever\n}"),
        '\n'
    );
}

#[test]
pub fn grapheme_boundaries() {
    let input = r##"
RECIPE_MAINTAINER_pn-apt = "Aníbal Limón <limon.anibal@gmail.com>"
RECIPE_MAINTAINER_pn-apt-native = "Aníbal Limón <limon.anibal@gmail.com>"
"##;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("RECIPE_MAINTAINER_pn-apt"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("Aníbal Limón <limon.anibal@gmail.com>"),
        '\n',
        TokenMock::identifier("RECIPE_MAINTAINER_pn-apt-native"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("Aníbal Limón <limon.anibal@gmail.com>"),
        '\n'
    );
}

#[test]
pub fn dam() {
    let input = r##"
# Míke
ABC = "test""##;

    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::comment("# Míke"),
        '\n',
        TokenMock::identifier("ABC"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("test")
    );
}

#[test]
pub fn ambiguous_comment() {
    // TODO implement detection of ambiguous comment in parser
    let input = r##"
# This is a comment \

"##;

    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::comment("# This is a comment "),
        TokenMock::escaped_newline(),
        '\n'
    );
}

#[test]
fn comment_with_escape_sequences() {
    let input = r##"
# This is a comment \t"##;

    assert_tokenizes_as!(input, '\n', TokenMock::comment("# This is a comment \\t"));
}

#[test]
fn fragment_from_base_bbclass() {
    let input = r##"PACKAGECONFIG_CONFARGS ??= ""

def oe_import(d):
    import sys

    bbpath = d.getVar("BBPATH").split(":")
"##;

    assert_tokenizes_as!(
        input,
        TokenMock::identifier("PACKAGECONFIG_CONFARGS"),
        ' ',
        T![??=],
        ' ',
        TokenMock::double_quote_token(""),
        '\n',
        '\n',
        T![def],
        ' ',
        TokenMock::python_def_function_name("oe_import"),
        TokenMock::python_def_function_args("(d)"),
        T![:],
        TokenMock::python_def_function_body(
            "\n    import sys\n\n    bbpath = d.getVar(\"BBPATH\").split(\":\")"
        ),
        '\n'
    );
}
