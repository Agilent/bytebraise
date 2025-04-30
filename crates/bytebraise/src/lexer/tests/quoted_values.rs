#[cfg(test)]
use crate::assert_tokenizes_as;
#[cfg(test)]
use crate::lexer::tests::{TokenMock, T};

#[test]
pub fn simple_strings() {
    assert_tokenizes_as!(
        "A = \"\"",
        TokenMock::identifier("A"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("")
    );
    assert_tokenizes_as!(
        "A  =  \"\"",
        TokenMock::identifier("A"),
        "  ",
        T![=],
        "  ",
        TokenMock::double_quote_token("")
    );
    assert_tokenizes_as!(
        "A =  \"   \"",
        TokenMock::identifier("A"),
        ' ',
        T![=],
        "  ",
        TokenMock::double_quote_token("   ")
    );
}

#[test]
pub fn simple_multiline_strings() {
    assert_tokenizes_as!(
        r###"A = "\\\n""###,
        TokenMock::identifier("A"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(r###"\\\n"###)
    );
}

#[test]
pub fn multiline_string() {
    let input = r###"q[a] = 'ok then \
        buddy'"###;
    assert_tokenizes_as!(
        input,
        TokenMock::identifier("q"),
        TokenMock::varflag("a"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("ok then \\\n        buddy")
    );
}

#[test]
pub fn multiline_string2() {
    let input = r###"Q = "ok then\
    OK \
    "
    "###;
    assert_tokenizes_as!(
        input,
        TokenMock::identifier("Q"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("ok then\\\n    OK \\\n    "),
        '\n',
        "    "
    );
}

#[test]
fn embedded_double_quotes() {
    assert_tokenizes_as!(
        r###"VAR = ""WAT"""###,
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("\"WAT\"")
    );

    assert_tokenizes_as!(
        r###"VAR = " "WAT" ""###,
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \"WAT\" ")
    );

    assert_tokenizes_as!(
        r###"VAR = """"###,
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("\"")
    );
}

#[test]
fn embedded_single_quotes() {
    assert_tokenizes_as!(
        "VAR = ''WAT''",
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("'WAT'")
    );

    assert_tokenizes_as!(
        "VAR = ' 'WAT' '",
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token(" 'WAT' ")
    );

    assert_tokenizes_as!(
        "VAR = '''",
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("'")
    );
}

#[test]
fn mixed_quotes() {
    assert_tokenizes_as!(
        r###"VAR = '"'"WAT""'"###,
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token(r####""'"WAT"""####)
    );

    assert_tokenizes_as!(
        r###"VAR = """"'"WAT""'''''''''''""###,
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(r####""""'"WAT""'''''''''''"####)
    );
}

#[test]
fn end_quote_positions() {
    // TODO for single quote as well

    let input = r###"DEPENDS_append = " \
    library1 \
    library2 \
    library3 \
""###;
    assert_tokenizes_as!(
        input,
        TokenMock::identifier("DEPENDS_append"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \\\n    library1 \\\n    library2 \\\n    library3 \\\n")
    );

    let input = r###"DEPENDS_append = " \
    library1 \
    library2 \
    library3 \
    ""###;
    assert_tokenizes_as!(
        input,
        TokenMock::identifier("DEPENDS_append"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(
            " \\\n    library1 \\\n    library2 \\\n    library3 \\\n    "
        )
    );

    let input = r###"
DEPENDS_append = " \
                   library1 \
                   library2 \
                   library3 \
                  ""###;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("DEPENDS_append"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(
            " \\\n                   library1 \\\n                   library2 \\\n                   library3 \\\n                  "
        )
    );
}

#[test]
fn multiline_embedded_quotes() {
    // TODO for single quotes too

    let input = r###"
DEPENDS_append = " \
    ${@"library1" if true else ""} \
    library2 \
    ""###;

    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("DEPENDS_append"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(
            " \\\n    ${@\"library1\" if true else \"\"} \\\n    library2 \\\n    "
        )
    );

    let input = r###"
DEPENDS_append = " \
    ${@"library1" if true else ""} \
    library2 \
""###;

    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("DEPENDS_append"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(
            " \\\n    ${@\"library1\" if true else \"\"} \\\n    library2 \\\n"
        )
    );
}

#[test]
fn more_multiline_embedded_quotes() {
    let input = r###"
VAR = " \
    library1 "'a" \
    library2 \
""###;

    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \\\n    library1 \"'a\" \\\n    library2 \\\n")
    );

    let input = r###"
VAR = " \
    library1 "'a" \
    library2 \
    ""###;

    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \\\n    library1 \"'a\" \\\n    library2 \\\n    ")
    );
}

#[test]
fn tricky_multiline_embedded_quotes_1() {
    let input = r##"
VAR = " ""\
library1 "
"##;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \"\"\\\nlibrary1 "),
        '\n'
    );
}

#[test]
fn tricky_multiline_embedded_quotes_2() {
    let input = r##"
VAR = " ""\
library1 ""
"##;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \"\"\\\nlibrary1 \""),
        '\n'
    );
}

#[test]
fn tricky_multiline_embedded_quotes_3() {
    let input = r##"
VAR = " " " \
     library1 "
"##;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \" \" \\\n     library1 "),
        '\n'
    );
}

#[test]
fn tricky_multiline_embedded_quotes_4() {
    let input = r##"
VAR = " " \
VAR2 = ""
"##;
    assert_tokenizes_as!(
        input,
        '\n',
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(" \" \\\nVAR2 = \""),
        '\n'
    );
}

// TODO reconstitute error tests

// #[test]
// fn error_bad_line_continuation() {
//     let input = r##"
// VAR = " " \
//
// VAR2 = ""
// "##;
//     assert_tokenizes_as!(
//         input,
//         '\n',
//         TokenMock::identifier("VAR"),
//         ' ',
//         T![=],
//         ' ',
//         Error(LexerError {
//             bad_input: " \" \\\n",
//             kind: LexerErrorKind::UnterminatedLineContinuation,
//             line_no: 0
//         }),
//         '\n',
//         TokenMock::identifier("VAR2"),
//         ' ',
//         T![=],
//         ' ',
//         TokenMock::double_quote_token(""),
//         '\n'
//     );
// }

// #[test]
// fn error_unclosed_string() {
//     let input = "VAR = \"";
//     assert_tokenizes_as!(
//         input,
//         TokenMock::identifier("VAR"),
//         ' ',
//         T![=],
//         ' ',
//         Error(LexerError {
//             bad_input: "",
//             kind: LexerErrorKind::UnterminatedQuotedValue,
//             line_no: 0
//         })
//     );
// }

// #[test]
// fn error_unparsed_embedded_quotes() {
//     let input = r##"
// VAR = " "\
//     library1 "
// "##;
//     assert_tokenizes_as!(
//         input,
//         '\n',
//         TokenMock::identifier("VAR"),
//         ' ',
//         T![=],
//         ' ',
//         Error(LexerError {
//             bad_input: " \"\\\n    library1 ",
//             kind: LexerErrorKind::UnparsedEmbeddedQuotes,
//             line_no: 0
//         }),
//         '\n'
//     );
// }

#[test]
fn value_with_escape_sequence() {
    let input = r##"VAR = "TAB \t""##;
    assert_tokenizes_as!(
        input,
        TokenMock::identifier("VAR"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token("TAB \\t")
    );
}

#[test]
fn whitespace_following_assignments() {
    assert_tokenizes_as!(
        "A = \"\"   ",
        TokenMock::identifier("A"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(""),
        "   "
    );

    assert_tokenizes_as!(
        "A = \"\"   \n",
        TokenMock::identifier("A"),
        ' ',
        T![=],
        ' ',
        TokenMock::double_quote_token(""),
        "   ",
        '\n'
    );

    assert_tokenizes_as!(
        r###"q[a] = 'ok then \
        buddy'  "###,
        TokenMock::identifier("q"),
        TokenMock::varflag("a"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("ok then \\\n        buddy"),
        "  "
    );

    assert_tokenizes_as!(
        "q[a] = 'ok then \\\n        buddy'  \n",
        TokenMock::identifier("q"),
        TokenMock::varflag("a"),
        ' ',
        T![=],
        ' ',
        TokenMock::single_quote_token("ok then \\\n        buddy"),
        "  ",
        '\n'
    );
}
