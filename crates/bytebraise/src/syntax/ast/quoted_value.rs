use std::borrow::Cow;
use std::fmt::Debug;
use std::ops::Range;

use cow_utils::CowUtils;
use derive_builder::Builder;
use itertools::Itertools;
use nested_intervals::IntervalSet;
use rowan::{TextRange, TextSize};

use crate::syntax::ast::tokens::{DoubleQuotedValue, SingleQuotedValue};
use crate::syntax::ast::AstToken;
use crate::syntax::make;
use crate::syntax::syntax_kind::SyntaxKind;
use crate::syntax::syntax_node::SyntaxToken;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum QuotedValue {
    SingleQuoted(SingleQuotedValue),
    DoubleQuoted(DoubleQuotedValue),
}

impl AstToken for QuotedValue {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        match token {
            SyntaxKind::DoubleQuotedValue | SyntaxKind::SingleQuotedValue => true,
            _ => false,
        }
    }

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        let ret = match syntax.kind() {
            SyntaxKind::DoubleQuotedValue => {
                QuotedValue::DoubleQuoted(DoubleQuotedValue { syntax })
            }
            SyntaxKind::SingleQuotedValue => {
                QuotedValue::SingleQuoted(SingleQuotedValue { syntax })
            }
            _ => return None,
        };

        Some(ret)
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            QuotedValue::DoubleQuoted(it) => &it.syntax,
            QuotedValue::SingleQuoted(it) => &it.syntax,
        }
    }
}

impl QuotedValue {
    pub fn text_range_between_quotes(&self) -> TextRange {
        let range = self.syntax().text_range();
        let quote_size = TextSize::of("\"");
        let (start, end) = (range.start() + quote_size, range.end() - quote_size);
        TextRange::new(start, end)
    }

    pub fn raw_value(&self) -> &str {
        &self.text()[self.text_range_between_quotes() - self.syntax().text_range().start()]
    }

    pub fn lines(&self) -> impl Iterator<Item = &str> {
        self.line_ranges().into_iter().map(move |range| {
            let start_offset = self.syntax().text_range().start();
            let absolute_range = range - start_offset;
            &self.text()[absolute_range]
        })
    }

    pub fn sorted_lines(&self) -> Self {
        let vals = self
            .lines()
            .filter_map(|v| match v.trim() {
                v if v.is_empty() => None,
                v => Some(v.to_owned()),
            })
            .sorted()
            .collect::<Vec<_>>();
        make::quoted_value_from_slice(&vals)
    }

    /// Value text (between the quotes) with escaped newlines removed
    pub fn value(&self) -> Cow<str> {
        // Handle escaped newlines
        self.raw_value().cow_replace("\\\n", "")
    }

    ///
    pub fn line_ranges(&self) -> Vec<TextRange> {
        // Absolute range of the text between the quotes
        let range = self.text_range_between_quotes();

        // Absolute position of the opening quote value
        let start_offset = self.syntax().text_range().start();

        // Relative range of the text between the quotes, i.e. 1..
        let relative_range =
            TextRange::new(range.start() - start_offset, range.end() - start_offset);
        assert_eq!(u32::from(relative_range.start()), 1);
        assert_eq!(
            u32::from(relative_range.end()),
            self.text().len() as u32 - 1
        );

        let escaped_newline_size = TextSize::of("\\\n");

        // Produce relative (to the text between quotes) ranges of each escaped newline
        let intervals = self
            .raw_value()
            .match_indices("\\\n")
            .map(|m| {
                // Add 1 because `match_indices` produces indices starting at 0, but the string
                // we are searching is at an offset of 1.
                let start = m.0 as u32 + 1;
                start..start + u32::from(escaped_newline_size)
            })
            .collect::<Vec<Range<u32>>>();

        let interval = IntervalSet::new(intervals.as_slice()).unwrap();

        // Invert the set to produce a set of relative ranges of the unescaped content, then
        // transform them to be absolute ranges
        let ret = interval
            .invert(
                u32::from(relative_range.start()),
                u32::from(relative_range.end()),
            )
            .iter()
            .map(|(range, _)| {
                TextRange::new(
                    TextSize::from(range.start) + start_offset,
                    TextSize::from(range.end) + start_offset,
                )
            })
            .collect::<Vec<_>>();
        ret
    }
}

#[derive(Debug, Builder, Clone)]
#[builder(setter(into))]
pub struct QuotedValueFormat {
    packing: SubValuePacking,
    closing_quote_on_own_line: bool,
    alignment: SubValueAlignment,
}

impl Default for QuotedValueFormat {
    fn default() -> Self {
        Self {
            alignment: SubValueAlignment::default(),
            closing_quote_on_own_line: true,
            packing: SubValuePacking::default(),
        }
    }
}

/// Controls how to layout multiple space-separated values.
#[derive(Debug, Default, Clone)]
pub enum SubValuePacking {
    Off,
    #[default]
    OnePerLine,
    Wrapped {
        line_width: usize,
    },
}

#[derive(Debug, Clone)]
pub enum SubValueAlignment {
    WithOperator,
    WithFirstSubValue,
    Indented { indent: usize },
}

impl Default for SubValueAlignment {
    fn default() -> Self {
        SubValueAlignment::Indented { indent: 4 }
    }
}

pub struct QuotedValueBuilder {
    sub_values: Vec<String>,
}

fn ff() {
    let _b = QuotedValueFormatBuilder::default()
        .alignment(SubValueAlignment::WithFirstSubValue)
        .build();
}
