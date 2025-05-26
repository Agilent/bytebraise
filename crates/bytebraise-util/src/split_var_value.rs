use itertools::Itertools;
use rowan::{TextRange, TextSize};

pub fn split_var_value3(s: &str) -> Vec<TextRange> {
    struct SplitVarValueInternalItem {
        inexpr: u32,
        char_pos: usize,
        char: char,
    }

    #[derive(Default)]
    struct SplitVarValueState {
        previous_char: Option<char>,
        inexpr: u32,
    }

    let iter = s
        .char_indices()
        // This `scan` tracks whether we are inside a ${} expression
        .scan(
            SplitVarValueState::default(),
            |state: &mut SplitVarValueState, b| {
                match b.1 {
                    '{' if matches!(state.previous_char, Some('$')) => state.inexpr += 1,
                    '}' => state.inexpr = state.inexpr.saturating_sub(1),
                    _ => {}
                }

                state.previous_char = Some(b.1);

                Some(SplitVarValueInternalItem {
                    inexpr: state.inexpr,
                    char_pos: b.0,
                    char: b.1,
                })
            },
        )
        // Whitespace outside of a ${} expression is what separates each value
        .chunk_by(|item| item.char.is_whitespace() && (item.inexpr == 0));

    let mut ret = vec![];
    for q in &iter {
        let group = q.1.collect::<Vec<_>>();
        if group.iter().all(|item| item.char.is_whitespace()) {
            continue;
        }

        let range_start = group.first().unwrap().char_pos;
        let range_end = group.last().unwrap().char_pos + group.last().unwrap().char.len_utf8();
        ret.push(TextRange::new(
            TextSize::from(range_start as u32),
            TextSize::from(range_end as u32),
        ));
    }
    ret
}

#[cfg(test)]
mod test {
    use crate::split_var_value::split_var_value3;

    #[test]
    fn empty() {}

    #[test]
    fn test_split_3() {
        let input = "  A B C  D E F ${'Q B P'} G";
        let ret = split_var_value3(input);

        let parts = ret
            .into_iter()
            .map(|range| &input[range])
            .collect::<Vec<_>>();

        assert_eq!(parts, &["A", "B", "C", "D", "E", "F", "${'Q B P'}", "G"]);
    }
}
