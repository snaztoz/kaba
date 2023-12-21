use logos::Span;

pub fn get_line_row_and_col_from_span(program: &str, span: &Span) -> (String, usize, usize) {
    let left_of_selected = &program[..span.start];
    let right_of_selected = &program[span.end..];

    let line_start = left_of_selected
        .rfind('\n')
        .map(|pos| pos + 1) // don't include the newline character
        .unwrap_or(0);

    let next_newline = right_of_selected.find('\n').map(|pos| span.end + pos); // offset

    let line = match next_newline {
        Some(newline) => &program[line_start..newline],
        None => &program[line_start..],
    };

    let row = left_of_selected.matches('\n').count() + 1;
    let col = span.start - line_start + 1;

    (String::from(line), row, col)
}

pub fn pad_white_spaces(n: usize) -> String {
    (0..n).map(|_| " ").collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_find_line_row_and_col_from_span() {
        let input = indoc! {"
            var x = 50;
            x = x + 50;
            print(x);
        "};

        let cases = [
            // (span, expected_line, expected_row, expected_col)
            (4..5, String::from("var x = 50;"), 1, 5),
            (16..22, String::from("x = x + 50;"), 2, 5),
            (24..29, String::from("print(x);"), 3, 1),
        ];

        for (span, expected_line, expected_row, expected_col) in cases {
            let (line, row, col) = get_line_row_and_col_from_span(input, &span);

            assert_eq!(line, expected_line);
            assert_eq!(row, expected_row);
            assert_eq!(col, expected_col);
        }
    }
}
