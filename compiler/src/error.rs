//! This module contains errors that may be thrown during
//! compiling process.

use colored::Colorize;
use indoc::writedoc;
use logos::Span;
use std::{fmt, path::Path};

type Line = String;
type Row = usize;
type Col = usize;

/// Wraps error variants that may occur during the compilation
/// process, alongside with the information of its source code
/// file path and the source code itself.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Error<'a> {
    pub path: Option<&'a Path>,
    pub src: &'a str,
    pub message: String,
    pub span: Option<Span>,
}

impl Error<'_> {
    fn get_position(&self) -> Option<(Line, Row, Col)> {
        let span = self.span.as_ref().unwrap();
        let left_of_selected = &self.src[..span.start];
        let right_of_selected = &self.src[span.end..];

        let line_start = left_of_selected
            .rfind('\n')
            .map(|pos| pos + 1) // don't include the newline character
            .unwrap_or(0);

        let next_newline = right_of_selected.find('\n').map(|pos| span.end + pos); // offset

        let line = match next_newline {
            Some(newline) => &self.src[line_start..newline],
            None => &self.src[line_start..],
        };

        let row = left_of_selected.matches('\n').count() + 1;
        let col = span.start - line_start + 1;

        Some((String::from(line), row, col))
    }

    fn pad_white_spaces(&self, n: usize) -> String {
        (0..n).map(|_| " ").collect::<String>()
    }

    fn create_highlight_line(&self, n: usize) -> String {
        let mut line = String::from("^");
        line.push_str(&(1..n).map(|_| "~").collect::<String>());
        line
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = "error:".bright_red().bold();
        let message = &self.message;

        let position = if self.span.is_some() {
            self.get_position()
        } else {
            None
        };

        if let Some((line, row, col)) = position {
            let file_path = self.path.as_ref().unwrap().display();
            let position_information = format!("{file_path} ({row}:{col})");

            let row_number_pad = self.pad_white_spaces(row.to_string().len());
            let col_pad = self.pad_white_spaces(col - 1);
            let highlighter_line = self
                .create_highlight_line(self.span.as_ref().unwrap().clone().count())
                .bright_red()
                .bold();

            writedoc!(
                f,
                "
                {label} {position_information}
                 {row_number_pad} |
                 {row} | {line}
                 {row_number_pad} | {col_pad}{highlighter_line}
                 {row_number_pad} |
                 {row_number_pad} = {message}",
            )
        } else {
            write!(f, "{label} {message}")
        }
    }
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
            let error = Error {
                src: input,
                span: Some(span),
                ..Error::default()
            };

            let (line, row, col) = error.get_position().unwrap();

            assert_eq!(line, expected_line);
            assert_eq!(row, expected_row);
            assert_eq!(col, expected_col);
        }
    }
}
