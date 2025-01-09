use colored::Colorize;
use indoc::writedoc;
use kabac::Span;
use std::{fmt::Display, path::Path};

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
type Line = String;
type Row = usize;
type Col = usize;

pub enum Error<'a> {
    InvalidExtension,
    FileNotExist {
        path: &'a Path,
    },
    CompilationError {
        path: Option<&'a Path>,
        src: &'a str,
        message: String,
        span: Option<Span>,
    },
}

impl Error<'_> {
    fn message(&self) -> String {
        match self {
            Self::InvalidExtension => {
                String::from("Kaba source code file must have '.kaba' extension")
            }
            Self::FileNotExist { path } => {
                format!("file '{}' does not exist", path.display())
            }
            Self::CompilationError { message, .. } => message.to_string(),
        }
    }

    fn position(&self) -> (Line, Row, Col) {
        let (src, span) = if let Error::CompilationError { src, span, .. } = self {
            (src, span)
        } else {
            unreachable!()
        };

        let span = span.as_ref().unwrap();
        let before_span = &src[..span.start];
        let after_span = &src[span.end..];

        let line_start = before_span
            .rfind('\n')
            .map(|pos| pos + 1) // don't include the newline character
            .unwrap_or(0);

        let next_newline = after_span.find('\n').map(|pos| span.end + pos); // offset

        let line = match next_newline {
            Some(newline) => &src[line_start..newline],
            None => &src[line_start..],
        };

        let row = before_span.matches('\n').count() + 1;
        let col = span.start - line_start + 1;

        (String::from(line), row, col)
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = "error:".bright_red().bold();
        let message = &self.message();

        if let Self::CompilationError { path, span, .. } = self {
            let (line, row, col) = self.position();
            let file_path = path.as_ref().unwrap().display();
            let position_information = format!("{file_path} ({row}:{col})");

            let row_number_pad = pad_white_spaces(row.to_string().len());
            let col_pad = pad_white_spaces(col - 1);
            let highlight_line_len = span.as_ref().unwrap().clone().count();
            let highlight_line = create_highlight_line(highlight_line_len)
                .bright_red()
                .bold();

            writedoc!(
                f,
                "
                {label} {position_information}
                 {row_number_pad} |
                 {row} | {line}
                 {row_number_pad} | {col_pad}{highlight_line}
                 {row_number_pad} |
                 {row_number_pad} = {message}",
            )
        } else {
            write!(f, "{label} {message}")
        }
    }
}

fn pad_white_spaces(n: usize) -> String {
    (0..n).map(|_| " ").collect::<String>()
}

fn create_highlight_line(n: usize) -> String {
    let mut line = String::from("^");
    line.push_str(&(1..n).map(|_| "~").collect::<String>());
    line
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
            let error = Error::CompilationError {
                path: None,
                src: input,
                span: Some(span),
                message: String::from("ERROR TEST"),
            };

            let (line, row, col) = error.position();

            assert_eq!(line, expected_line);
            assert_eq!(row, expected_row);
            assert_eq!(col, expected_col);
        }
    }
}
