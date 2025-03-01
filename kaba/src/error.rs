use colored::Colorize;
use kaba_compiler::Span;
use std::{cmp, fmt::Display, path::Path};

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
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

    fn position(&self) -> (Vec<&str>, Row, Col) {
        let src = self.as_compilation_error_src();
        let span = self.as_compilation_error_span().unwrap();

        let src_before_span = &src[..span.start];
        let src_after_span = &src[span.end..];

        let line_start = src_before_span
            .rfind('\n')
            .map(|pos| pos + 1) // don't include the newline character
            .unwrap_or(0);

        let line_end = src_after_span.find('\n').map(|pos| span.end + pos); // offset

        let err_src = match line_end {
            Some(newline) => &src[line_start..newline],
            None => &src[line_start..],
        };

        let lines = err_src.split('\n').collect::<Vec<_>>();
        let row = src_before_span.matches('\n').count() + 1;
        let col = span.start - line_start + 1;

        (lines, row, col)
    }

    fn is_compilation_error(&self) -> bool {
        matches!(self, Self::CompilationError { .. })
    }

    fn as_compilation_error_path(&self) -> Option<&Path> {
        if let Error::CompilationError { path, .. } = self {
            *path
        } else {
            unreachable!()
        }
    }

    fn as_compilation_error_src(&self) -> &str {
        if let Error::CompilationError { src, .. } = self {
            src
        } else {
            unreachable!()
        }
    }

    fn as_compilation_error_span(&self) -> Option<&Span> {
        if let Error::CompilationError { span, .. } = self {
            span.as_ref()
        } else {
            unreachable!()
        }
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = "error:".bright_red().bold();
        let message = &self.message();

        if !self.is_compilation_error() {
            return write!(f, "{label} {message}");
        }

        let path = self.as_compilation_error_path();
        let src = self.as_compilation_error_src();
        let span = self.as_compilation_error_span().unwrap();

        let (lines, row, col) = self.position();
        let file_path = path.as_ref().unwrap().display();

        // Write header
        writeln!(f, "{label} {file_path} ({row}:{col})")?;

        let widest_row_width = cmp::max(
            (row + lines.len() - 1).to_string().len(),
            2, // the ".." characters
        );

        // Write empty line
        if row == 1 {
            writeln!(f, " {:>width$} |", "", width = widest_row_width)?;
        } else {
            writeln!(f, " {:>width$} |", "..", width = widest_row_width)?;
        }

        if lines.len() == 1 {
            let line = lines[0];

            // Write a single line with error highlighter
            writeln!(f, " {:>width$} | {}", row, line, width = widest_row_width)?;

            let highlight_line_len = span.clone().count();
            let highlight_line = create_highlight_line(highlight_line_len)
                .bright_red()
                .bold();

            // Write highlighter line
            writeln!(
                f,
                " {:>width$} | {:>col_width$}{highlight_line}",
                "",
                "",
                width = widest_row_width,
                col_width = col - 1,
            )?;
        } else {
            for (i, line) in lines.iter().enumerate() {
                writeln!(
                    f,
                    " {:>width$} | {line}",
                    (row + i).to_string().bright_red(),
                    width = widest_row_width
                )?;
            }
        }

        // Write empty line
        if src[span.end..].find('\n').is_none() {
            writeln!(f, " {:>width$} |", "", width = widest_row_width)?;
        } else {
            writeln!(f, " {:>width$} |", "..", width = widest_row_width)?;
        }

        // Write message
        writeln!(f, " {:>width$} = {message}", "", width = widest_row_width)
    }
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

            assert_eq!(&line, &[expected_line]);
            assert_eq!(row, expected_row);
            assert_eq!(col, expected_col);
        }
    }
}
