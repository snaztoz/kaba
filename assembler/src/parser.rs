#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct KabaAsmParser;

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    #[test]
    fn parse_assembly() {
        let input = "\
            // this is a header comment

            main:
                foo 5
                bar 10, 15

            baz:
                bat abc, def
        ";

        let parsing_result = KabaAsmParser::parse(Rule::assembly, input);

        assert!(parsing_result.is_ok());
    }

    #[test]
    fn parse_label() {
        let input = "\
            main: // a comment after label
        ";

        let parsing_result = KabaAsmParser::parse(Rule::label_line, input);

        assert!(parsing_result.is_ok());
    }

    #[test]
    fn parse_instruction() {
        let input = "\
            something 5, 10, abc  // a comment after instruction
        ";

        let parsing_result = KabaAsmParser::parse(Rule::instruction_line, input);

        assert!(parsing_result.is_ok());
    }

    #[test]
    fn parse_identifier() {
        let inputs = ["a12bcd_ef", "__", "_123", "a"];

        for input in &inputs {
            let parsing_result = KabaAsmParser::parse(Rule::identifier, input);

            assert!(parsing_result.is_ok());
        }

        // invalid identifier
        let parsing_result = KabaAsmParser::parse(Rule::identifier, "_");

        assert!(parsing_result.is_err());
    }

    #[test]
    fn parse_with_no_newline_at_the_last_line() {
        let input = "\
            main:
                foo 1, 3";

        let parsing_result = KabaAsmParser::parse(Rule::assembly, input);

        assert!(parsing_result.is_ok());
    }

    #[test]
    fn parse_multiple_in_one_line() {
        let inputs = ["main: foo:\n", "foo 1, 2 bar 3, 4\n"];

        for input in inputs {
            let parsing_result = KabaAsmParser::parse(Rule::assembly, input);

            assert!(parsing_result.is_err());
        }
    }
}
