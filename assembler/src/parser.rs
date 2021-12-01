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
}
