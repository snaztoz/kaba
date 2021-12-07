#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct KabaAsmParser;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils;
    use pest::{self, consumes_to, Parser};

    #[test]
    fn parse_assembly() {
        let input = testutils::read_input_file("parse/test_assembly");

        pest::parses_to! {
            parser: KabaAsmParser,
            input: &input,
            rule: Rule::assembly,
            tokens: [
                assembly(0, 73, [
                    assembly_line(29, 34, [
                        label_line(29, 34, [
                            identifier(29, 33),
                        ])
                    ]),
                    assembly_line(39, 49, [
                        instruction_line(39, 49, [
                            identifier(39, 42),
                            expr(43, 45, [
                                number(43, 45)
                            ]),
                            expr(47, 49, [
                                number(47, 49)
                            ]),
                        ])
                    ]),
                    assembly_line(51, 55, [
                        label_line(51, 55, [
                            identifier(51, 54),
                        ])
                    ]),
                    assembly_line(60, 72, [
                        instruction_line(60, 72, [
                            identifier(60, 63),
                            expr(64, 67, [
                                identifier(64, 67),
                            ]),
                            expr(69, 72, [
                                identifier(69, 72),
                            ]),
                        ])
                    ]),
                    EOI(73, 73),
                ]),
            ]
        };
    }

    #[test]
    fn parse_label() {
        let input = testutils::read_input_file("parse/test_label");

        pest::parses_to! {
            parser: KabaAsmParser,
            input: &input,
            rule: Rule::label_line,
            tokens: [
                label_line(0, 5, [
                    identifier(0, 4),
                ]),
            ]
        };
    }

    #[test]
    fn parse_instruction() {
        let input = testutils::read_input_file("parse/test_instruction");

        pest::parses_to! {
            parser: KabaAsmParser,
            input: &input,
            rule: Rule::instruction_line,
            tokens: [
                instruction_line(0, 20, [
                    identifier(0, 9),
                    expr(10, 11, [
                        number(10, 11),
                    ]),
                    expr(13, 15, [
                        number(13, 15),
                    ]),
                    expr(17, 20, [
                        identifier(17, 20),
                    ]),
                ]),
            ]
        };
    }

    #[test]
    fn parse_identifier() {
        let inputs = ["a12bcd_ef", "__", "_123", "a"];

        for input in &inputs {
            let tok_start = 0;
            let tok_end = input.len();

            pest::parses_to! {
                parser: KabaAsmParser,
                input: input,
                rule: Rule::identifier,
                tokens: [identifier(tok_start, tok_end)]
            }
        }

        // invalid identifier
        let parsing_result = KabaAsmParser::parse(Rule::identifier, "_");

        assert!(parsing_result.is_err());
    }

    #[test]
    fn parse_with_no_newline_at_the_last_line() {
        let input = testutils::read_input_file("parse/test_no_newline");

        pest::parses_to! {
            parser: KabaAsmParser,
            input: &input,
            rule: Rule::assembly,
            tokens: [
                assembly(0, 18, [
                    assembly_line(0, 5, [
                        label_line(0, 5, [
                            identifier(0, 4),
                        ]),
                    ]),
                    assembly_line(10, 18, [
                        instruction_line(10, 18, [
                            identifier(10, 13),
                            expr(14, 15, [
                                number(14, 15),
                            ]),
                            expr(17, 18, [
                                number(17, 18),
                            ]),
                        ]),
                    ]),
                    EOI(18, 18),
                ]),
            ]
        };
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
