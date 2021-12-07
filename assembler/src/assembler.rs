use super::{
    bytecode::{Bytecode, EOI_BYTECODE},
    parser::{KabaAsmParser, Rule},
};
use pest::{iterators::Pair, Parser};
use std::collections::HashMap;

pub fn assemble(assembly: &str) -> Result<Vec<Bytecode>, ()> {
    let mut assembler = Assembler::new();
    assembler.assemble(assembly);

    Ok(assembler.result)
}

struct Assembler {
    result: Vec<Bytecode>,

    labels_mapping: HashMap<String, usize>,
}

impl Assembler {
    fn new() -> Self {
        let mut assembler = Self {
            result: vec![],
            labels_mapping: HashMap::new(),
        };

        assembler.add_header_string();

        assembler
    }

    fn add_header_string(&mut self) {
        let magic_bytes = "KABA BYTECODE".as_bytes();
        let version_bytes = &self.get_version_bytes();
        let header = [magic_bytes, version_bytes].concat();

        self.result.extend_from_slice(&header);
    }

    fn get_version_bytes(&self) -> [u8; 3] {
        [
            env!("CARGO_PKG_VERSION_MAJOR").parse::<u8>().unwrap(),
            env!("CARGO_PKG_VERSION_MINOR").parse::<u8>().unwrap(),
            env!("CARGO_PKG_VERSION_PATCH").parse::<u8>().unwrap(),
        ]
    }

    fn assemble(&mut self, assembly: &str) {
        // for now just panic to handle parsing error
        let parsed_assembly = KabaAsmParser::parse(Rule::assembly, assembly)
            .unwrap()
            .next()
            .unwrap();

        for line in parsed_assembly.into_inner() {
            match line.as_rule() {
                Rule::assembly_line => {
                    let assembly_line = line.into_inner().next().unwrap();
                    self.assemble_line(assembly_line);
                }

                Rule::EOI => (),
                _ => unreachable!(),
            }
        }

        self.result.push(EOI_BYTECODE);
    }

    fn assemble_line(&mut self, line: Pair<Rule>) {
        match line.as_rule() {
            Rule::label_line => {
                let label = line.into_inner().next().unwrap();
                self.assemble_label(label);
            }
            Rule::instruction_line => todo!(),
            _ => unreachable!(),
        }
    }

    fn assemble_label(&mut self, label: Pair<Rule>) {
        let label = label.as_str().to_string();
        let next_instruction_pos = self.result.len();

        if self.labels_mapping.contains_key(&label) {
            // duplicated labels error handling
            todo!()
        }

        self.labels_mapping.insert(label, next_instruction_pos);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils;

    #[test]
    fn check_header_string() {
        let assembler = Assembler::new();

        assert_eq!(assembler.result.len(), 16usize);
    }

    #[test]
    fn assemble_labels() {
        let mut assembler = Assembler::new();
        let assembly = testutils::read_test_file("assembling/label");

        assembler.assemble(&assembly);

        let eoi_pos = assembler.result.len() - 1;
        for label in ["main", "foo"] {
            assert_eq!(assembler.labels_mapping[label], eoi_pos);
        }
    }

    #[test]
    #[should_panic]
    fn assemble_duplicated_labels() {
        let mut assembler = Assembler::new();
        let assembly = testutils::read_test_file("assembling/duplicated_labels");

        assembler.assemble(&assembly);
    }
}
