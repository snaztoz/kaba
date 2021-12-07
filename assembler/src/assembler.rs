use super::{
    bytecode::{Bytecode, EOI_BYTECODE},
    parser::{KabaAsmParser, Rule},
};
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use std::collections::HashMap;

pub fn assemble(assembly: &str) -> Result<Vec<Bytecode>, ()> {
    let mut assembler = Assembler::new();
    assembler.assemble(assembly);

    Ok(assembler.result)
}

struct Assembler {
    result: Vec<Bytecode>,

    labels_mapping: HashMap<String, usize>,
    unmapped_labels: Vec<String>,
}

impl Assembler {
    fn new() -> Self {
        let mut assembler = Self {
            result: vec![],
            labels_mapping: HashMap::new(),
            unmapped_labels: vec![],
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
                Rule::assembly_line => self.assemble_line(line.into_inner()),
                Rule::EOI => (),
                _ => unreachable!(),
            }
        }

        self.push_instruction_bytecode(EOI_BYTECODE);
    }

    fn assemble_line(&mut self, assembly_line: Pairs<Rule>) {
        let mut assembly_line = assembly_line;
        let line = assembly_line.next().unwrap();

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
        let label = label.as_str();
        self.unmapped_labels.push(String::from(label));
    }

    fn push_instruction_bytecode(&mut self, bytecode: Bytecode) {
        self.result.push(bytecode);
        self.map_labels();
    }

    fn map_labels(&mut self) {
        let newest_instruction_pos = self.result.len() - 1;
        for label in self.unmapped_labels.drain(..) {
            self.labels_mapping.insert(label, newest_instruction_pos);
        }
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
        let header_size = assembler.result.len();
        let assembly = testutils::read_input_file("assembling/label");

        assembler.assemble(&assembly);

        assert_eq!(assembler.result.len(), header_size + 1);
        assert!(assembler.unmapped_labels.is_empty());

        let assembly_eoi_pos = assembler.result.len() - 1;
        for label in ["main", "foo"] {
            assert_eq!(assembler.labels_mapping[label], assembly_eoi_pos);
        }
    }
}
