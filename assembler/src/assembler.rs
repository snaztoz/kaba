use super::bytecode::Bytecode;

pub fn assemble(assembly: &str) -> Result<Vec<Bytecode>, ()> {
    todo!();
}

struct Assembler {
    result: Vec<Bytecode>,
}

impl Assembler {
    fn new() -> Self {
        let mut assembler = Self { result: vec![] };
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_header_string() {
        let assembler = Assembler::new();

        assert_eq!(assembler.result.len(), 16usize);
    }
}
