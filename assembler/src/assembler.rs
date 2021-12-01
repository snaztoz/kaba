use super::bytecode::Bytecode;

pub fn assemble(assembly: &str) -> Result<Vec<Bytecode>, ()> {
    todo!();
}

struct AssemblingResult {
    bytecode: Vec<Bytecode>,
}

impl AssemblingResult {
    fn new() -> Self {
        let mut ar = Self { bytecode: vec![] };
        ar.add_header_string();

        ar
    }

    fn add_header_string(&mut self) {
        let magic_bytes = "KABA BYTECODE".as_bytes();
        let version_bytes = &self.get_version_bytes();
        let mut header = [magic_bytes, version_bytes].concat();

        self.bytecode.extend_from_slice(&mut header);
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
        let ar = AssemblingResult::new();

        assert_eq!(ar.bytecode.len(), 16usize);
    }
}
