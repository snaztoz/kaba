use lazy_static::lazy_static;
use std::collections::HashMap;

pub type Bytecode = u8;
// we use 64-bit size to ensure the bytecode can be run
// in 32-bit and 64-bit platforms
pub type BytecodePtr = u64;
type Instruction = &'static str;
type Args = Option<&'static [ArgType]>;

pub enum ArgType {
    Ptr,
    Int,
    Byte,
}

impl ArgType {
    pub fn size(&self) -> u8 {
        match *self {
            ArgType::Ptr => 8,
            ArgType::Int => 4,
            ArgType::Byte => 1,
        }
    }
}

const INSTRUCTION_SET: &[(Instruction, Args)] = &[
    // Functions
    ("invoke", Some(&[ArgType::Ptr, ArgType::Int])),
    ("return", None),

    // References
    ("rpushl", Some(&[ArgType::Ptr])),
    ("rpusho", Some(&[ArgType::Ptr])),
    ("rset",   None),
    ("rload",  None),
    ("rceq",   None),
    ("rcne",   None),

    // Integers
    ("ipushl", Some(&[ArgType::Int])),
    ("ipusho", Some(&[ArgType::Int])),
    ("ipopl",  Some(&[ArgType::Int])),
    ("ipopo",  Some(&[ArgType::Int])),
    ("iset",   None),
    ("iload",  None),
    ("iadd",   None),
    ("isub",   None),
    ("imul",   None),
    ("idiv",   None),
    ("iceq",   None),
    ("icne",   None),
    ("iclt",   None),
    ("icle",   None),
    ("icgt",   None),
    ("icge",   None),

    ("moffset", Some(&[ArgType::Byte])),

    ("jmp",  Some(&[ArgType::Ptr])),
    ("jmpc", Some(&[ArgType::Ptr])),
];

const INSTRUCTION_BYTECODE_START: u8 = 0x01;

lazy_static! {
    pub static ref INSTRUCTION_SET_BYTECODE: HashMap<Instruction, Bytecode> = {
        let mut instruction_bytecodes = HashMap::new();

        let mut bytecode = INSTRUCTION_BYTECODE_START;
        for &(instruction, _) in INSTRUCTION_SET {
            instruction_bytecodes.insert(instruction, bytecode);
            bytecode += 1;
        }

        instruction_bytecodes
    };

    pub static ref BYTECODE_ARGS: HashMap<Bytecode, Args> = {
        let mut bytecode_args = HashMap::new();

        let mut bytecode = INSTRUCTION_BYTECODE_START;
        for &(_, args) in INSTRUCTION_SET {
            bytecode_args.insert(bytecode, args);
            bytecode += 1;
        }

        bytecode_args
    };
}
