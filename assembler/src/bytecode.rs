use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    mem,
};

type Instruction = &'static str;
pub type Bytecode = u8;
type Args = Option<&'static [u8]>;

const PTR: u8 = mem::size_of::<usize>() as u8;
const INT: u8 = 4;
const BYTE: u8 = 1;

pub const EOI_BYTECODE: u8 = 0x00;
const BYTECODE_START: u8 = EOI_BYTECODE + 1;

static INSTRUCTION_SET: &[(Instruction, Args)] = &[
    // Functions
    ("invoke", Some(&[PTR, INT])),
    ("return", None),

    // References
    ("rpushl", Some(&[PTR])),
    ("rpusho", Some(&[PTR])),
    ("rset",   None),
    ("rload",  None),
    ("rceq",   None),
    ("rcne",   None),

    // Integers
    ("ipushl", Some(&[INT])),
    ("ipusho", Some(&[INT])),
    ("ipopl",  Some(&[INT])),
    ("ipopo",  Some(&[INT])),
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

    ("moffset", Some(&[BYTE])),

    ("jmp",  Some(&[PTR])),
    ("jmpc", Some(&[PTR])),
];

lazy_static! {
    pub static ref INSTRUCTION_SET_BYTECODE: HashMap<Instruction, Bytecode> = {
        let mut instruction_bytecodes = HashMap::new();

        let mut bytecode = BYTECODE_START;
        for &(instruction, _) in INSTRUCTION_SET {
            instruction_bytecodes.insert(instruction, bytecode);
            bytecode += 1;
        }

        instruction_bytecodes
    };

    pub static ref BYTECODE_ARGS: HashMap<Bytecode, Args> = {
        let mut bytecode_args = HashMap::new();

        let mut bytecode = BYTECODE_START;
        for &(_, args) in INSTRUCTION_SET {
            bytecode_args.insert(bytecode, args);
            bytecode += 1;
        }

        bytecode_args
    };
}
