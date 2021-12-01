use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    mem,
};

type Instruction = &'static str;
type Bytecode = u8;
type Args = Option<&'static [u8]>;

const PTR: u8 = mem::size_of::<usize>() as u8;
const INT: u8 = 4;
const BYTE: u8 = 1;

static INSTRUCTION_SET: &[(Instruction, Bytecode, Args)] = &[
    // Functions
    ("invoke", 0x01, Some(&[PTR, INT])),
    ("return", 0x02, None),

    // Pointers
    ("ppushl", 0x03, Some(&[PTR])),
    ("ppusho", 0x04, Some(&[INT])),
    ("pset",   0x05, None),
    ("pload",  0x06, None),
    ("pceq",   0x06, None),
    ("pcne",   0x06, None),

    // Integers
    ("ipushl", 0x07, Some(&[INT])),
    ("ipusho", 0x08, Some(&[INT])),
    ("iset",   0x09, None),
    ("iload",  0x0A, None),
    ("iadd",   0x0B, None),
    ("isub",   0x0C, None),
    ("imul",   0x0D, None),
    ("idiv",   0x0E, None),
    ("iceq",   0x0F, None),
    ("icne",   0x10, None),
    ("iclt",   0x11, None),
    ("icle",   0x12, None),
    ("icgt",   0x13, None),
    ("icge",   0x14, None),

    ("moffset", 0x80, Some(&[BYTE])),

    ("jmp",  0x81, Some(&[PTR])),
    ("jmpc", 0x82, Some(&[PTR])),
];

lazy_static! {
    static ref INSTRUCTION_SET_BYTECODE: HashMap<Instruction, Bytecode> = {
        let mut instruction_bytecodes = HashMap::new();

        for &(instruction, code, _) in INSTRUCTION_SET {
            instruction_bytecodes.insert(instruction, code);
        }

        instruction_bytecodes
    };

    static ref BYTECODE_ARGS: HashMap<Bytecode, Args> = {
        let mut bytecode_args = HashMap::new();

        for &(_, code, args) in INSTRUCTION_SET {
            bytecode_args.insert(code, args);
        }

        bytecode_args
    };
}
