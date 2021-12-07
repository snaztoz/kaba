extern crate pest;
#[macro_use]
extern crate pest_derive;
#[cfg(test)]
extern crate regex;

pub use assembler::assemble;

mod assembler;
#[rustfmt::skip]
mod bytecode;
mod parser;
#[cfg(test)]
mod testutils;
