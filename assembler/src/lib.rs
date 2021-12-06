extern crate pest;
#[macro_use]
extern crate pest_derive;
#[cfg(test)]
extern crate regex;

mod assembler;
#[rustfmt::skip]
mod bytecode;
mod parser;
