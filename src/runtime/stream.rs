use std::io::Write;

pub type WriteStream<'a> = &'a mut dyn Write;

pub struct RuntimeStream<'a> {
    pub out: WriteStream<'a>,
    _err: WriteStream<'a>,
}

impl<'a> RuntimeStream<'a> {
    pub fn new(out: WriteStream<'a>, err: WriteStream<'a>) -> Self {
        Self { out, _err: err }
    }
}
