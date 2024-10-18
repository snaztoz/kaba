use std::{
    cell::{RefCell, RefMut},
    io::Write,
};

type WriteStream<'a> = &'a mut dyn Write;

pub struct RuntimeStream<'a> {
    out_stream: RefCell<WriteStream<'a>>,
    _err_stream: RefCell<WriteStream<'a>>,
}

impl<'a> RuntimeStream<'a> {
    pub fn new(out_stream: WriteStream<'a>, err_stream: WriteStream<'a>) -> Self {
        Self {
            out_stream: RefCell::new(out_stream),
            _err_stream: RefCell::new(err_stream),
        }
    }

    pub fn output(&self) -> RefMut<'_, WriteStream<'a>> {
        self.out_stream.borrow_mut()
    }
}
