use std::io::Write;

type WriteStream<'a> = &'a mut dyn Write;

pub struct RuntimeStream<'a> {
    pub out_stream: WriteStream<'a>,
    _err_stream: WriteStream<'a>,
}

impl<'a> RuntimeStream<'a> {
    pub fn new(out_stream: WriteStream<'a>, err_stream: WriteStream<'a>) -> Self {
        Self {
            out_stream,
            _err_stream: err_stream,
        }
    }
}
