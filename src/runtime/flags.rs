pub struct RuntimeFlags {
    pub stop_exec: bool,
    pub exit_loop: bool,
}

impl RuntimeFlags {
    pub fn new() -> Self {
        Self {
            stop_exec: false,
            exit_loop: false,
        }
    }
}
