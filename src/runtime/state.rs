use std::cell::RefCell;

pub struct RuntimeState {
    stop_exec: RefCell<bool>,
    exit_loop: RefCell<bool>,
}

impl RuntimeState {
    pub fn new() -> Self {
        Self {
            stop_exec: RefCell::new(false),
            exit_loop: RefCell::new(false),
        }
    }

    pub fn is_stop_executing(&self) -> bool {
        *self.stop_exec.borrow()
    }

    pub fn is_exiting_loop(&self) -> bool {
        *self.exit_loop.borrow()
    }

    pub fn stop_execution(&self) {
        *self.stop_exec.borrow_mut() = true;
    }

    pub fn exit_loop(&self) {
        *self.exit_loop.borrow_mut() = true;
    }

    pub fn resume_execution(&self) {
        *self.stop_exec.borrow_mut() = false;
    }

    pub fn reset_loop_state(&self) {
        *self.exit_loop.borrow_mut() = false;
    }
}
