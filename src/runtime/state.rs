use super::{error::Result, stream::RuntimeStream, value::RuntimeValue};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
};

pub type Scope = HashMap<String, RuntimeValue>;

pub struct RuntimeState<'a> {
    stop_exec: RefCell<bool>,
    exit_loop: RefCell<bool>,
    return_val: RefCell<RuntimeValue>,

    // All array allocation will be stored here.
    //
    // This is temporary solution, so there is no automatic cleanup for arrays
    // that are no longer in use.
    pub array_arena: RefCell<Vec<Vec<RuntimeValue>>>,

    pub ss: RefCell<Vec<Scope>>,
    pub streams: RefCell<RuntimeStream<'a>>,
}

impl<'a> RuntimeState<'a> {
    pub fn new(streams: RuntimeStream<'a>) -> Self {
        Self {
            stop_exec: RefCell::new(false),
            exit_loop: RefCell::new(false),
            return_val: RefCell::new(RuntimeValue::Void),

            array_arena: RefCell::new(vec![]),

            ss: RefCell::new(vec![
                HashMap::new(), // global scope
            ]),
            streams: RefCell::new(streams),
        }
    }
}

impl RuntimeState<'_> {
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

    pub fn return_value(&self) -> Ref<RuntimeValue> {
        self.return_val.borrow()
    }

    pub fn set_return_value(&self, val: RuntimeValue) {
        *self.return_val.borrow_mut() = val;
    }

    pub fn get_value(&self, id: &str) -> Result<RuntimeValue> {
        let ss = self.ss.borrow();
        let scope = ss
            .iter()
            .rev()
            .find(|scope| scope.contains_key(id))
            .unwrap();

        Ok(scope.get(id).unwrap().clone())
    }

    pub fn store_value(&self, id: &str, val: RuntimeValue) {
        let last_i = self.ss.borrow().len() - 1;
        self.ss.borrow_mut()[last_i].insert(String::from(id), val);
    }

    pub fn update_value(&self, id: &str, val: RuntimeValue) -> Result<()> {
        let mut ss = self.ss.borrow_mut();
        let scope = ss
            .iter_mut()
            .rev()
            .find(|scope| scope.contains_key(id))
            .unwrap();
        *scope.get_mut(id).unwrap() = val;
        Ok(())
    }
}
