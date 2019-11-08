use std::collections::HashMap;
use crate::variables::{Variable, VariableBindings};
use crate::constants::Const;

pub struct State {
    bindings: VariableBindings,
    signums: HashMap<Variable, Variable>,
}

impl State {
    pub fn new() -> State {
        unimplemented!()
    }

    pub fn add_variable(&mut self, name: &str, description: &str) -> Variable {
        unimplemented!()
    }

    pub fn add_signum_for(&mut self, v: &Variable) -> Variable {
        unimplemented!()
    }

    pub fn deduce(&mut self) {
        unimplemented!()
    }

    pub fn add_binding(&mut self, var: &Variable, val: Const) {
        unimplemented!()
    }

    pub fn remove_binding(&mut self, var: &Variable) {
        unimplemented!()
    }

    pub fn get_binding(&self, var: &Variable) {
        unimplemented!()
    }
}
