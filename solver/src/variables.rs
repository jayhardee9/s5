use crate::display::{Printable, PrintUnit, PrintUnits};
use crate::constants::Const;
use std::collections::HashMap;

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Variable {
    name: String,
    description: String,
}

impl Variable {
    pub fn new(name: &str, description: &str) -> Variable {
        Variable {
            name: name.into(),
            description: description.into(),
        }
    }
}

impl Printable for Variable {
    fn to_print_units(&self) -> PrintUnits {
        PrintUnits::new(vec![PrintUnit::new(&self.name)])
    }
}

pub struct VariableBindings {
    m: HashMap<Variable, Const>,
}

impl VariableBindings {
    pub fn new() -> VariableBindings {
        VariableBindings {
            m: HashMap::new(),
        }
    }

    pub fn get(&self, v: &Variable) -> Option<&Const> {
        self.m.get(&v)
    }
}
