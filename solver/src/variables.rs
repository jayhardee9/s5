use crate::constants::Const;
use crate::display::{PrintUnit, PrintUnits, Printable};
use std::collections::{HashMap, HashSet};
use std::fmt::{Error, Formatter};

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Debug)]
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

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        self.to_print_units().fmt(f)
    }
}

#[derive(Debug, Default)]
pub struct VariableBindings {
    bindings: HashMap<Variable, Option<Const>>,
    signums: HashMap<Variable, Variable>,
}

impl VariableBindings {
    pub fn new() -> VariableBindings {
        Default::default()
    }

    pub fn get(&self, v: &Variable) -> Option<Const> {
        let rv = self.bindings.get(&v)?;
        *rv
    }

    pub fn add(&mut self, v: &Variable) {
        self.bindings.insert(v.clone(), None);
    }

    pub fn bind(&mut self, v: &Variable, c: &Const) {
        self.bindings.insert(v.clone(), Some(*c));
    }

    pub fn unbind(&mut self, v: &Variable) {
        self.bindings.insert(v.clone(), None);
    }

    pub fn bound(&self) -> HashSet<&Variable> {
        self.bindings
            .keys()
            .filter(|variable| self.bindings.get(*variable).unwrap().is_some())
            .collect()
    }

    // TODO automatically detect signum relationships
    pub fn add_signum_for(&mut self, v: &Variable, sgn_v: &Variable) {
        self.signums.insert(v.clone(), sgn_v.clone());
    }

    pub fn get_signum(&self, v: &Variable) -> Option<&Variable> {
        self.signums.get(v)
    }

    pub fn clear_bindings(&mut self) {
        for (_, c) in self.bindings.iter_mut() {
            *c = None
        }
    }
}
