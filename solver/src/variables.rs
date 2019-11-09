//! Variables
//!
//! Type for representing variables in expressions and equations.
use crate::constants::Const;
use crate::display::{PrintUnit, PrintUnits, Printable};
use std::collections::{HashMap, HashSet};
use std::fmt::{Error, Formatter};

/// Type for representing variables in expressions and equations.
#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Debug)]
pub struct Variable {
    name: String,
    description: String,
}

impl Variable {
    /// Create a new variable with provided name and description.
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

/// Type for binding `Variable`s with `Const`s. Some bindings will lack a `Const`, representing an
/// unknown value to be solved for.
#[derive(Debug, Default)]
pub struct VariableBindings {
    bindings: HashMap<Variable, Option<Const>>,
    signums: HashMap<Variable, Variable>,
}

impl VariableBindings {
    /// Initialize a new `VariableBindings`
    pub fn new() -> VariableBindings {
        Default::default()
    }

    /// Get `Const` assigned to `v`. Returns `None` if variable is still unknown.
    pub fn get(&self, v: &Variable) -> Option<Const> {
        let rv = self.bindings.get(&v)?;
        *rv
    }

    /// Add `v` without an assignment.
    pub fn add(&mut self, v: &Variable) {
        self.bindings.insert(v.clone(), None);
    }

    /// Add (or update) the assignment for `v`.
    pub fn bind(&mut self, v: &Variable, c: &Const) {
        self.bindings.insert(v.clone(), Some(*c));
    }

    /// Clear any assignment for `v`
    pub fn unbind(&mut self, v: &Variable) {
        self.bindings.insert(v.clone(), None);
    }

    /// Return all variables that are bound to a `Const`. Unassigned variables aren't returned.
    pub fn bound(&self) -> HashSet<&Variable> {
        self.bindings
            .keys()
            .filter(|variable| self.bindings.get(*variable).unwrap().is_some())
            .collect()
    }

    // TODO automatically detect signum relationships
    /// See documentation for [`add_signum_for`](../solver_state/struct.SolverState.html#method_add_signum)
    pub fn add_signum_for(&mut self, v: &Variable, sgn_v: &Variable) {
        self.signums.insert(v.clone(), sgn_v.clone());
    }

    /// Return `y` such that `y = sgn(x)`.
    pub fn get_signum(&self, v: &Variable) -> Option<&Variable> {
        self.signums.get(v)
    }

    /// Remove all assignments.
    pub fn clear_bindings(&mut self) {
        for (_, c) in self.bindings.iter_mut() {
            *c = None
        }
    }
}
