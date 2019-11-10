//! Equations
//!
//! Combine two `Formula`s into an `Equation`! Use them to model something and use the s5 to
//! solve for the unknowns.
use crate::formulas::Formula;
use crate::variables::Variable;
use std::collections::HashSet;

/// Represents an equation of the form `left = right`.
#[derive(Clone, Debug)]
pub struct Equation {
    /// Left side of equation
    pub left: Formula,

    /// Right side of equation
    pub right: Formula,
}

impl Equation {
    /// Create a new `Equation` of the form `left = right`.
    pub fn new(left: Formula, right: Formula) -> Equation {
        Equation { left, right }
    }

    /// Create a new `Equation` of the form `left = right`, where `left` is a `Variable`.
    pub fn new_assignment(left: &Variable, right: Formula) -> Equation {
        Equation {
            left: left.into(),
            right,
        }
    }

    /// Return a variables involved in this equation.
    pub fn variables(&self) -> HashSet<&Variable> {
        let mut rv = self.left.variables();
        rv.extend(&self.right.variables());
        rv
    }

    /// Flip the sides of an equation. Return a new `Equation` where `right = left`.
    pub fn flip(&self) -> Equation {
        Equation {
            left: self.right.clone(),
            right: self.left.clone(),
        }
    }
}
