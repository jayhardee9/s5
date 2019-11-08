use crate::formulas::Formula;
use crate::variables::Variable;
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub struct Equation {
    pub left: Formula,
    pub right: Formula,
}

impl Equation {
    pub fn new(left: Formula, right: Formula) -> Equation {
        Equation {
            left,
            right,
        }
    }

    pub fn new_assignment(left: &Variable, right: Formula) -> Equation {
        Equation {
            left: left.into(),
            right,
        }
    }

    pub fn variables(&self) -> HashSet<&Variable> {
        let mut rv = self.left.variables();
        rv.extend(&self.right.variables());
        rv
    }

    pub fn flip(&self) -> Equation {
        Equation {
            left: self.right.clone(),
            right: self.left.clone(),
        }
    }
}
