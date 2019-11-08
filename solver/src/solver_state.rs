use std::collections::HashMap;
use crate::variables::{Variable, VariableBindings};
use crate::constants::Const;
use crate::equations::Equation;
use crate::formulas::Formula;
use std::ptr::eq;

#[derive(Debug)]
struct SolverEquation {
    used: bool,
    equation: Equation,
}

#[derive(Default, Debug)]
pub struct SolverState {
    equations: Vec<SolverEquation>,
    pub bindings: VariableBindings,
}

impl SolverState {
    pub fn new() -> SolverState {
        Default::default()
    }

    pub fn add_equations(&mut self, equations: &[Equation]) {
        self.equations = equations.iter().map(|equation| {
            SolverEquation {
                used: false,
                equation: equation.clone(),
            }
        }).collect();

        // Add equation variables
        for equation in &self.equations {
            for variable in equation.equation.variables() {
                self.bindings.add(variable);
            }
        }
    }

    pub fn add_variable(&mut self, variable: &Variable) {
        self.bindings.add(variable)
    }

    pub fn bind_variable(&mut self, variable: &Variable, c: &Const) {
        self.bindings.bind(variable, c)
    }

    pub fn add_signum_for(&mut self, v: &Variable, sgn_v: &Variable) {
        self.bindings.add_signum_for(v, sgn_v);
    }

    pub fn deduce(&mut self) {
        let mut finished = false;
        while !finished {
            finished = true;

            for equation in self.equations.iter_mut() {
                if equation.used {
                    continue;
                }

                let e_vars = equation.equation.variables();
                let bound_vars = self.bindings.bound();
                let binding_diff = e_vars.difference(&bound_vars).collect::<Vec<&&Variable>>();

                if binding_diff.len() != 1 {
                    continue;
                }

                // Clone variable so that we don't have any immutable references to self.bindings -
                // may need to modify it soon
                let solving_for = (**binding_diff.first().unwrap()).clone();

                if let Some(simplifying_formula) = SolverState::symbolic_solve(&solving_for, &equation.equation, &self.bindings) {
                    if let Some(new_binding_value) = simplifying_formula.simplify(&self.bindings) {
                        self.bindings.bind(&solving_for, &new_binding_value);
                        Formula::print_formulas3(&solving_for.into(), &simplifying_formula, &new_binding_value.into());
                        finished = false;
                    } else {
//                        println!("Couldn't simplify: {}", simplifying_formula);
                    }
                } else {
//                    println!("Info: couldn't solve for {}:", solving_for);
//                    Formula::print_formulas2(&equation.equation.left, &equation.equation.right);
                }

                equation.used = true;
            }
        }
    }

    fn symbolic_solve(solving_for: &Variable, equation: &Equation, bindings: &VariableBindings) -> Option<Formula> {
        equation.left.solve(&equation.right, solving_for, bindings)
    }

    pub fn add_binding(&mut self, var: &Variable, val: &Const) {
        self.bindings.bind(var, val)
    }

    pub fn remove_binding(&mut self, var: &Variable) {
        self.bindings.unbind(var)
    }

    pub fn get_binding(&self, var: &Variable) -> Option<Const> {
        self.bindings.get(var)
    }

    pub fn clear_bindings(&mut self) {
        self.bindings.clear_bindings();

        for equation in self.equations.iter_mut() {
            equation.used = false
        }
    }
}
