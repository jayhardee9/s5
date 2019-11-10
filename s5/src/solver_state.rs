//! Solver
//!
//! This module implements the s5.
use crate::constants::Const;
use crate::equations::Equation;
use crate::formulas::{Formula, SimplifyErr, SymbolicSolveErr};
use crate::variables::{Variable, VariableBindings};

// Internal equation for the s5.
#[derive(Debug)]
struct SolverEquation {
    // True if equation has already been solved
    used: bool,

    equation: Equation,
}

/// The s5 type.
#[derive(Default, Debug)]
pub struct SolverState {
    equations: Vec<SolverEquation>,
    bindings: VariableBindings,
}

impl SolverState {
    /// Create a new s5
    pub fn new() -> SolverState {
        Default::default()
    }

    /// Add a slice of equations to the s5.
    pub fn add_equations(&mut self, equations: &[Equation]) {
        // Convert to internal equation representation
        self.equations = equations
            .iter()
            .map(|equation| SolverEquation {
                used: false,
                equation: equation.clone(),
            })
            .collect();

        // Add equation variables
        for equation in &self.equations {
            for variable in equation.equation.variables() {
                self.bindings.add(variable);
            }
        }
    }

    /// Bind a value to a variable
    pub fn bind_variable(&mut self, variable: &Variable, c: &Const) {
        self.bindings.bind(variable, c)
    }

    /// Assert that `sgn_v = sgn(v)`. This is needed for solving equations involving trig functions.
    pub fn add_signum_for(&mut self, v: &Variable, sgn_v: &Variable) {
        self.bindings.add_signum_for(v, sgn_v);
    }

    /// Deduce all possible variable assignments
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

                match SolverState::symbolic_solve(&solving_for, &equation.equation, &self.bindings)
                {
                    Ok(simplifying_formula) => {
                        match simplifying_formula.simplify(&self.bindings) {
                            Ok(new_binding_value) => {
                                self.bindings.bind(&solving_for, &new_binding_value);
                                //                                Formula::print_formulas3(
                                //                                    &solving_for.into(),
                                //                                    &simplifying_formula,
                                //                                    &new_binding_value.into(),
                                //                                );
                                finished = false;
                                equation.used = true;
                            }

                            Err(SimplifyErr::NotFinite) => {
                                equation.used = true;
                                // println!("Couldn't simplify: {}", simplifying_formula);
                            }

                            Err(SimplifyErr::MissingInfo) => {
                                // NOP
                                // println!("Couldn't simplify: {}", simplifying_formula);
                            }
                        }
                    }
                    Err(e) => {
                        // println!("Info: couldn't solve for {}:", solving_for);
                        // Formula::print_formulas2(&equation.equation.left, &equation.equation.right);
                        match e {
                            SymbolicSolveErr::TargetVariableAbsent => unreachable!(),

                            SymbolicSolveErr::SignumOutOfRange => panic!("signum out of range"),

                            SymbolicSolveErr::CantSolveYet => {
                                equation.used = true;
                            }

                            SymbolicSolveErr::MissingSignum => {
                                equation.used = true;
                            }

                            SymbolicSolveErr::SignumValMissing => {
                                // NOP
                            }
                        }
                    }
                }
            }
        }
    }

    // Solve for a single variable.
    fn symbolic_solve(
        solving_for: &Variable,
        equation: &Equation,
        bindings: &VariableBindings,
    ) -> Result<Formula, SymbolicSolveErr> {
        equation.left.solve(&equation.right, solving_for, bindings)
    }

    /// Return value assigned to variable. Returns `None` if variable value could be deduced.
    pub fn get_binding(&self, var: &Variable) -> Option<Const> {
        self.bindings.get(var)
    }

    /// Remove all variable assignments and reset s5 state.
    pub fn clear_bindings(&mut self) {
        self.bindings.clear_bindings();

        for equation in self.equations.iter_mut() {
            equation.used = false
        }
    }
}
