/// Formulas are expression that can evaluate to some value. Can be combined into an `Equation`.
///
/// # Examples
/// ```
/// use solver::formulas::*;
/// use solver::variables::*;
///
/// let s = Variable::new("s", "speed of satellite");
/// let me = Variable::new("E", "specific mechanical energy of orbit");
/// let r = Variable::new("r", "distance of satellite from center of Earth");
/// let u = Variable::new("u", "gravitational parameter of Earth");
///
/// let f: Formula = s^2 / 2 - u / r;
/// ```

use crate::variables::Variable;
use std::rc::Rc;
use std::{ops, fmt};
use crate::constants::Const;
use crate::display::{Printable, PrintUnit, PrintUnits};
use std::cmp::max;
use std::fmt::{Formatter, Error, Pointer};

enum BinOps {
    Add,
    Subtract,
    Multiply,
    Divide,
    Pow,
}

enum Functions {
    Sin,
    Cos,
    PosArccos,
    NegArccos,
}

enum EFormula {
    Const(Const),
    Variable(Variable),
    BinOp(BinOps, Rc<EFormula>, Rc<EFormula>),
    Function(Functions, Rc<EFormula>),
}

// Stores result in `operand1_units`
fn simple_binop_print_units(binop_str: &str, operand1_units: &mut PrintUnits, operand2_units: &mut PrintUnits) {
    let mut operator_units = PrintUnits::new(vec![PrintUnit::new(&format!(" {} ", binop_str))]);
    operator_units.right_of(&operand1_units);
    operand2_units.right_of(&operator_units);
    operand1_units.append(&operator_units);
    operand1_units.append(&operand2_units);
}

impl Printable for EFormula {
    fn to_print_units(&self) -> PrintUnits {
        match self {
            EFormula::Const(c) => {
                c.to_print_units()
            },

            EFormula::Variable(v) => {
                v.to_print_units()
            },

            EFormula::BinOp(operator, operand1, operand2) => {
                let mut operand1_units = operand1.to_print_units();
                let mut operand2_units = operand2.to_print_units();

                match operator {
                    BinOps::Add => {
                        simple_binop_print_units("+", &mut operand1_units, &mut operand2_units);
                        operand1_units
                    },

                    BinOps::Subtract => {
                        simple_binop_print_units("-", &mut operand1_units, &mut operand2_units);
                        operand1_units
                    },

                    BinOps::Multiply => {
                        simple_binop_print_units("*", &mut operand1_units, &mut operand2_units);
                        operand1_units
                    },

                    BinOps::Divide => {
                        let bar_length = max(operand1_units.width(), operand2_units.width());
                        let bar = str::repeat("-", bar_length);
                        let mut operator_units = PrintUnits::new(vec![PrintUnit::new(&format!(" {} ", bar))]);
                        operand1_units.shift_right(1);
                        operand2_units.shift_right(1);
                        operand2_units.below(&operator_units);
                        operand1_units.on_top_of(&operator_units);
                        operand1_units.append(&operator_units);
                        operand1_units.append(&operand2_units);
                        operand1_units
                    },

                    BinOps::Pow => {
                        operand2_units.on_top_of(&operand1_units);
                        operand2_units.right_of(&operand1_units);
                        operand1_units.append(&operand2_units);
                        operand1_units
                    },
                }
            },

            EFormula::Function(function, argument) => {
                let function_part1 = match function {
                    Functions::Sin => {
                        "sin("
                    },

                    Functions::Cos => {
                        "cos("
                    },

                    Functions::PosArccos => {
                        "acos+("
                    },

                    Functions::NegArccos => {
                        "acos-("
                    },
                };
                let mut function_units1 =
                    PrintUnits::new(vec![PrintUnit::new(function_part1)]);

                let mut arg_units = argument.to_print_units();

                let mut function_units2 = PrintUnits::new(vec![PrintUnit::new(")")]);

                arg_units.right_of(&function_units1);
                function_units2.right_of(&arg_units);
                function_units1.append(&arg_units);
                function_units1.append(&function_units2);
                function_units1
            },
        }
    }
}

pub struct Formula {
    f: Rc<EFormula>,
}

impl Formula {
    pub fn sin(&self) -> Formula {
        Formula {
            f: Rc::new(EFormula::Function(Functions::Sin, Rc::clone(&self.f))),
        }
    }

    pub fn cos(&self) -> Formula {
        Formula {
            f: Rc::new(EFormula::Function(Functions::Cos, Rc::clone(&self.f))),
        }
    }

    pub fn pos_acos(&self) -> Formula {
        Formula {
            f: Rc::new(EFormula::Function(Functions::PosArccos, Rc::clone(&self.f))),
        }
    }

    pub fn neg_acos(&self) -> Formula {
        Formula {
            f: Rc::new(EFormula::Function(Functions::NegArccos, Rc::clone(&self.f))),
        }
    }
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let units = self.f.to_print_units();
        units.fmt(f)
    }
}

impl From<i64> for Formula {
    fn from(i: i64) -> Self {
        Formula {
            f: Rc::new(EFormula::Const(i.into())),
        }
    }
}

impl From<f64> for Formula {
    fn from(f: f64) -> Self {
        Formula {
            f: Rc::new(EFormula::Const(f.into())),
        }
    }
}

impl From<Const> for Formula {
    fn from(c: Const) -> Self {
        Formula {
            f: Rc::new(EFormula::Const(c)),
        }
    }
}

impl From<Variable> for Formula {
    fn from(v: Variable) -> Self {
        Formula {
            f: Rc::new(EFormula::Variable(v)),
        }
    }
}

macro_rules! formula_op {
    ($($trait:ident)::+, $fn:ident, $($bin_op_ty:ident)::+) => {
        impl $($trait)::+<Formula> for Formula {
            type Output = Formula;

            fn $fn(self, rhs: Formula) -> Self::Output {
                Formula {
                    f: Rc::new(EFormula::BinOp($($bin_op_ty)::+, self.f, rhs.f)),
                }
            }
        }

        impl $($trait)::+<i64> for Formula {
            type Output = Formula;

            fn $fn(self, rhs: i64) -> Self::Output {
                self.$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Formula> for i64 {
            type Output = Formula;

            fn $fn(self, rhs: Formula) -> Self::Output {
                Formula::from(self).$fn(rhs)
            }
        }

        impl $($trait)::+<f64> for Formula {
            type Output = Formula;

            fn $fn(self, rhs: f64) -> Formula {
                self.$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Formula> for f64 {
            type Output = Formula;

            fn $fn(self, rhs: Formula) -> Formula {
                Formula::from(self).$fn(rhs)
            }
        }

        impl $($trait)::+<Variable> for Variable {
            type Output = Formula;

            fn $fn(self, rhs: Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Variable> for i64 {
            type Output = Formula;

            fn $fn(self, rhs: Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<i64> for Variable {
            type Output = Formula;

            fn $fn(self, rhs: i64) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Variable> for f64 {
            type Output = Formula;

            fn $fn(self, rhs: Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<f64> for Variable {
            type Output = Formula;

            fn $fn(self, rhs: f64) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Variable> for Formula {
            type Output = Formula;

            fn $fn(self, rhs: Variable) -> Formula {
                self.$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Formula> for Variable {
            type Output = Formula;

            fn $fn(self, rhs: Formula) -> Formula {
                Formula::from(self).$fn(rhs)
            }
        }
    };
}

formula_op!(ops::Add, add, BinOps::Add);
formula_op!(ops::Sub, sub, BinOps::Subtract);
formula_op!(ops::Mul, mul, BinOps::Multiply);
formula_op!(ops::Div, div, BinOps::Divide);

// Using ^ for exponentiation
formula_op!(ops::BitXor, bitxor, BinOps::Pow);
