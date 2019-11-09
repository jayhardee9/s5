use crate::constants::Const;
use crate::display::{PrintUnit, PrintUnits, Printable};
use crate::formulas::Side::{Both, Left, Neither, Right};
use crate::formulas::SimplifyErr::{MissingInfo, NotFinite};
use crate::formulas::SymbolicSolveErr::CantSolveYet;
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
use crate::variables::{Variable, VariableBindings};
use std::cmp::max;
use std::collections::HashSet;
use std::fmt::{Error, Formatter};
use std::ops::{Add, BitXor, Div, Mul, Sub};
use std::{fmt, ops};

#[derive(Copy, Clone, Debug)]
enum BinOps {
    Add,
    Subtract,
    Multiply,
    Divide,
    Pow,
}

#[derive(Copy, Clone, Debug)]
enum Functions {
    Sin,
    Cos,
    PosArccos,
    NegArccos,
    Sgn,
}

enum Side {
    Left,
    Right,
    Both,
    Neither,
}

#[derive(Clone, Debug)]
enum EFormula {
    Const(Const),
    Variable(Variable),
    BinOp(BinOps, Box<EFormula>, Box<EFormula>),
    Function(Functions, Box<EFormula>),
}

pub enum SimplifyErr {
    MissingInfo,
    NotFinite,
}

pub enum SymbolicSolveErr {
    TargetVariableAbsent,
    SignumOutOfRange,
    CantSolveYet,
    MissingSignum,
    SignumValMissing,
}

macro_rules! simplify_simple_binop {
    ($op1:ident, $fn:ident, $op2:ident, $bindings:ident) => {{
        let operand1 = $op1.simplify(&$bindings)?;
        let operand2 = $op2.simplify(&$bindings)?;
        Ok(operand1.$fn(operand2))
    }};
}

macro_rules! simplify_function_call {
    ($arg:ident, $fn:ident, $bindings:ident) => {{
        let arg = $arg.simplify($bindings)?;
        Ok(Const::from(f64::from(arg).$fn()))
    }};
}

impl EFormula {
    fn invert(&self) -> EFormula {
        let one = Box::new(EFormula::Const(Const::from(1)));

        EFormula::BinOp(BinOps::Divide, one, Box::new(self.clone()))
    }

    fn variables(&self) -> HashSet<&Variable> {
        match self {
            EFormula::Const(_) => HashSet::new(),

            EFormula::Variable(v) => {
                let mut rv = HashSet::new();
                rv.insert(v);
                rv
            }

            EFormula::BinOp(_, op1, op2) => {
                let mut op1_vars = op1.variables();
                op1_vars.extend(&op2.variables());
                op1_vars
            }

            EFormula::Function(_, arg) => arg.variables(),
        }
    }

    fn simplify(&self, bindings: &VariableBindings) -> Result<Const, SimplifyErr> {
        let rv = match self {
            EFormula::Const(c) => Ok(*c),

            EFormula::Variable(v) => {
                if let Some(c) = bindings.get(v) {
                    Ok(c)
                } else {
                    Err(MissingInfo)
                }
            }

            EFormula::BinOp(operator, operand1, operand2) => match operator {
                BinOps::Add => simplify_simple_binop!(operand1, add, operand2, bindings),

                BinOps::Subtract => simplify_simple_binop!(operand1, sub, operand2, bindings),

                BinOps::Multiply => simplify_simple_binop!(operand1, mul, operand2, bindings),

                BinOps::Divide => simplify_simple_binop!(operand1, div, operand2, bindings),

                BinOps::Pow => simplify_simple_binop!(operand1, bitxor, operand2, bindings),
            },

            EFormula::Function(function, arg) => match function {
                Functions::Sin => simplify_function_call!(arg, sin, bindings),

                Functions::Cos => simplify_function_call!(arg, cos, bindings),

                Functions::PosArccos => simplify_function_call!(arg, acos, bindings),

                Functions::NegArccos => {
                    let arg = arg.simplify(bindings)?;
                    Ok(Const::from(-f64::from(arg).acos()))
                }

                Functions::Sgn => simplify_function_call!(arg, signum, bindings),
            },
        };

        if let Ok(c) = rv {
            if !c.is_finite() {
                return Err(NotFinite);
            }
        }

        rv
    }

    fn side(&self, rhs: &EFormula, target: &Variable) -> Side {
        let on_right = rhs.variables().contains(target);
        let on_left = self.variables().contains(target);
        if on_left && on_right {
            return Both;
        }

        if on_right {
            return Right;
        }

        if on_left {
            return Left;
        }

        Neither
    }

    fn solve(
        &self,
        rhs: &EFormula,
        solving_for: &Variable,
        bindings: &VariableBindings,
    ) -> Result<EFormula, SymbolicSolveErr> {
        if !self.variables().contains(solving_for) && !rhs.variables().contains(solving_for) {
            return Err(SymbolicSolveErr::TargetVariableAbsent);
        }

        if !self.variables().contains(solving_for) {
            return rhs.solve(self, solving_for, bindings);
        }

        match self {
            EFormula::Const(_) => unreachable!(),

            EFormula::Variable(v) => {
                if v == solving_for {
                    Ok(rhs.clone())
                } else {
                    unreachable!()
                }
            }

            EFormula::BinOp(operator, op_lhs, op_rhs) => {
                let side = op_lhs.side(op_rhs, solving_for);

                match operator {
                    BinOps::Add => match side {
                        Left => op_lhs.solve(&(rhs - op_rhs), solving_for, bindings),

                        Right => op_rhs.solve(&(rhs - op_lhs), solving_for, bindings),

                        Both => Err(CantSolveYet),

                        Neither => unreachable!(),
                    },

                    BinOps::Subtract => match side {
                        Left => op_lhs.solve(&(rhs + op_rhs), solving_for, bindings),

                        Right => op_rhs.solve(&(op_lhs - rhs), solving_for, bindings),

                        Both => Err(CantSolveYet),

                        Neither => unreachable!(),
                    },

                    BinOps::Multiply => match side {
                        Left => op_lhs.solve(&(rhs / op_rhs), solving_for, bindings),

                        Right => op_rhs.solve(&(rhs / op_lhs), solving_for, bindings),

                        Both => Err(CantSolveYet),

                        Neither => unreachable!(),
                    },

                    BinOps::Divide => match side {
                        Left => op_lhs.solve(&(rhs * op_rhs), solving_for, bindings),

                        Right => op_rhs.solve(&(op_lhs / rhs), solving_for, bindings),

                        Both => Err(CantSolveYet),

                        Neither => unreachable!(),
                    },

                    BinOps::Pow => {
                        let exponent = &Box::new(op_rhs.invert());
                        op_lhs.solve(&(rhs ^ exponent), solving_for, bindings)
                    }
                }
            }

            EFormula::Function(function, arg) => match function {
                Functions::Cos => match arg.as_ref() {
                    EFormula::Variable(v) => {
                        if v != solving_for {
                            unreachable!()
                        }

                        let sgn_v = if let Some(v) = bindings.get_signum(v) {
                            v
                        } else {
                            return Err(SymbolicSolveErr::MissingSignum);
                        };

                        let sgn_v_val = if let Some(c) = bindings.get(sgn_v) {
                            c
                        } else {
                            return Err(SymbolicSolveErr::SignumValMissing);
                        };

                        let new_rhs = if sgn_v_val.is_one() {
                            EFormula::Function(Functions::PosArccos, Box::new(rhs.clone()))
                        } else if sgn_v_val.is_zero() {
                            0.into()
                        } else if sgn_v_val.is_minus_one() {
                            EFormula::Function(Functions::NegArccos, Box::new(rhs.clone()))
                        } else {
                            return Err(SymbolicSolveErr::SignumOutOfRange);
                        };

                        arg.solve(&new_rhs, solving_for, bindings)
                    }

                    _ => Err(SymbolicSolveErr::CantSolveYet),
                },

                _ => Err(SymbolicSolveErr::CantSolveYet),
            },
        }
    }
}

impl From<i64> for EFormula {
    fn from(i: i64) -> Self {
        EFormula::Const(i.into())
    }
}

// Stores result in `operand1_units`
fn simple_binop_print_units(
    binop_str: &str,
    operand1_units: &mut PrintUnits,
    operand2_units: &mut PrintUnits,
) {
    let mut operator_units: PrintUnits = PrintUnit::new(&format!(" {} ", binop_str)).into();
    operator_units.right_of(&operand1_units);
    operand2_units.right_of(&operator_units);
    operand1_units.append(&operator_units);
    operand1_units.append(&operand2_units);
}

impl Printable for EFormula {
    fn to_print_units(&self) -> PrintUnits {
        match self {
            EFormula::Const(c) => c.to_print_units(),

            EFormula::Variable(v) => v.to_print_units(),

            EFormula::BinOp(operator, operand1, operand2) => {
                let mut operand1_units = operand1.to_print_units();
                let mut operand2_units = operand2.to_print_units();

                match operator {
                    BinOps::Add => {
                        simple_binop_print_units("+", &mut operand1_units, &mut operand2_units);
                        operand1_units
                    }

                    BinOps::Subtract => {
                        simple_binop_print_units("-", &mut operand1_units, &mut operand2_units);
                        operand1_units
                    }

                    BinOps::Multiply => {
                        simple_binop_print_units("*", &mut operand1_units, &mut operand2_units);
                        operand1_units
                    }

                    BinOps::Divide => {
                        let bar_length = max(operand1_units.width(), operand2_units.width());
                        let div_bar = str::repeat("-", bar_length);
                        let operator_units =
                            PrintUnits::new(vec![PrintUnit::new(&format!(" {} ", div_bar))]);
                        operand1_units.shift_right(1);
                        operand2_units.shift_right(1);
                        operand2_units.below(&operator_units);
                        operand1_units.on_top_of(&operator_units);
                        operand1_units.append(&operator_units);
                        operand1_units.append(&operand2_units);
                        operand1_units
                    }

                    BinOps::Pow => {
                        operand2_units.on_top_of(&operand1_units);
                        operand2_units.right_of(&operand1_units);
                        operand1_units.append(&operand2_units);
                        operand1_units
                    }
                }
            }

            EFormula::Function(function, argument) => {
                let function_part1 = match function {
                    Functions::Sin => "sin(",

                    Functions::Cos => "cos(",

                    Functions::PosArccos => "acos+(",

                    Functions::NegArccos => "acos-(",

                    Functions::Sgn => "sgn(",
                };
                let mut function_units1 = PrintUnit::new(function_part1).into();

                let mut arg_units: PrintUnits = argument.to_print_units();

                let mut function_units2: PrintUnits = PrintUnit::new(")").into();

                arg_units.right_of(&function_units1);
                function_units2.right_of(&arg_units);
                function_units1.append(&arg_units);
                function_units1.append(&function_units2);
                function_units1
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Formula {
    f: EFormula,
}

macro_rules! func_formula_meth {
    ($method:ident, $($variant:ident)::+) => {
        pub fn $method(&self) -> Formula {
            Formula {
                f: EFormula::Function($($variant)::+, self.f.clone().into()),
            }
        }
    };
}

impl Formula {
    func_formula_meth!(sin, Functions::Sin);
    func_formula_meth!(cos, Functions::Cos);
    func_formula_meth!(pos_acos, Functions::PosArccos);
    func_formula_meth!(neg_acos, Functions::NegArccos);
    func_formula_meth!(sgn, Functions::Sgn);

    pub fn variables(&self) -> HashSet<&Variable> {
        self.f.variables()
    }

    pub fn simplify(&self, bindings: &VariableBindings) -> Result<Const, SimplifyErr> {
        self.f.simplify(bindings)
    }

    pub fn solve(
        &self,
        rhs: &Formula,
        solving_for: &Variable,
        bindings: &VariableBindings,
    ) -> Result<Formula, SymbolicSolveErr> {
        Ok(Formula {
            f: self.f.solve(&rhs.f, solving_for, bindings)?,
        })
    }

    pub fn print_formulas3(f1: &Formula, f2: &Formula, f3: &Formula) {
        let mut f1_units = f1.f.to_print_units();
        let mut eq1: PrintUnits = PrintUnit::new(" = ").into();
        let mut f2_units = f2.f.to_print_units();
        let mut eq2: PrintUnits = eq1.clone();
        let mut f3_units = f3.f.to_print_units();

        eq1.right_of(&f1_units);
        f2_units.right_of(&eq1);
        eq2.right_of(&f2_units);
        f3_units.right_of(&eq2);

        f1_units.append(&eq1);
        f1_units.append(&f2_units);
        f1_units.append(&eq2);
        f1_units.append(&f3_units);

        println!("\n{}\n", f1_units);
    }

    pub fn print_formulas2(f1: &Formula, f2: &Formula) {
        let mut f1_units = f1.f.to_print_units();
        let mut eq: PrintUnits = PrintUnit::new(" = ").into();
        let mut f2_units = f2.f.to_print_units();

        eq.right_of(&f1_units);
        f2_units.right_of(&eq);

        f1_units.append(&eq);
        f1_units.append(&f2_units);

        println!("\n{}\n", f1_units);
    }
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let units = self.f.to_print_units();
        writeln!(f)?;
        units.fmt(f)
    }
}

impl From<i64> for Formula {
    fn from(i: i64) -> Self {
        Formula {
            f: EFormula::Const(i.into()),
        }
    }
}

impl From<f64> for Formula {
    fn from(f: f64) -> Self {
        Formula {
            f: EFormula::Const(f.into()),
        }
    }
}

impl From<Const> for Formula {
    fn from(c: Const) -> Self {
        Formula {
            f: EFormula::Const(c),
        }
    }
}

impl From<&Const> for Formula {
    fn from(c: &Const) -> Self {
        Formula {
            f: EFormula::Const(*c),
        }
    }
}

impl From<Variable> for Formula {
    fn from(v: Variable) -> Self {
        Formula {
            f: EFormula::Variable(v),
        }
    }
}

impl From<&Variable> for Formula {
    fn from(v: &Variable) -> Self {
        Formula {
            f: EFormula::Variable(v.clone()),
        }
    }
}

macro_rules! formula_op {
    ($($trait:ident)::+, $fn:ident, $($bin_op_ty:ident)::+) => {
        impl $($trait)::+<Formula> for Formula {
            type Output = Formula;

            fn $fn(self, rhs: Formula) -> Self::Output {
                Formula {
                    f: EFormula::BinOp($($bin_op_ty)::+, Box::new(self.f), Box::new(rhs.f)),
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

        impl $($trait)::+<&Variable> for Variable {
            type Output = Formula;

            fn $fn(self, rhs: &Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Variable> for &Variable {
            type Output = Formula;

            fn $fn(self, rhs: Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<&Variable> for &Variable {
            type Output = Formula;

            fn $fn(self, rhs: &Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Variable> for i64 {
            type Output = Formula;

            fn $fn(self, rhs: Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<&Variable> for i64 {
            type Output = Formula;

            fn $fn(self, rhs: &Variable) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<i64> for Variable {
            type Output = Formula;

            fn $fn(self, rhs: i64) -> Formula {
                Formula::from(self).$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<i64> for &Variable {
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

        impl $($trait)::+<&Variable> for Formula {
            type Output = Formula;

            fn $fn(self, rhs: &Variable) -> Formula {
                self.$fn(Formula::from(rhs))
            }
        }

        impl $($trait)::+<Formula> for Variable {
            type Output = Formula;

            fn $fn(self, rhs: Formula) -> Formula {
                Formula::from(self).$fn(rhs)
            }
        }

        impl $($trait)::+<Formula> for &Variable {
            type Output = Formula;

            fn $fn(self, rhs: Formula) -> Formula {
                Formula::from(self).$fn(rhs)
            }
        }

        // EFormula implementations
        impl $($trait)::+<EFormula> for EFormula {
            type Output = EFormula;

            fn $fn(self, rhs: EFormula) -> EFormula {
                EFormula::BinOp($($bin_op_ty)::+,
                    Box::new(self.clone()),
                    Box::new(rhs.clone()))
            }
        }

        impl $($trait)::+<&Box<EFormula>> for &EFormula {
            type Output = EFormula;

            fn $fn(self, rhs: &Box<EFormula>) -> Self::Output {
                EFormula::BinOp($($bin_op_ty)::+, Box::new(self.clone()), rhs.clone())
            }
        }

        impl $($trait)::+<&EFormula> for &Box<EFormula> {
            type Output = EFormula;

            fn $fn(self, rhs: &EFormula) -> Self::Output {
                EFormula::BinOp($($bin_op_ty)::+, self.clone(), Box::new(rhs.clone()))
            }
        }

        impl $($trait)::+<Box<EFormula>> for Box<EFormula> {
            type Output = EFormula;

            fn $fn(self, rhs: Box<EFormula>) -> Self::Output {
                EFormula::BinOp($($bin_op_ty)::+, self, rhs)
            }
        }

        impl $($trait)::+<&Box<EFormula>> for Box<EFormula> {
            type Output = EFormula;

            fn $fn(self, rhs: &Box<EFormula>) -> Self::Output {
                EFormula::BinOp($($bin_op_ty)::+, self, rhs.clone())
            }
        }

        impl $($trait)::+<Box<EFormula>> for &Box<EFormula> {
            type Output = EFormula;

            fn $fn(self, rhs: Box<EFormula>) -> Self::Output {
                EFormula::BinOp($($bin_op_ty)::+, self.clone(), rhs)
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
