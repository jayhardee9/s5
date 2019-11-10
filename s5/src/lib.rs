//! A very basic equation s5 for physics problems.
//!
//! This crate implements a s5 that consumes a set of equations modelling some system plus
//! known variable values and deduces values for as many other system variables as possible. See
//! [`solver_state`] for API usage and the [`problems`] crate for example usage for solving problems
//! from the text book _Fundamentals of Astrodynamics_.
//!
//! # Organization
//! The modules [`constants`], [`variables`] and [`formulas`] contain the types used for
//! constructing the equations that model your physics system, as well as providing known values
//! for your problem variables. See [`formulas`] for examples on building formulas.
//!
//! The [`solver_state`] module implements the s5 itself.
//!
//! [`constants`]: constants/index.html
//! [`equations`]: equations/index.html
//! [`formulas`]: formulas/index.html
//! [`solver_state`]: solver_state/index.html
//! [`variables`]: variables/index.html
//! [`problems`]: ../problems/index.html
pub mod constants;
mod display;
pub mod equations;
pub mod formulas;
pub mod solver_state;
pub mod variables;
