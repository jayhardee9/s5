[![Build Status](https://travis-ci.org/jayhardee9/s5.svg?branch=master)](https://travis-ci.org/jayhardee9/s5)
# Super Slick Simple Substitution Solver (S<sup>5</sup>)
S<sup>5</sup> is a simple equation solver written in Rust for the 
purpose of solving some homework-style problems in orbital 
mechanics. It can be used for any system of 
equations though (not necessarily modelling orbital mechanics).

## How it works
It works by taking all known variables, finding an
equation with exactly one unknown variable, solving for it in terms
of the knowns, and finally calculating the solution (hence the term
"substitution" in the name).

## Organization
This repo is a Rust workspace comprising of two crates:
* `s5` - the solver library
* `problems` - a crate using `s5` as a dependency to solve problems
in the text _Fundamentals of Astrodynamics_.  

The `s3` crate is what your project would use; `problems` is just an example
crate and is not required to use S<sup>5</sup>.

## Installation
The solver is distributed as a Rust library and can be added to any
Rust project by adding this to `Cargo.toml`:
```toml
s5 = "0.1.0"
```

## Usage
Please see the docs [here](https://docs.rs/s5/0.1.0/s5/)

## Contributing
There are three ways of contributing to S<sup>5</sup>:
1) Improving the solver itself (look for TODOs)
2) Improving documentation
3) Completing the `problems` crate (intends to solve all problems in
_Fundamentals of Astrodynamics_ using S<sup>5</sup>)

Pull requests are welcome! For major changes, please open an issue 
first to discuss what you would like to change.

Please make sure to update tests as appropriate, and all must pass
before merging into `master`.

## License
MIT. Refer to `LICENSE.txt`.
