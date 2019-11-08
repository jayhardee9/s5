use std::ops;
use crate::display::{Printable, PrintUnit, PrintUnits};

enum EConst {
    IntConst(i64),

    FloatConst(f64),
}

impl Printable for EConst {
    fn to_print_units(&self) -> PrintUnits {
        let s = match self {
            EConst::IntConst(i) => {
                i.to_string()
            },

            EConst::FloatConst(f) => {
                f.to_string()
            },
        };

        PrintUnits::new(vec![PrintUnit::new(s.as_str())])
    }
}

pub struct Const {
    c: EConst,
}

impl Printable for Const {
    fn to_print_units(&self) -> PrintUnits {
        self.c.to_print_units()
    }
}

impl From<i64> for Const {
    fn from(n: i64) -> Self {
        Const {
            c: EConst::IntConst(n),
        }
    }
}

impl From<f64> for Const {
    fn from(f: f64) -> Self {
        Const {
            c: EConst::FloatConst(f),
        }
    }
}

macro_rules! const_op {
    ($($trait:ident)::+, $fn:ident) => {
        impl $($trait)::+<Const> for Const {
            type Output = Const;

            fn $fn(self, rhs: Const) -> Self::Output {
                match (self.c, rhs.c) {
                    (EConst::IntConst(i1), EConst::IntConst(i2)) => {
                        Const {
                            c: EConst::IntConst(i1.$fn(i2)),
                        }
                    },

                    (EConst::IntConst(i), EConst::FloatConst(f)) => {
                        Const {
                            c: EConst::FloatConst((i as f64).$fn(f)),
                        }
                    },

                    (EConst::FloatConst(f), EConst::IntConst(i)) => {
                        Const {
                            c: EConst::FloatConst(f.$fn(i as f64)),
                        }
                    },

                    (EConst::FloatConst(f1), EConst::FloatConst(f2)) => {
                        Const {
                            c: EConst::FloatConst(f1.$fn(f2)),
                        }
                    },
                }
            }
        }

        impl $($trait)::+<i64> for Const {
            type Output = Const;

            fn $fn(self, rhs: i64) -> Self::Output {
                self.$fn(Const::from(rhs))
            }
        }

        impl $($trait)::+<Const> for i64 {
            type Output = Const;

            fn $fn(self, rhs: Const) -> Self::Output {
                Const::from(self).$fn(rhs)
            }
        }

        impl $($trait)::+<f64> for Const {
            type Output = Const;

            fn $fn(self, rhs: f64) -> Const {
                self.$fn(Const::from(rhs))
            }
        }

        impl $($trait)::+<Const> for f64 {
            type Output = Const;

            fn $fn(self, rhs: Const) -> Const {
                Const::from(self).$fn(rhs)
            }
        }
    };
}

const_op!(ops::Add, add);
const_op!(ops::Sub, sub);
const_op!(ops::Mul, mul);
const_op!(ops::Div, div);

// Using the ^ for exponents
impl ops::BitXor<i64> for Const {
    type Output = Const;

    fn bitxor(self, rhs: i64) -> Self::Output {
        match self.c {
            EConst::IntConst(i) => {
                if i < 0 {
                    Const {
                        c: EConst::FloatConst((i as f64).powi(rhs as i32)),
                    }
                } else {
                    Const {
                        c: EConst::IntConst(i.pow(rhs as u32)),
                    }
                }
            },

            EConst::FloatConst(f) => {
                Const {
                    c: EConst::FloatConst(f.powi(rhs as i32)),
                }
            },
        }
    }
}

impl ops::BitXor<Const> for i64 {
    type Output = Const;

    fn bitxor(self, rhs: Const) -> Self::Output {
        match rhs.c {
            EConst::IntConst(i) => {
                if i < 0 {
                    Const {
                        c: EConst::FloatConst((self as f64).powi(i as i32)),
                    }
                } else {
                    Const {
                        c: EConst::IntConst(self.pow(i as u32)),
                    }
                }
            },

            EConst::FloatConst(f) => {
                Const {
                    c: EConst::FloatConst((self as f64).powf(f)),
                }
            },
        }
    }
}

impl ops::BitXor<Const> for Const {
    type Output = Const;

    fn bitxor(self, rhs: Const) -> Self::Output {
        match (self.c, rhs.c) {
            (EConst::IntConst(i1), EConst::IntConst(i2)) => {
                if i2 < 0 {
                    Const {
                        c: EConst::FloatConst((i1 as f64).powi(i2 as i32)),
                    }
                } else {
                    Const {
                        c: EConst::IntConst(i1.pow(i2 as u32)),
                    }
                }
            },

            (EConst::IntConst(i), EConst::FloatConst(f)) => {
                Const {
                    c: EConst::FloatConst((i as f64).powf(f)),
                }
            },

            (EConst::FloatConst(f), EConst::IntConst(i)) => {
                Const {
                    c: EConst::FloatConst(f.powf(i as f64)),
                }
            },

            (EConst::FloatConst(f1), EConst::FloatConst(f2)) => {
                Const {
                    c: EConst::FloatConst(f1.powf(f2)),
                }
            },
        }
    }
}

impl ops::BitXor<f64> for Const {
    type Output = Const;

    fn bitxor(self, rhs: f64) -> Const {
        match self.c {
            EConst::IntConst(i) => {
                Const {
                    c: EConst::FloatConst((i as f64).powf(rhs)),
                }
            },

            EConst::FloatConst(f) => {
                Const {
                    c: EConst::FloatConst(f.powf(rhs)),
                }
            },
        }
    }
}

impl ops::BitXor<Const> for f64 {
    type Output = Const;

    fn bitxor(self, rhs: Const) -> Const {
        match rhs.c {
            EConst::IntConst(i) => {
                Const {
                    c: EConst::FloatConst(self.powf(i as f64)),
                }
            },

            EConst::FloatConst(f) => {
                Const {
                    c: EConst::FloatConst(self.powf(f)),
                }
            },
        }
    }
}
