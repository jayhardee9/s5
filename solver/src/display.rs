use std::fmt;
use std::fmt::{Error, Formatter};

pub trait Printable {
    fn to_print_units(&self) -> PrintUnits;
}

#[derive(Clone, Debug)]
pub struct PrintUnits(Vec<PrintUnit>);

impl PrintUnits {
    pub fn new(units: Vec<PrintUnit>) -> PrintUnits {
        PrintUnits(units)
    }

    /// Move `self` to the right of `others`.
    pub fn right_of(&mut self, others: &PrintUnits) {
        let offset = others.max_x() - self.min_x() + 1;
        self.shift_right(offset);
    }

    /// Move `self` on top of `others`.
    pub fn on_top_of(&mut self, others: &PrintUnits) {
        let offset = self.max_y() - others.min_y() + 1;
        self.shift_down(-offset);
    }

    /// Move `self` on below `others`.
    pub fn below(&mut self, others: &PrintUnits) {
        let offset = others.max_y() - self.min_y() + 1;
        self.shift_down(offset);
    }

    pub fn width(&self) -> usize {
        (self.max_x() - self.min_x() + 1) as usize
    }

    pub fn height(&self) -> usize {
        (self.max_y() - self.min_y() + 1) as usize
    }

    pub fn append(&mut self, others: &PrintUnits) {
        let mut others_copy = others.0.clone();
        self.0.append(&mut others_copy);
    }

    fn min_x(&self) -> i64 {
        self.0
            .iter()
            .map(|print_unit| print_unit.x)
            .min()
            .unwrap_or(0)
    }

    fn max_x(&self) -> i64 {
        self.0
            .iter()
            .map(|print_unit| print_unit.x + print_unit.s.len() as i64 - 1)
            .max()
            .unwrap_or(0)
    }

    fn min_y(&self) -> i64 {
        self.0
            .iter()
            .map(|print_unit| print_unit.y)
            .min()
            .unwrap_or(0)
    }

    fn max_y(&self) -> i64 {
        self.0
            .iter()
            .map(|print_unit| print_unit.y)
            .max()
            .unwrap_or(0)
    }

    /// Shift units by `amount` places to the right.
    pub fn shift_right(&mut self, amount: i64) {
        for print_unit in self.0.iter_mut() {
            print_unit.x += amount
        }
    }

    /// Shift units down by `amount` places.
    pub fn shift_down(&mut self, amount: i64) {
        for print_unit in self.0.iter_mut() {
            print_unit.y += amount
        }
    }
}

impl From<PrintUnit> for PrintUnits {
    fn from(print_unit: PrintUnit) -> Self {
        PrintUnits(vec![print_unit])
    }
}

impl fmt::Display for PrintUnits {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let mut sorted = self.0.clone();
        sorted.sort_by_key(|print_unit| (print_unit.y, print_unit.x));

        if sorted.is_empty() {
            return Ok(());
        }

        let mut sorted = PrintUnits(sorted);

        // Start from (0,0)
        sorted.shift_right(-sorted.min_x());
        sorted.shift_down(-sorted.min_y());
        let mut cur_x = 0;
        let mut cur_y = 0;
        for print_unit in sorted.0 {
            // cur_x, cur_y should always be less than or equal to print_unit.x and print_unit.y,
            // respectively
            if cur_y > print_unit.y {
                panic!();
            }

            if cur_y < print_unit.y {
                cur_x = 0;
            }

            if cur_x > print_unit.x {
                panic!();
            }

            while cur_y < print_unit.y {
                writeln!(f)?;
                cur_y += 1;
            }

            while cur_x < print_unit.x {
                write!(f, " ")?;
                cur_x += 1;
            }

            f.write_str(&print_unit.s)?;

            cur_x += print_unit.s.len() as i64;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct PrintUnit {
    x: i64,
    y: i64,
    s: String,
}

impl PrintUnit {
    pub fn new(s: &str) -> PrintUnit {
        PrintUnit {
            x: 0,
            y: 0,
            s: s.to_owned(),
        }
    }
}
