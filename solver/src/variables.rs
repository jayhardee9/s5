use crate::display::{Printable, PrintUnit, PrintUnits};

pub struct Variable {
    name: String,
    description: String,
}

impl Variable {
    pub fn new(name: &str, description: &str) -> Variable {
        Variable {
            name: name.into(),
            description: description.into(),
        }
    }
}

impl Printable for Variable {
    fn to_print_units(&self) -> PrintUnits {
        PrintUnits::new(vec![PrintUnit::new(&self.name)])
    }
}