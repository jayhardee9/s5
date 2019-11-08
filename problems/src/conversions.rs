use crate::constants::*;

pub fn from_feet(n: i64) -> f64 {
    (n as f64) / FEET_PER_METER
}

pub fn from_nautical_miles(n: i64) -> i64 {
    n * METERS_PER_NAUTICAL_MILE
}

pub fn to_feet(f: f64) -> f64 {
    f * FEET_PER_METER
}

pub fn to_nautical_miles(f: f64) -> f64 {
    f / (METERS_PER_NAUTICAL_MILE as f64)
}
