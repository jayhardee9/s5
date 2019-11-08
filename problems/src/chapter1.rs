use solver::equations::Equation;
use solver::variables::Variable;
use solver::formulas::Formula;
use std::f64::consts::PI;
use solver::solver_state::SolverState;
use crate::conversions::*;
use crate::constants::*;
use crate::chapter1::OrbitType::*;

macro_rules! def_variable {
    ($const_name:ident, $var_name:expr, $description:expr) => {
        let $const_name = &Variable::new($var_name, $description);
    };
}

fn general_specific_energy_equation(specific_energy: &Variable, u: &Variable, speed: &Variable, distance: &Variable) -> Equation {
    Equation::new(
        specific_energy.into(), ((speed ^ 2) / 2) - (u / distance),
    )
}

#[derive(Debug)]
enum OrbitType {
    Circle,
    Ellipse,
    Parabola,
    Hyperbola,
}

impl OrbitType {
    pub fn classify_by_eccentricity(e: f64) -> OrbitType {
        if approx_eq!(f64, e, 0f64) {
            return Circle;
        }

        if e < 1f64 {
            return Ellipse;
        }

        if approx_eq!(f64, e, 1f64) {
            return Parabola;
        }

        Hyperbola
    }

    pub fn classify_by_semi_major_axis(a: f64) -> Option<OrbitType> {
        if a < 0f64 {
            return Hyperbola.into();
        }
        None
    }
}

pub fn run() {
    def_variable!(v_x, "v_x", "x-coordinate of velocity");
    def_variable!(v_y, "v_y", "y-coordinate of velocity");
    def_variable!(v_z, "v_z", "z-coordinate of velocity");
    def_variable!(v, "v", "speed");
    def_variable!(me, "E", "specific mechanical energy");
    def_variable!(r_x, "r_x", "x-coordinate of position");
    def_variable!(r_y, "r_y", "y-coordinate of position");
    def_variable!(r_z, "r_z", "z-coordinate of position");
    def_variable!(r, "r", "distance");
    def_variable!(u, "u", "gravitational parameter");
    def_variable!(h, "h", "specific angular momentum");
    def_variable!(h_x, "h_x", "x-coordinate specific angular momentum");
    def_variable!(h_y, "h_y", "y-coordinate specific angular momentum");
    def_variable!(h_z, "h_z", "z-coordinate specific angular momentum");
    def_variable!(phi, "phi", "flight path angle");
    def_variable!(sgn_phi, "sgn_phi", "signum of the flight path angle");
    def_variable!(gamma, "gamma", "zenith angle");
    def_variable!(p, "p", "semi-latus rectum");
    def_variable!(nu, "nu", "true anomaly");
    def_variable!(e, "e", "eccentricity");
    def_variable!(e_x, "e_x", "x-coordinate of eccentricity vector");
    def_variable!(e_y, "e_y", "y-coordinate of eccentricity vector");
    def_variable!(e_z, "e_z", "z-coordinate of eccentricity vector");
    def_variable!(a, "a", "semi-major axis");
    def_variable!(r_p, "r_p", "distance at periapsis");
    def_variable!(r_a, "r_a", "distance at apoapsis");
    def_variable!(tp, "TP", "orbital period");
    def_variable!(c, "c", "linear eccentricity");
    def_variable!(v_p, "v_p", "speed at periapsis");
    def_variable!(v_a, "v_a", "speed at apoapsis");

    let half = Formula::from(1) / 2;
    let three_halves = Formula::from(3) / 2;

    let two_body_equations =
        vec![
            general_specific_energy_equation(me, u, v, r),
            general_specific_energy_equation(me, u, v_a, r_a),
            general_specific_energy_equation(me, u, v_p, r_p),
            Equation::new_assignment(me, Formula::from(0) - (u / (2 * a))),
            Equation::new_assignment(h_x, (r_y * v_z) - (r_z * v_y)),
            Equation::new_assignment(h_y, (r_z * v_x) - (r_x * v_z)),
            Equation::new_assignment(h_z, (r_x * v_y) - (r_y * v_x)),

            Equation::new_assignment(h, r * v * (Formula::from(phi).cos())),
            Equation::new_assignment(h, r * v * (Formula::from(gamma).sin())),
            Equation::new_assignment(h, ((h_x ^ 2) + (h_y ^ 2) + (h_z ^ 2)) ^ half.clone()),
            Equation::new_assignment(h, r_p * v_p),
            Equation::new_assignment(h, r_a * v_a),

            Equation::new_assignment(sgn_phi, Formula::from(phi).sgn()),

            Equation::new_assignment(r, p / (1 + (e * (Formula::from(nu).cos())))),
            Equation::new_assignment(r, ((r_x ^ 2) + (r_y ^ 2) + (r_z ^ 2)) ^ half.clone()),
            Equation::new_assignment(e_x, (((v_y * h_z) - (v_z * h_y)) / u) - (r_x / r)),
            Equation::new_assignment(e_y, (((v_z - h_x) - (v_x - h_z)) / u) - (r_y / r)),
            Equation::new_assignment(e_z, (((v_x - h_y) - (v_y - h_x)) / u) - (r_z / r)),
            Equation::new(u * e_x, (((v ^ 2) - (u / r)) * r_x) - (((r_x * v_x) + (r_y * v_y) + (r_z * v_z)) * v_x)),
            Equation::new(u * e_y, (((v ^ 2) - (u / r)) * r_y) - (((r_x * v_x) + (r_y * v_y) + (r_z * v_z)) * v_y)),
            Equation::new(u * e_z, (((v ^ 2) - (u / r)) * r_z) - (((r_x * v_x) + (r_y * v_y) + (r_z * v_z)) * v_z)),
            Equation::new_assignment(p, (h ^ 2) / u),
            Equation::new_assignment(p, a * (1 - (e ^ 2))),

            Equation::new_assignment(e, (Formula::from(1) + (2 * me * (h ^ 2)) / (u ^ 2)) ^ half.clone()),
            Equation::new_assignment(e, (r_a - r_p) / (r_a + r_p)),
            Equation::new_assignment(e, ((e_x ^ 2) + (e_y ^ 2) + (e_z ^ 2)) ^ half.clone()),
            Equation::new_assignment(e, c / a),

            Equation::new_assignment(tp, Formula::from(2.0 * PI) / (u ^ half.clone()) * (a ^ three_halves.clone())),
            Equation::new_assignment(r_a, p / (1 - e)),
            Equation::new_assignment(r_a, a * (1 + e)),
            Equation::new_assignment(r_p, p / (1 + e)),
            Equation::new_assignment(r_p, a * (1 - e)),
            Equation::new(r_p + r_a, 2 * a),
            Equation::new_assignment(v, ((v_x ^ 2) + (v_y ^ 2) + (v_z ^ 2)) ^ half.clone()),
        ];

    let mut state = SolverState::new();
    state.add_equations(&two_body_equations);
    state.add_signum_for(phi, sgn_phi);

    println!("Problem 1.1");

    state.bind_variable(r_x, &2.into());
    state.bind_variable(r_y, &2.into());
    state.bind_variable(r_z, &2.into());

    state.bind_variable(v_x, &(-0.4).into());
    state.bind_variable(v_y, &0.2.into());
    state.bind_variable(v_z, &0.4.into());

    state.bind_variable(u, &1.into());

    state.deduce();

    println!("{} = {}", h_x, state.get_binding(h_x).expect("h_x solved"));
    println!("{} = {}", h_y, state.get_binding(h_y).expect("h_y solved"));
    println!("{} = {}", h_z, state.get_binding(h_z).expect("h_z solved"));
    println!("{} = {}", me, state.get_binding(me).expect("me solved"));

    println!("\nProblem 1.2");
    state.clear_bindings();

    state.bind_variable(v, &from_feet(45_000).into());
    state.bind_variable(r, &from_nautical_miles(4_000).into());
    state.bind_variable(p, &from_nautical_miles(4_000).into());
    state.bind_variable(u, &EARTH_GRAVITATIONAL_PARAMETER.into());

    state.deduce();

    println!("{} = {}", e, state.get_binding(e).expect("e solved"));

    println!("\nProblem 1.3");
    state.clear_bindings();

    state.bind_variable(r_p, &(from_nautical_miles(100) as f64 + EARTH_RADIUS).into());
    state.bind_variable(r_a, &(from_nautical_miles(600) as f64 + EARTH_RADIUS).into());
    state.bind_variable(u, &EARTH_GRAVITATIONAL_PARAMETER.into());

    state.deduce();

    println!("{} = {}", tp, state.get_binding(tp).expect("TP solvecd"));

    println!("\nProblem 1.5");
    state.clear_bindings();

    state.bind_variable(a, &from_feet(30_000_000).into());
    state.bind_variable(e, &0.2.into());
    state.bind_variable(u, &EARTH_GRAVITATIONAL_PARAMETER.into());
    state.bind_variable(nu, &135f64.to_radians().into());

    state.deduce();

    println!("{} = {}", r_p, state.get_binding(r_p).expect("r_p solved"));
    println!("{} = {}", r_a, state.get_binding(r_a).expect("r_a solved"));
    println!("{} = {}", me, state.get_binding(me).expect("me solved"));
    println!("{} = {}", p, state.get_binding(p).expect("p solved"));
    let r_feet = to_feet(state.get_binding(r).expect("r solved").to_f64());
    println!("{} = {} ft", r, r_feet);

    println!("\nProblem 1.8");
    state.clear_bindings();

    print!("\n(a) ");

    state.add_binding(r, &3.into());
    state.add_binding(v, &1.5.into());
    state.add_binding(u, &1.into());

    state.deduce();

    println!("{:?} ", OrbitType::classify_by_semi_major_axis(state.get_binding(a).expect("a solved").to_f64()).expect("classification"));

    print!("\n(b) ");
    state.clear_bindings();

    state.add_binding(r_p, &1.5.into());
    state.add_binding(p, &3.into());
    state.add_binding(u, &1.into());

    state.deduce();

    println!("{:?} ", OrbitType::classify_by_eccentricity(state.get_binding(e).expect("e solved").to_f64()));

    print!("\n(c) ");
    state.clear_bindings();

    state.add_binding(me, &(-1.0 / 3.0).into());
    state.add_binding(p, &1.5.into());
    state.add_binding(u, &1.into());

    state.deduce();

    println!("{:?} ", OrbitType::classify_by_eccentricity(state.get_binding(e).expect("e solved").to_f64()));

    print!("\n(d) ");
    state.clear_bindings();

    state.add_binding(r_x, &0.into());
    state.add_binding(r_y, &1.into());
    state.add_binding(r_z, &0.2.into());

    state.add_binding(v_x, &0.9.into());
    state.add_binding(v_y, &0.into());
    state.add_binding(v_z, &0.123.into());

    state.add_binding(u, &1.into());

    state.deduce();

    println!("{:?} ", OrbitType::classify_by_eccentricity(state.get_binding(e).expect("e solved").to_f64()));

    print!("\n(e) ");
    state.clear_bindings();

    state.add_binding(r_x, &0.into());
    state.add_binding(r_y, &0.into());
    state.add_binding(r_z, &1.01.into());

    state.add_binding(v_x, &1.into());
    state.add_binding(v_y, &0.into());
    state.add_binding(v_z, &1.4.into());

    state.add_binding(u, &1.into());

    state.deduce();

    println!("{:?} ", OrbitType::classify_by_eccentricity(state.get_binding(e).expect("e solved").to_f64()));

    println!("\nProblem 1.9");
    state.clear_bindings();

    state.add_binding(v, &from_feet(25_000).into());
    state.add_binding(r, &(from_feet(300_000) + EARTH_RADIUS).into());
    state.add_binding(phi, &(-60f64.to_radians()).into());
    state.add_binding(u, &EARTH_GRAVITATIONAL_PARAMETER.into());

    state.deduce();

    let eccentricity = state.get_binding(e).expect("e solved");
    let specific_angular_momentum = state.get_binding(h).expect("h solved");

    state.clear_bindings();

    state.add_binding(e, &eccentricity);
    state.add_binding(h, &specific_angular_momentum);
    state.add_binding(r, &(from_nautical_miles(100) as f64 + EARTH_RADIUS).into());
    state.add_binding(u, &EARTH_GRAVITATIONAL_PARAMETER.into());

    let n_one = -1i64;
    state.add_binding(sgn_phi, &n_one.into());

    state.deduce();

    let v_fps = to_feet(state.get_binding(v).expect("v solved").to_f64());
    println!("{} = {} ft/sec", v, v_fps);

    let phi_degs = state.get_binding(phi).expect("phi solved").to_f64().to_degrees();
    println!("{} = {} deg", phi, phi_degs);

    println!("\nProblem 1.11");
    state.clear_bindings();

    state.add_binding(v, &from_feet(10_000).into());
    state.add_binding(r, &(from_feet(100_000) + EARTH_RADIUS).into());
    state.add_binding(u, &EARTH_GRAVITATIONAL_PARAMETER.into());
    state.add_binding(phi, &(90f64.to_radians()).into());

    state.deduce();

    let radius_max = state.get_binding(r_a).expect("r_a solved").to_f64();
    let alt_max = radius_max - EARTH_RADIUS;
    println!("max altitude = {}", alt_max);
}
