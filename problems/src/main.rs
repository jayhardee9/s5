use solver;
use solver::variables::Variable;
use solver::formulas::Formula;

fn main() {
    let s = Variable::new("s", "speed of satellite");
    let me = Variable::new("E", "specific mechanical energy of orbit");
    let r = Variable::new("r", "distance of satellite from center of Earth");
    let u = Variable::new("u", "gravitational parameter of Earth");

    let f: Formula = ((s ^ 2) / 2) - (u / r);

    println!("{}", f);
}
