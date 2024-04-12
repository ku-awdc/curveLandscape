use extendr_api::prelude::*;
use readonly::make as readonly;


/// Birth- and death process
#[readonly]
#[derive(Debug)]
struct BirthDeath {
    // Birth rate
    birth: f64,
    death: f64,
}

#[derive(Debug)]
struct BirthDeathTraj {
    time: f64,
    count: u32,
}

#[extendr]
impl BirthDeath {}

fn simulate_birth_death(count_0: u32) {
    // let mut traj = Vec::with_capacity(capacity);
}