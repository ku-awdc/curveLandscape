use extendr_api::prelude::*;
use itertools::{izip, Itertools};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use readonly::make as readonly;

use crate::linspace;

/// Birth- and death process
#[readonly]
#[derive(Debug)]
struct BirthDeath {
    // Birth rate
    birth: f64,
    death: f64,
    carrying_capacity: f64,
}

#[derive(Debug)]
struct BirthDeathTraj {
    time: f64,
    count: u32,
}

#[extendr]
impl BirthDeath {}

#[inline(always)]
fn rate_to_probability(rate: f64) -> f64 {
    // use expm1.neg()
    1. - (-rate).exp()
}

#[extendr]
fn sim_bd_demo() {
    let result = simulate_birth_death(1.9875, 0.6540, 80., 400);
    todo!()
}

#[derive(Debug)]
pub(crate) struct State {
    repetition: usize,
    time: f64,
    count: f64,
}

fn simulate_birth_death(birth: f64, death: f64, carrying_capacity: f64, reps: usize) -> Vec<State> {
    // birth  death
    // 1.9875 0.6540
    let birth_baseline = birth;
    let death_baseline = death;
    let adjustment = (birth_baseline - death_baseline) / (birth_baseline + death_baseline);
    // carrying_capacity example is 80
    let k0 = carrying_capacity;

    // let reps = 20 * 5 * 2; // 400
    let max_t_years = 25.;
    let u0 = linspace::range(1., 12., 0.5).collect_vec();
    let mut current_u = u0.repeat(reps);
    let mut current_t = [0.].repeat(reps);

    let mut results: Vec<State> = Vec::with_capacity(1000);
    let mut current_rep = (0..reps * u0.len()).collect_vec();
    // record initial states
    for (&repetition, &count, &time) in izip!(current_rep.iter(), u0.iter(), current_t.iter()) {
        results.push(State {
            repetition: repetition + 1,
            time,
            count,
        });
    }

    let mut rng = SmallRng::seed_from_u64(20240416);
    // elapsed and extinct `rep` in a given timestep
    let mut elapsed = Vec::with_capacity(current_rep.len());
    // extinct is defined as u == 0 prior to max_t_years.
    let mut extinct = Vec::with_capacity(current_rep.len());
    // note: extinct and elapsed must not overlap.

    loop {
        assert!(!current_u.is_empty());
        assert!(!current_t.is_empty());

        // clear the indices buffers
        elapsed.clear();
        extinct.clear();

        let current_n = current_u.len();

        let next_unif = rng
            // FIXME: investigate the consequence of this clone
            .clone()
            .sample_iter::<f64, _>(rand::distributions::Open01)
            .take(current_n)
            .collect_vec();

        // does it not suffice that we are doing this at the end?
        // assert_eq!(current_rep.len(), current_t.len());
        // assert_eq!(current_t.len(), current_u.len());
        // assert_eq!(current_u.len(), next_unif.len());

        for (rep, u, next_unif, t) in izip!(
            current_rep.iter(),
            current_u.iter_mut(),
            next_unif.into_iter(),
            current_t.iter_mut()
        ) {
            let birth = birth_baseline * (1. - (adjustment * *u) / k0);
            let death = death_baseline * (1. + (adjustment * *u) / k0);
            assert!(birth > 0.);
            assert!(death > 0.);
            // ALTERNATIVE:
            // delta_t <- rexp(n = current_n, rate = (birth + death) * current_u)
            let delta_t = -next_unif.ln() / ((birth + death) * *u);
            assert!(delta_t.is_finite());
            *t += delta_t;

            // let p_birth = rate_to_probability(birth);
            // let p_death = rate_to_probability(death);
            let delta_u = rng.gen_bool(birth / (birth + death));
            // let delta_u = rng.sample(rand::distributions::Bernoulli::new(birth / (birth + death)))
            let delta_u = if delta_u { 1. } else { 0. };
            let delta_u = 2. * delta_u - 1.;
            *u += delta_u;
            // or is the population past simulation time?
            if *t >= max_t_years {
                elapsed.push(*rep);
            } else {
                // is the population extinct?
                if *u == 0. {
                    extinct.push(*rep);
                }
            }
            // what if a group is elapsed and extinct?
            // an extinct group is defined as extinct if it dies out prior to
            // the end of the simulation
        }

        // record
        for (&rep, &time, &count) in izip!(current_rep.iter(), current_t.iter(), current_u.iter()) {
            results.push(State {
                repetition: rep + 1,
                time,
                count,
            });
        }

        // retire elapsed trajectories

        // record end time for extinct groups
        for rep_extinct in extinct.iter() {
            results.push(State {
                repetition: rep_extinct + 1,
                time: max_t_years,
                count: 0.,
            });
        }

        // remove the elapsed simulations from consideration
        while let Some((id, rep)) = current_rep.iter().enumerate().next() {
            if !extinct.contains(rep) && !elapsed.contains(rep) {
                continue;
            }
            current_rep.remove(id);
            current_t.remove(id);
            current_u.remove(id);
        }
        
        assert_eq!(current_rep.len(), current_t.len());
        assert_eq!(current_t.len(), current_u.len());

        // if there are no more to simulate, exit
        if current_u.is_empty() {
            break;
        }
    }
    //TODO: return something
    results
}
