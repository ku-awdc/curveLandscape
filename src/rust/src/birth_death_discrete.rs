use extendr_api::prelude::*;
use itertools::Itertools;
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

fn sim_bd_demo() {
    simulate_birth_death(1.9875, 0.6540, 80.)
}

#[derive(Debug)]
pub(crate) struct State {
    repetition: usize,
    time: f64,
    count: f64,
}

fn simulate_birth_death(birth: f64, death: f64, carrying_capacity: f64) {
    // birth  death
    // 1.9875 0.6540
    let birth_baseline = birth;
    let death_baseline = death;
    let adjustment = (birth_baseline - death_baseline) / (birth_baseline + death_baseline);
    let k0 = carrying_capacity;

    let reps = 20 * 5 * 2;
    let max_t_years = 25.;
    let u0 = linspace::range(1., 12., 0.5).collect_vec();
    let mut current_u = u0.repeat(reps);
    let mut current_t = [0.].repeat(reps);

    let mut results: Vec<State> = Vec::with_capacity(1000);
    //   id_rep <- seq_along(current_u)
    // add initial states
    for rep in 0..reps {
        for (&count, &time) in u0.iter().zip_eq(current_t.iter()) {
            results.push(State {
                repetition: rep + 1,
                time,
                count,
            })
        }
    }
    loop {

        //   repeat {

        //     stopifnot(all(!is.na(current_u),
        //                   !is.na(current_t)))

        //     current_n <- length(current_u)
        //     next_unif <- runif(current_n)

        //     stopifnot(
        //       vapply(next_unif, \(x) !isTRUE(all.equal.numeric(x, 0)), FUN.VALUE = logical(1))
        //     )
        //     # stopifnot(current_u < k0)

        //     birth <- birth_baseline * (1 - (birth_baseline - death_baseline) / (birth_baseline + death_baseline) * current_u / k0)
        //     death <- death_baseline * (1 + (birth_baseline - death_baseline) / (birth_baseline + death_baseline) * current_u / k0)
        //     stopifnot(all(birth > 0), all(death > 0))

        //     delta_t <- -log(next_unif) / ((birth + death) * current_u)
        //     # ALTERNATIVE:
        //     # delta_t <- rexp(n = current_n, rate = (birth + death) * current_u)
        //     stopifnot(all(!is.infinite(delta_t)))
        //     next_t <- current_t + delta_t

        //     # IDEA: use rexp instead you crazy person
        //     # birth <- yearly_prob_to_weekly_prob(rate_to_probability(birth))
        //     # death <- yearly_prob_to_weekly_prob(death)
        //     birth <- rate_to_probability(birth)
        //     death <- rate_to_probability(death)
        //     delta_u <- rbinom(n = current_n, size = 1, prob = birth / (birth + death))
        //     delta_u <- 2 * delta_u - 1
        //     next_u <- current_u + delta_u

        //     # shift
        //     current_u <- next_u
        //     current_t <- next_t

        //     # record
        //     results <- rbind(
        //       results,
        //       cbind(rep = id_rep, t = current_t, u = current_u)
        //     )

        //     # retire elapsed trajectories
        //     # elapsed_t <- which(current_t >= max_t_weeks)
        //     elapsed_t <- which(current_t >= max_t_years)
        //     elapsed_u <- which(current_u == 0)
        //     elapsed_reps <- unique(c(elapsed_t, elapsed_u))

        //     if (length(elapsed_reps) > 0) {
        //       # message(glue("{length(elapsed_u)}"))

        //       #add `t_max` to the `elapsed_u` group
        //       results <- rbind(
        //         results, cbind(rep = id_rep[elapsed_u],
        //                        # t = current_t[elapsed_u],
        //                        t = rep.int(max_t_years, length(elapsed_u)),
        //                        u = current_u[elapsed_u])
        //       )

        //       # remove the elapsed simulations from consideration
        //       current_u <- current_u[-elapsed_reps]
        //       current_t <- current_t[-elapsed_reps]
        //       id_rep <- id_rep[-elapsed_reps]
        //     }
        //     stopifnot(length(current_u) == length(current_t),
        //               length(current_t) == length(id_rep)
        //     )
        //     if (length(current_u) == 0) {
        //       break
        //     }
        //   }
    }
    //TODO: return something
    
}
