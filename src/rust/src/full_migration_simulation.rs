use crate::{
    simulation::{as_u32, Record},
    triangle_indexing::{get_row_col, get_total_number_of_elements},
};

use extendr_api::prelude::*;
use itertools::Itertools;
use rand::prelude::*;
use rand_distr::WeightedIndex;

/// Perform only simulation with migration, no birth/death feedback
#[extendr]
fn sim_migration_only(
    n0: &[i32],
    migration_baseline: &[f64],
    carrying_capacity: &[i32],
    k_dij: &[f64],
    t_max: f64,
) -> Record {
    let n_len = n0.len();
    // assume: |vec(n0)| = |vec(m0)| = |vec(cc)|
    assert_eq!(n_len, migration_baseline.len());
    assert_eq!(migration_baseline.len(), carrying_capacity.len());

    // k_dij is assumed to be lower triangular, column-wise, w/o diagonal distance "matrix"
    assert_eq!(k_dij.len(), get_total_number_of_elements(n_len));

    let mut rng = SmallRng::seed_from_u64(20240506);
    // let n0: Vec<u32> = n0.iter().map(|&x| x.try_into().unwrap()).collect_vec();
    let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
    let cc =
        as_u32(carrying_capacity).expect("`carrying_capacity` must be all non-negative integers");
    let cc_double = cc.iter().map(|x| -> f64 { *x as _ }).collect_vec();
    // dbg!(&cc_double, &cc);
    // initialize state
    let mut n: Vec<u32> = Vec::with_capacity(n_len);
    n.extend_from_slice(n0);
    assert_eq!(n.len(), n_len);
    let n = n.as_mut_slice(); // state variable

    let mut record = Record::new(0);

    // TODO: initial time should be a result of Exp(1), as to have previous time...
    let mut t = 0.0;

    // record initial state
    record.add_initial_state(t, n0);

    //TODO: make them into slices?
    let mut emigration = vec![0.; k_dij.len()];
    let emigration = emigration.as_mut_slice();
    let mut immigration = vec![0.; k_dij.len()];
    let immigration = immigration.as_mut_slice();
    loop {
        let delta_t: f64 = rng.sample(rand::distributions::Open01);

        // Equation (1) m0 • k(d_ij) • exp(-cc[source])
        // Equation (2) m0 • k(d_ij) • exp(-cc[source]) • exp(-(cc[target] - n[source??]))

        let mut total_propensity: f64 = 0.;
        let mut emigration_propensity: f64 = 0.;
        let mut immigration_propensity: f64 = 0.;
        for k in 0..k_dij.len() {
            let [i, j] = get_row_col(k, n_len);
            // since k_dij is symmetric, k_dij[k] "=" k_dji[k] let us say...
            let equation_1_ji = migration_baseline[i] * k_dij[k] * (-cc_double[i]).exp();
            let equation_1_ij = migration_baseline[j] * k_dij[k] * (-cc_double[j]).exp();
            // let equation_1_ji = migration_baseline[i] * k_dij[k] / cc_double[i];
            // let equation_1_ij = migration_baseline[j] * k_dij[k] / cc_double[j];

            let rate_equation_1_ji = equation_1_ji * n[i] as f64;
            let rate_equation_1_ij = equation_1_ij * n[j] as f64;
            //TODO: test if they are sign positive?

            emigration[k] = rate_equation_1_ji;
            immigration[k] = rate_equation_1_ij;

            emigration_propensity += rate_equation_1_ji;
            immigration_propensity += rate_equation_1_ij;

            total_propensity += rate_equation_1_ji + rate_equation_1_ij;

            // TODO: add equation_2_ij, equation_2_ji
        }
        let total_propensity = total_propensity;
        let emigration_propensity = emigration_propensity;
        let immigration_propensity = immigration_propensity;

        // dbg!(total_propensity);
        // dbg!(&immigration, &emigration, total_propensity);
        let delta_t = -delta_t.ln() / total_propensity;
        assert!(delta_t.is_finite());
        t += delta_t;

        if t >= t_max {
            break;
        }

        // next event
        // TODO: use `updated_weights` to speed this up.
        let which_component =
            WeightedIndex::new([emigration_propensity, immigration_propensity]).unwrap();
        let which_component = rng.sample(&which_component);

        let current_component_propensity = if which_component == 0 {
            // emigration
            emigration.as_ref()
        } else {
            // immigration
            immigration.as_ref()
        };
        // TODO: use `updated_weights` to speed this up.
        let which_k = WeightedIndex::new(current_component_propensity).unwrap();
        let which_k = rng.sample(&which_k);

        let [source, target] = if which_component == 0 {
            // emigration
            let [i, j] = get_row_col(which_k, n_len);
            [i, j]
        } else {
            // immigration
            let [i, j] = get_row_col(which_k, n_len);
            [j, i]
        };

        n[source] -= 1;
        n[target] += 1;
        record.time.push(t);
        record.time.push(t);
        record.id_patch.push(source);
        record.count.push(n[source]);
        record.id_patch.push(target);
        record.count.push(n[target]);
    }
    record
}

/// Updates the population state vector according to the migration equation
///
/// (CURRENT) Equation (1 - exp) m0 • k(d_ij) • exp(-cc[source])
/// (MISSING) Equation (1 - div) m0 • k(d_ij) / cc[source]
/// (MISSING) Equation (2 - exp) m0 • k(d_ij) • exp(-cc[source]) • exp(-(cc[target] - n[source]))
/// (MISSING) Equation (2 - div) m0 • k(d_ij) n[source] / (cc[source]) • cc[target])
///
///
#[extendr]
fn update_migration_only(
    population_total: &mut [f64],
    migration_baseline: &[f64],
    carrying_capacity: &[f64],
    k_dij: &[f64],
) {
    let n_len = population_total.len();
    // k_dij is assumed to be lower triangular, column-wise, w/o diagonal distance "matrix"
    assert_eq!(k_dij.len(), get_total_number_of_elements(n_len));
    // assume: |vec(n)| = |vec(m0)| = |vec(cc)|
    assert_eq!(n_len, migration_baseline.len());
    assert_eq!(migration_baseline.len(), carrying_capacity.len());

    let n_current = population_total.to_owned();
    let n_current: &[f64] = n_current.as_ref();

    population_total.fill(0.);
    for k in 0..k_dij.len() {
        let [i, j] = get_row_col(k, n_len);
        // since k_dij is symmetric, k_dij[k] "=" k_dji[k] let us say...
        // let equation_1_ji = migration_baseline[i] * k_dij[k] * (-carrying_capacity[i]).exp();
        // let equation_1_ij = migration_baseline[j] * k_dij[k] * (-carrying_capacity[j]).exp();

        // verified!
        let equation_1_ji = migration_baseline[i] * k_dij[k] / carrying_capacity[i];
        let equation_1_ij = migration_baseline[j] * k_dij[k] / carrying_capacity[j];
        // let equation_1_ji = migration_baseline[i] * k_dij[k] * (-carrying_capacity[i]).exp();
        // let equation_1_ij = migration_baseline[j] * k_dij[k] * (-carrying_capacity[j]).exp();

        // for debug purpose
        // let equation_1_ji = migration_baseline[i];
        // let equation_1_ij = migration_baseline[j];
        let rate_equation_1_ji = equation_1_ji * n_current[i] as f64;
        let rate_equation_1_ij = equation_1_ij * n_current[j] as f64;

        population_total[i] += rate_equation_1_ij - rate_equation_1_ji;
        population_total[j] += rate_equation_1_ji - rate_equation_1_ij;
    }
}

/// Updates the population state vector according to the migration equation together with
/// density-dependent birth-death.
///
/// (CURRENT) Equation (1 - exp) m0 • k(d_ij) • exp(-cc[source])
/// (MISSING) Equation (1 - div) m0 • k(d_ij) / cc[source]
/// (MISSING) Equation (2 - exp) m0 • k(d_ij) • exp(-cc[source]) • exp(-(cc[target] - n[source]))
/// (MISSING) Equation (2 - div) m0 • k(d_ij) n[source] / (cc[source]) • cc[target])
///
///
#[extendr]
fn update_birth_death_and_migration(
    population_total: &mut [f64],
    birth: f64,
    death: f64,
    migration_baseline: &[f64],
    // carrying_capacity: &[i32],
    carrying_capacity: &[f64],
    k_dij: &[f64],
) {
    let n_len = population_total.len();
    // assume: |vec(n0)| = |vec(m0)| = |vec(cc)|
    assert_eq!(n_len, migration_baseline.len());
    assert_eq!(migration_baseline.len(), carrying_capacity.len());
    // k_dij is assumed to be lower triangular, column-wise, w/o diagonal distance "matrix"
    assert_eq!(k_dij.len(), get_total_number_of_elements(n_len));

    let n_current = population_total.to_owned();
    let n_current: &[f64] = n_current.as_ref();

    population_total.fill(0.);
    for k in 0..k_dij.len() {
        let [i, j] = get_row_col(k, n_len);
        // since k_dij is symmetric, k_dij[k] "=" k_dji[k] let us say...
        // let equation_1_ji = migration_baseline[i] * k_dij[k] * (-carrying_capacity[i]).exp();
        // let equation_1_ij = migration_baseline[j] * k_dij[k] * (-carrying_capacity[j]).exp();

        // verified!
        let equation_1_ji = migration_baseline[i] * k_dij[k] / carrying_capacity[i];
        let equation_1_ij = migration_baseline[j] * k_dij[k] / carrying_capacity[j];
        // let equation_1_ji = migration_baseline[i] * k_dij[k] * (-carrying_capacity[i]).exp();
        // let equation_1_ij = migration_baseline[j] * k_dij[k] * (-carrying_capacity[j]).exp();

        let rate_equation_1_ji = equation_1_ji * n_current[i] as f64;
        let rate_equation_1_ij = equation_1_ij * n_current[j] as f64;

        population_total[i] += rate_equation_1_ij - rate_equation_1_ji;
        population_total[j] += rate_equation_1_ji - rate_equation_1_ij;
    }

    // after migration, apply birth/death dynamics.
    let growth_rate: f64 = birth - death;
    for i in 0..n_len {
        population_total[i] +=
            n_current[i] * growth_rate * (1. - n_current[i] / carrying_capacity[i]);
    }
}
