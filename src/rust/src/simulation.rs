use extendr_api::{prelude::*, IntoRobj};
use itertools::Itertools;
use itertools::{izip, repeat_n};
use rand::distributions::WeightedIndex;
use rand::{rngs::SmallRng, Rng, SeedableRng};

#[deprecated = "todo"]
#[derive(Debug)]
struct PopConfig {}

#[derive(Debug, IntoRobj)]
struct Record {
    repetition: usize,
    time: Vec<f64>,
    id_state: Vec<usize>,
    state: Vec<u32>,
}

impl Record {
    fn new(repetition: usize) -> Self {
        let time = Vec::new();
        let state = Vec::new();
        let id_state = Vec::new();
        Self {
            repetition,
            time,
            state,
            id_state,
        }
    }

    fn add_initial_state(&mut self, time: f64, n0: &[u32]) {
        // FIXME: check if the records are empty before inserting initial states..
        self.time.extend(repeat_n(time, n0.len()));
        self.state.extend_from_slice(n0);
        self.id_state.extend(0..n0.len());
    }
}

/// Simulates birth, death and migration process of a multi-patch system.
///
///
/// It is assumed that birth-rate exceeds death-rate.
///
#[extendr]
fn sim_bdm(
    n0: &[i32],
    birth_baseline: &[f64],
    death_baseline: &[f64],
    carrying_capacity: &[i32],
    t_max: f64,
) -> Record {
    assert_eq!(n0.len(), birth_baseline.len());
    assert_eq!(birth_baseline.len(), death_baseline.len());
    assert_eq!(death_baseline.len(), carrying_capacity.len());

    let n_len = n0.len();
    let mut rng = SmallRng::seed_from_u64(20240506);
    let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
    let cc =
        as_u32(carrying_capacity).expect("`carrying_capacity` must be all non-negative integers");
    let cc_double = cc.iter().map(|x| -> f64 { *x as _ }).collect_vec();

    // initialize state
    let mut n: Vec<u32> = Vec::with_capacity(n_len);
    n.extend_from_slice(n0);
    assert_eq!(n.len(), n_len);
    let n = n.as_mut_slice();

    let mut record = Record::new(0);

    // TODO: initial time should be a result of Exp(1), as to have previous time...
    let mut t = 0.0;

    // record initial state
    record.add_initial_state(t, n0);
    loop {
        let delta_t: f64 = rng.sample(rand::distributions::Open01);

        let birth = izip!(
            n.iter(),
            cc_double.iter(),
            birth_baseline.iter(),
            death_baseline.iter()
        )
        .map(|(&n, &cc, &beta, &mu)| {
            let n_double = n as f64;
            let rate = if cc > n_double {
                mu + (cc - n_double) * (beta - mu) / cc
            } else {
                mu
            };
            assert!(rate.is_sign_positive());
            rate * n_double
        });
        // TODO: combine the calculation step of both of these in one
        let death = izip!(
            n.iter(),
            cc_double.iter(),
            birth_baseline.iter(),
            death_baseline.iter()
        )
        .map(|(&n, &cc, &beta, &mu)| {
            let n_double = n as f64;
            let rate = if n_double > cc {
                mu + (n_double - cc) * (beta - mu) / cc
            } else {
                mu
            };
            assert!(rate.is_sign_positive());
            rate * n_double
        });
        let propensity = birth.chain(death).collect_vec();

        let total_propensity: f64 = propensity.iter().sum1().unwrap();
        let delta_t = -delta_t.ln() / total_propensity;
        assert!(delta_t.is_finite());
        t += delta_t;

        if t >= t_max {
            break;
        }

        // next event
        let which_event = WeightedIndex::new(propensity).unwrap();
        let event = rng.sample(&which_event);
        // TODO: use `updated_weights` to speed this up.
        // println!("t = {t}");
        if event < n_len {
            // birth
            n[event] += 1;

            record.time.push(t);
            record.id_state.push(event);
            record.state.push(n[event]);
        } else if event < 2 * n_len {
            // death
            let event_idx = event - n_len;
            assert_ne!(n[event_idx], 0);
            n[event_idx] -= 1;

            record.time.push(t);
            record.id_state.push(event_idx);
            record.state.push(n[event_idx]);
        }
        // TODO: what about if any n == 0 ? extinction?
        if n.iter().all(|&x| x == 0) {
            println!("terminating because no-one is alive anymore");

            // TODO: add an event at `t_max` that is just the repeat of last state

            break;
        }
    }
    record
}

struct ScenarioRecords {
    records: Vec<Record>,
}

// #[extendr]
// fn sim_multiple_bdm(
//     nreps: usize,
//     n0: &[i32],
//     birth_baseline: &[f64],
//     death_baseline: &[f64],
//     t_max: f64,
// ) -> ScenarioRecords {

//     for 0..nreps {

//     }
// }

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
    assert_eq!(carrying_capacity.len(), carrying_capacity.len());

    // k_dij is assumed to be lower triangular, column-wise, w/o diagonal distance "matrix"
    assert_eq!(k_dij.len(), get_total_number_of_elements(n_len));

    let mut rng = SmallRng::seed_from_u64(20240506);
    // let n0: Vec<u32> = n0.iter().map(|&x| x.try_into().unwrap()).collect_vec();
    let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
    let cc =
        as_u32(carrying_capacity).expect("`carrying_capacity` must be all non-negative integers");
    let cc_double = cc.iter().map(|x| -> f64 { *x as _ }).collect_vec();
    dbg!(&cc_double, &cc);
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
        record.id_state.push(source);
        record.state.push(n[source]);
        record.id_state.push(target);
        record.state.push(n[target]);
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

    // rprintln!("before migration {:?}", population_total);
    let n_current = population_total.to_owned();
    let n_current: &[f64] = n_current.as_ref();

    population_total.fill(0.);
    for k in 0..k_dij.len() {
        let [i, j] = get_row_col(k, n_len);
        rprintln!("(i,j) = ({},{})", i, j);
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

    // rprintln!("after migration {:?}", population_total);
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

        // for debug purpose
        // let equation_1_ji = migration_baseline[i];
        // let equation_1_ij = migration_baseline[j];
        let rate_equation_1_ji = equation_1_ji * n_current[i] as f64;
        let rate_equation_1_ij = equation_1_ij * n_current[j] as f64;

        population_total[i] += rate_equation_1_ij - rate_equation_1_ji;
        population_total[j] += rate_equation_1_ji - rate_equation_1_ij;
    }

    // after migration, apply birth/death dynamics.
    let growth_rate: f64 = birth - death;
    // rprintln!("growth_rate: {growth_rate}");
    for i in 0..n_len {
        // rprintln!("before {}", population_total[i]);
        population_total[i] +=
            n_current[i] * growth_rate * (1. - n_current[i] / carrying_capacity[i]);
    }
}

/// Checks if all elements of `integer` are non-negative, thus returns a transmuted unsigned integer slice back.
fn as_u32(integer: &[i32]) -> Option<&[u32]> {
    let any_negative = integer.iter().any(|x| x.is_negative());
    if any_negative {
        return None;
    }
    Some(unsafe { std::mem::transmute(integer) })
}

#[extendr]
/// Returns the row and column from a 0-indexed, column-wise linear index `k` for a square matrix of dimension `n` x `n`
fn get_row_col(k: usize, n: usize) -> [usize; 2] {
    // let j = (((n - 1).pow(2) - 2 * k) as f64).sqrt() as usize - n + 1;
    // todo!();

    // kp := k_max - k

    let kp = (n * (n - 1)) / 2 - 1 - k;
    let kp = kp as f64;
    let p = (((1. + 8. * kp).sqrt() - 1.) / 2.).floor();
    let n_float = n as f64;
    let i = n_float - (kp - p * (p + 1.) / 2.) - 1.;
    let j = n_float - p - 2.;
    //   cbind(i = i, j = j, k = k, kp = kp, p = p)
    [i as _, j as _]
}

#[extendr]
/// Returns the linear, 0-index id for (i,j) for n x n matrix.
fn get_linear_id(i: usize, j: usize, n: usize) -> usize {
    n * (n - 1) / 2 - (n - j + 1) * (n - j) / 2 + (i - j - 1)
}

#[extendr]
/// Return the total number of elements in lower-triangular matrix (without diagonal)
fn get_total_number_of_elements(n: usize) -> usize {
    // FIXME: make safe when n = 0
    n * (n - 1) / 2
}

extendr_module! {
    mod simulation;
    // Gillespie approach
    fn sim_bdm;
    fn sim_migration_only;
    // fn sim_multiple_bdm;

    // ODE approach
    fn update_migration_only;
    fn update_birth_death_and_migration;

    // internal functions, remove eventually
    fn get_row_col;
    fn get_linear_id;
    fn get_total_number_of_elements;
}

#[cfg(test)]
mod tests {
    use extendr_engine::with_r;

    use super::*;

    #[test]
    fn test_name() {
        let n0 = [10, 0, 0, 1];
        let birth_baseline = [4., 4., 4., 4.];
        let death_baseline = [1., 1., 1., 1.];
        // let t_max = 25.;
        let t_max = 5.;
        let cc = [10, 7, 1, 1];

        let record = sim_bdm(&n0, &birth_baseline, &death_baseline, &cc, t_max);
        // dbg!(record);
    }

    #[test]
    fn test_indexing() {
        with_r(|| {});
    }

    #[test]
    fn tri_u_index() {
        // n = 2
        assert_eq!(get_row_col(0, 2), [1, 0]);

        // n = 3
        let ij = get_row_col(0, 3);
        assert_eq!(ij, [1, 0]);
        let ij = get_row_col(1, 3);
        assert_eq!(ij, [2, 0]);
        let ij = get_row_col(2, 3);
        assert_eq!(ij, [2, 1]);

        //TODO: make tests
        // index_to_i_j_colwise_nodiag(1 - 1, 4) # 1 0
        // index_to_i_j_colwise_nodiag(2 - 1, 4) # 2 0
        // index_to_i_j_colwise_nodiag(3 - 1, 4) # 3 0
        // index_to_i_j_colwise_nodiag(4 - 1, 4) # 2 1
        // index_to_i_j_colwise_nodiag(5 - 1, 4) # 3 1
        // index_to_i_j_colwise_nodiag(6 - 1, 4) # 3 2
    }
    #[test]
    fn test_sim_migration_only() {
        let n0 = [10, 2];
        let migration_baseline = [0.1, 0.0001];
        let carrying_capacity = [5, 5];
        let k_dij = [1.];
        let t_max = 100000.;

        sim_migration_only(&n0, &migration_baseline, &carrying_capacity, &k_dij, t_max);
    }
}
