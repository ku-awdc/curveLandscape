use extendr_api::{prelude::*, IntoRobj};
use itertools::Itertools;
use itertools::{izip, repeat_n};
use rand::{rngs::SmallRng, Rng, SeedableRng};

#[derive(Debug)]
struct PopConfig {
    // initial_count: [],
}

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
#[extendr]
fn sim_bdm(n0: &[i32], birth_baseline: &[f64], death_baseline: &[f64], t_max: f64) -> Record {
    assert_eq!(n0.len(), birth_baseline.len());
    assert_eq!(birth_baseline.len(), death_baseline.len());
    let n_len = n0.len();
    let mut rng = SmallRng::seed_from_u64(20240506);
    // let n0: Vec<u32> = n0.iter().map(|&x| x.try_into().unwrap()).collect_vec();
    let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");

    // initialize state
    let mut n: Vec<u32> = Vec::with_capacity(n_len);
    n.extend_from_slice(n0);
    assert_eq!(n.len(), n_len);
    let n = n.as_mut_slice();

    let mut record = Record::new(0);

    // TODO: initial time should be a result of Exp(1), as to have previous time...
    let mut t = 0.0;

    let mut current_year = 0;

    // record initial state
    record.add_initial_state(t, n0);
    loop {
        let delta_t: f64 = rng.sample(rand::distributions::Open01);
        // TODO: calculate birth and death rates according to the density...

        let propensity_birth =
            izip!(birth_baseline.iter(), n.iter()).map(|(&rate, &n)| rate * (n as f64));
        let propensity_death =
            izip!(death_baseline.iter(), n.iter()).map(|(&rate, &n)| rate * (n as f64));
        let propensity = propensity_birth.chain(propensity_death).collect_vec();

        let total_propensity: f64 = propensity.iter().sum1().unwrap();
        let delta_t = -delta_t.ln() / total_propensity;
        assert!(delta_t.is_finite());
        t += delta_t;

        if t >= t_max {
            break;
        }

        if current_year > t as i32 {
            println!("just passed {current_year}");
            dbg!(t, &n, total_propensity, &record);
            current_year += 1;
        }

        // next event
        let which_event = rand::distributions::WeightedIndex::new(propensity).unwrap();
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
            break;
        }
    }
    // TODO: add an event at `t_max` that is just the repeat of last staet

    // birth_baseline.iter().sum::<f64>() +
    // death_baseline.iter().sum::<f64>()

    // n0.iter().map(|x| *x as f64).sum1().unwrap();
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

extendr_module! {
    mod simulation;
    fn sim_bdm;
    // fn sim_multiple_bdm;
}

/// Checks if all elements of `integer` are non-negative, thus returns a transmuted unsigned integer slice back.
fn as_u32(integer: &[i32]) -> Option<&[u32]> {
    let any_negative = integer.iter().any(|x| x.is_negative());
    if any_negative {
        return None;
    }
    Some(unsafe { std::mem::transmute(integer) })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {
        let n0 = [10, 0, 0, 1];
        let birth_baseline = [4., 4., 4., 4.];
        let death_baseline = [1., 1., 1., 1.];
        let t_max = 25.;
        let t_max = 5.;

        let record = sim_bdm(&n0, &birth_baseline, &death_baseline, t_max);
        // dbg!(record);
    }
}
