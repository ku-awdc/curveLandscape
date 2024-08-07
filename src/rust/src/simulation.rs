use extendr_api::{prelude::*, IntoRobj};
use itertools::Itertools;
use itertools::{izip, repeat_n};
use rand::distributions::WeightedIndex;
use rand::{rngs::SmallRng, Rng, SeedableRng};

#[derive(Debug, IntoRobj)]
pub(crate) struct Record {
    pub(crate) repetition: usize,
    pub(crate) time: Vec<f64>,
    pub(crate) id_patch: Vec<usize>,
    pub(crate) count: Vec<u32>,
}

impl Record {
    pub fn new(repetition: usize) -> Self {
        let time = Vec::new();
        let count = Vec::new();
        let id_patch = Vec::new();
        Self {
            repetition,
            time,
            count,
            id_patch,
        }
    }

    pub fn add_initial_state(&mut self, time: f64, n0: &[u32]) {
        // check if the records are empty before inserting initial states..
        assert!(
            self.time.is_empty() & self.id_patch.is_empty() & self.count.is_empty(),
            "cannot insert default in already used record"
        );
        self.time.extend(repeat_n(time, n0.len()));
        self.count.extend_from_slice(n0);
        self.id_patch.extend(0..n0.len());
    }
}

/// Simulates a birth and death process that is density-dependent
///
///
#[extendr]
fn sim_bd_only(
    n0: &[i32],
    birth_baseline: &[f64],
    death_baseline: &[f64],
    carrying_capacity: &[i32],
    t_max: f64,
) -> Record {
    assert_eq!(n0.len(), birth_baseline.len());
    assert_eq!(birth_baseline.len(), death_baseline.len());
    assert_eq!(death_baseline.len(), carrying_capacity.len());

    // extensive output list
    let mut record = Record::new(0);

    // FIXME: what's a better behavior pattern?
    // if n0 is all zero to begin with or carrying capacity is zero (thus propensity would be fully zero)
    if n0.iter().all(|n| *n == 0) || carrying_capacity.iter().all(|x| *x == 0) {
        return record;
    }

    let n_len = n0.len();
    // let mut rng = SmallRng::seed_from_u64(20240805);
    let mut rng = SmallRng::from_entropy();
    let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
    let cc =
        as_u32(carrying_capacity).expect("`carrying_capacity` must be all non-negative integers");
    let cc_double = cc.iter().map(|x| -> f64 { *x as _ }).collect_vec();

    // initialize state
    let mut n: Vec<u32> = Vec::with_capacity(n_len);
    n.extend_from_slice(n0);
    assert_eq!(n.len(), n_len);
    let n = n.as_mut_slice();

    let mut birth_rate = Vec::from(birth_baseline);
    let birth_rate = birth_rate.as_mut_slice();
    let mut death_rate = Vec::from(death_baseline);
    let death_rate = death_rate.as_mut_slice();
    // in propensity [(birth rate + death rate) * count, ...]
    let mut propensity = Vec::from(death_baseline);
    let propensity = propensity.as_mut_slice();
    let mut total_propensity = 0.;

    izip!(
        n.iter(),
        cc_double.iter(),
        birth_baseline.iter(),
        death_baseline.iter(),
        birth_rate.iter_mut(),
        death_rate.iter_mut(),
        propensity.iter_mut(),
    )
    .for_each(|(&n, &cc, &beta0, &mu0, beta, mu, prop)| {
        // birth-rate
        let n_double = n as f64;
        if n_double > cc {
            *mu = mu0 + (n_double - cc) * (beta0 - mu0) / cc;
            *beta = mu0
        } else {
            *mu = mu0;
            *beta = mu0 + (cc - n_double) * (beta0 - mu0) / cc
        };
        // TODO: debug assert if rates are positive..
        *prop = (*beta + *mu) * n_double;
        total_propensity += *prop;
    });
    let mut which_patch_sampler = WeightedIndex::new(propensity.as_ref()).unwrap();

    // TODO: initial time should be a result of Exp(1), as to have previous time...
    let mut current_t = 0.0;

    // record initial state
    record.add_initial_state(current_t, n0);
    'simulation_loop: loop {
        let delta_t: f64 = rng.sample(rand::distributions::Open01);
        let delta_t = -delta_t.ln() / total_propensity;
        assert!(delta_t.is_finite());
        current_t += delta_t;

        if current_t >= t_max {
            break;
        }

        // which patch experienced a (birth|death) event?
        let patch_id = rng.sample(&which_patch_sampler);

        // Did birth or death happen?
        // probability of birth happening
        let is_birth =
            rng.gen_bool(birth_rate[patch_id] / (birth_rate[patch_id] + death_rate[patch_id]));

        // println!("t = {t}");
        if is_birth {
            // birth
            n[patch_id] += 1;
        } else {
            // death
            debug_assert_ne!(n[patch_id], 0);
            n[patch_id] -= 1;
        }
        record.time.push(current_t);
        record.id_patch.push(patch_id);
        record.count.push(n[patch_id]);
        // Terminate due to extinction
        if n.iter().all(|&x| x == 0) {
            break 'simulation_loop;
        }

        // remove old propensity, but we don't have the new yet
        total_propensity -= propensity[patch_id];

        // update birth/death rate for the changed patch..
        let growth_baseline = birth_baseline[patch_id] - death_baseline[patch_id];
        let g_div_N = (growth_baseline as f64) / (cc_double[patch_id]);
        if n[patch_id] > cc[patch_id] {
            birth_rate[patch_id] = death_baseline[patch_id];
            death_rate[patch_id] =
                death_baseline[patch_id] + g_div_N * (n[patch_id] as f64 - cc_double[patch_id]);
        } else {
            birth_rate[patch_id] =
                death_baseline[patch_id] + g_div_N * (cc_double[patch_id] - n[patch_id] as f64);
            death_rate[patch_id] = death_baseline[patch_id];
        }

        // now we can update the new patch propensity
        propensity[patch_id] = (birth_rate[patch_id] + death_rate[patch_id]) * n[patch_id] as f64;
        // and total propensity can be updated now
        total_propensity += propensity[patch_id];

        // update which_patch_sampler, since it contains `propensities` as weights
        which_patch_sampler
            .update_weights(&[(patch_id, &propensity[patch_id])])
            .unwrap();
    }

    // FIXME: check if the last simulated event was exactly at `t_max`

    // add an event at `t_max` that is just the repeat of last state
    for (patch_id, last_n) in n.into_iter().enumerate() {
        record.time.push(t_max);
        record.id_patch.push(patch_id);
        record.count.push(*last_n);
    }

    record
}

#[extendr]
fn sim_bd_only_many() {
    todo!()
}

/// Simulates birth, death and migration process of a multi-patch system.
///
/// @param migration_baseline Double that is m_0 in the formulas and is normalised
/// by (n-1) internally.
///
/// @details
///
/// The migration-baseline must be normalised
///
///
///
/// It is assumed that birth-rate exceeds death-rate, i.e. beta >= mu
///
///
#[extendr]
fn sim_bdm(
    n0: &[i32],
    birth_baseline: &[f64],
    death_baseline: &[f64],
    carrying_capacity: &[i32],
    migration_baseline: f64,
    t_max: f64,
) -> Record {
    assert!(n0.len() > 0);
    assert_eq!(n0.len(), birth_baseline.len());
    assert_eq!(birth_baseline.len(), death_baseline.len());
    assert_eq!(death_baseline.len(), carrying_capacity.len());

    assert!(migration_baseline.is_sign_positive());

    // extensive output list
    let mut record = Record::new(0);

    // FIXME: what's a better behavior pattern?
    // if n0 is all zero to begin with or carrying capacity is zero (thus propensity would be fully zero)
    if n0.iter().all(|n| *n == 0) || carrying_capacity.iter().all(|x| *x == 0) {
        return record;
    }

    let n_len = n0.len();
    // let mut rng = SmallRng::seed_from_u64(20240805);
    let mut rng = SmallRng::from_entropy();
    let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
    let cc =
        as_u32(carrying_capacity).expect("`carrying_capacity` must be all non-negative integers");
    let cc_double = cc.iter().map(|x| -> f64 { *x as _ }).collect_vec();

    // migration baseline must be normalised based on number of patches
    let migration_baseline = if n_len == 1 {
        // no migration can happen, set to zero.
        0.
    } else {
        migration_baseline / (n_len - 1) as f64
    };
    debug_assert!(migration_baseline.is_finite());

    // initialize state
    let mut n: Vec<u32> = Vec::with_capacity(n_len);
    n.extend_from_slice(n0);
    assert_eq!(n.len(), n_len);
    let n = n.as_mut_slice();

    let mut birth_rate = Vec::from(birth_baseline);
    let birth_rate = birth_rate.as_mut_slice();
    birth_rate.fill(0.);

    let mut death_rate = Vec::from(death_baseline);
    let death_rate = death_rate.as_mut_slice();
    death_rate.fill(0.);

    let mut migration_rate = Vec::from(death_baseline);
    let migration_rate = migration_rate.as_mut_slice();
    migration_rate.fill(0.);

    // in propensity [(birth rate + death rate + migration_rate(source)) * count, ...]
    let mut propensity = Vec::from(death_baseline);
    let propensity = propensity.as_mut_slice();

    let mut g_div_cc_baseline = Vec::from(death_baseline);
    let g_div_cc_baseline = g_div_cc_baseline.as_mut_slice();
    g_div_cc_baseline.fill(0.);

    let mut total_propensity = 0.;

    izip!(
        n.iter(),
        cc_double.iter(),
        birth_baseline.iter(),
        death_baseline.iter(),
        birth_rate.iter_mut(),
        death_rate.iter_mut(),
        migration_rate.iter_mut(),
        propensity.iter_mut(),
        g_div_cc_baseline.iter_mut(),
    )
    .for_each(|(&n, &cc, &beta0, &mu0, beta, mu, mig, prop, g_div_cc)| {
        // birth-rate / death-rate
        let n = n as f64;
        if n > cc {
            *mu = mu0 + ((n - cc) * (beta0 - mu0)) / cc;
            *beta = mu0
        } else {
            *mu = mu0;
            *beta = mu0 + ((cc - n) * (beta0 - mu0)) / cc
        };

        // migration rate
        // APPROACH: wedge
        *mig = ((n - (cc - 1.)).max(0.) * migration_baseline) / cc;

        // TODO: APPROACH: smooth (untested)
        // let log2: f64 = 1_f64.ln();
        // *mig = ((n - cc).exp().ln_1p() * migration_baseline) / (cc * log2);

        // TODO: debug assert if rates are positive..
        *prop = (*beta + *mu + *mig) * n;
        // dbg!(*mig);
        total_propensity += *prop;

        *g_div_cc = (beta0 - mu0) / cc;
    });
    // rprintln!("{:#?}", propensity);
    let mut which_patch_sampler = WeightedIndex::new(propensity.as_ref()).unwrap();

    // TODO: initial time should be a result of Exp(1), as to have previous time...
    let mut current_t = 0.0;

    // record initial state
    record.add_initial_state(current_t, n0);
    'simulation_loop: loop {
        let delta_t: f64 = rng.sample(rand::distributions::Open01);
        let delta_t = -delta_t.ln() / total_propensity;
        assert!(delta_t.is_finite());
        current_t += delta_t;

        if current_t >= t_max {
            break;
        }

        // which patch experienced a (birth|death|migration) event?
        let patch_id = rng.sample(&which_patch_sampler);

        // Did (birth|death|migration) happen?
        let flag_bdm = rng.sample(
            WeightedIndex::new([
                birth_rate[patch_id],
                death_rate[patch_id],
                migration_rate[patch_id],
            ])
            .unwrap(),
        );

        let mut target_patch_id = patch_id; // sensible? default?
        match flag_bdm {
            // birth
            0 => {
                n[patch_id] += 1;

                // note: this bit is copied three times, and could be moved out of here,
                // but for clarity, it is copied..
                record.time.push(current_t);
                record.id_patch.push(patch_id);
                record.count.push(n[patch_id]);
            }
            // death
            1 => {
                debug_assert_ne!(n[patch_id], 0);
                n[patch_id] -= 1;

                record.time.push(current_t);
                record.id_patch.push(patch_id);
                record.count.push(n[patch_id]);
            }
            // migration
            2 => {
                debug_assert_ne!(n[patch_id], 0);
                n[patch_id] -= 1;

                record.time.push(current_t);
                record.id_patch.push(patch_id);
                record.count.push(n[patch_id]);

                // determine the target patch_id (exclude current as an option)
                target_patch_id = rng.gen_range(0..n_len - 1); // [0,..., n-2]
                target_patch_id = if target_patch_id < patch_id {
                    target_patch_id
                } else {
                    // don't return `patch_id`, but everything above it
                    target_patch_id + 1
                };

                n[target_patch_id] += 1;

                record.time.push(current_t);
                record.id_patch.push(target_patch_id);
                record.count.push(n[target_patch_id]);
            }
            _ => unreachable!(),
        };
        let target_patch_id = target_patch_id;

        // Terminate due to extinction
        if n.iter().all(|&x| x == 0) {
            break 'simulation_loop;
        }

        // remove old propensity, but we don't have the new yet
        total_propensity -= propensity[patch_id];

        // update birth/death rate for the changed patch..
        let g_div_cc = g_div_cc_baseline[patch_id];
        if n[patch_id] > cc[patch_id] {
            birth_rate[patch_id] = death_baseline[patch_id];
            death_rate[patch_id] =
                death_baseline[patch_id] + g_div_cc * (n[patch_id] as f64 - cc_double[patch_id]);
        } else {
            birth_rate[patch_id] =
                death_baseline[patch_id] + g_div_cc * (cc_double[patch_id] - n[patch_id] as f64);
            death_rate[patch_id] = death_baseline[patch_id];
        }

        // migration rate
        // APPROACH: wedge
        migration_rate[patch_id] = ((n[patch_id] as f64 - (cc_double[patch_id] - 1.)).max(0.)
            * migration_baseline)
            / cc_double[patch_id];

        // TODO: APPROACH: smooth (untested)
        // let log2: f64 = 1_f64.ln();
        // migration_rate[patch_id] = ((n[patch_id] as f64 - cc_double[patch_id]).exp().ln_1p() * migration_baseline) / (cc_double[patch_id] * log2);

        // now we can update the new patch propensity
        propensity[patch_id] =
            (birth_rate[patch_id] + death_rate[patch_id] + migration_rate[patch_id])
                * n[patch_id] as f64;
        // and total propensity can be updated now
        total_propensity += propensity[patch_id];

        if flag_bdm == 2 {
            // migration happened, so...

            // region: update everything for `target_patch_id` as well

            // remove old propensity, but we don't have the new yet
            total_propensity -= propensity[target_patch_id];

            // update birth/death rate for the changed (target) patch..
            let g_div_cc = g_div_cc_baseline[target_patch_id];
            if n[target_patch_id] > cc[target_patch_id] {
                birth_rate[target_patch_id] = death_baseline[target_patch_id];
                death_rate[target_patch_id] = death_baseline[target_patch_id]
                    + g_div_cc * (n[target_patch_id] as f64 - cc_double[target_patch_id]);
            } else {
                birth_rate[target_patch_id] = death_baseline[target_patch_id]
                    + g_div_cc * (cc_double[target_patch_id] - n[target_patch_id] as f64);
                death_rate[target_patch_id] = death_baseline[target_patch_id];
            }

            // migration rate
            // APPROACH: wedge
            migration_rate[target_patch_id] =
                ((n[target_patch_id] as f64 - (cc_double[target_patch_id] - 1.)).max(0.)
                    * migration_baseline)
                    / cc_double[target_patch_id];

            // TODO: APPROACH: smooth (untested)
            // let log2: f64 = 1_f64.ln();
            // migration_rate[target_patch_id] = ((n[target_patch_id] as f64 - cc_double[target_patch_id]).exp().ln_1p() * migration_baseline) / (cc_double[target_patch_id] * log2);

            // now we can update the new patch propensity
            propensity[target_patch_id] = (birth_rate[target_patch_id]
                + death_rate[target_patch_id]
                + migration_rate[target_patch_id])
                * n[target_patch_id] as f64;
            // and total propensity can be updated now
            total_propensity += propensity[target_patch_id];
            // endregion

            // dbg!(&[
            //     (patch_id, &propensity[patch_id]),
            //     (target_patch_id, &propensity[target_patch_id]),
            // ]);
            // update which_patch_sampler, since it contains `propensities` as weights
            which_patch_sampler
                .update_weights(
                    // the weights most be updated sequentially..
                    if patch_id <= target_patch_id {
                        [
                            (patch_id, &propensity[patch_id]),
                            (target_patch_id, &propensity[target_patch_id]),
                        ]
                    } else {
                        [
                            (target_patch_id, &propensity[target_patch_id]),
                            (patch_id, &propensity[patch_id]),
                        ]
                    }
                    .as_slice(),
                )
                .unwrap();
        } else {
            // migration didn't happen...
            // update which_patch_sampler, since it contains `propensities` as weights
            which_patch_sampler
                .update_weights(&[(patch_id, &propensity[patch_id])])
                .unwrap();
        }
    }

    // FIXME: check if the last simulated event was exactly at `t_max`

    // add an event at `t_max` that is just the repeat of last state
    for (patch_id, last_n) in n.into_iter().enumerate() {
        record.time.push(t_max);
        record.id_patch.push(patch_id);
        record.count.push(*last_n);
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

/// Checks if all elements of `integer` are non-negative, thus returns a transmuted unsigned integer slice back.
pub(crate) fn as_u32(integer: &[i32]) -> Option<&[u32]> {
    let any_negative = integer.iter().any(|x| x.is_negative());
    if any_negative {
        return None;
    }
    Some(unsafe { std::mem::transmute(integer) })
}

extendr_module! {
    mod simulation;
    // Gillespie approach
    fn sim_bd_only;
    fn sim_bd_only_many;
    fn sim_bdm;
    // fn sim_migration_only;
    // fn sim_multiple_bdm;

    // ODE approach
    // fn update_migration_only;
    // fn update_birth_death_and_migration;

}

#[cfg(test)]
mod tests {
    use extendr_engine::with_r;

    use super::*;

    #[test]
    fn test_indexing() {
        with_r(|| {});
    }

    // #[test]
    // fn test_sim_migration_only() {
    //     let n0 = [10, 2];
    //     let migration_baseline = [0.1, 0.0001];
    //     let carrying_capacity = [5, 5];
    //     let k_dij = [1.];
    //     let t_max = 100000.;

    //     sim_migration_only(&n0, &migration_baseline, &carrying_capacity, &k_dij, t_max);
    // }

    #[test]
    fn test_weighted_alias_update_twice() {
        // can I update the same weight twice?
    }

    #[test]
    fn test_sim_bdm() {
        let n0 = [2];
        let birth_baseline = [4.];
        let death_baseline = [1.];
        let carrying_capacity = [4];
        let migration_baseline = 0.1;
        let t_max = 10.;
        sim_bdm(
            n0.as_slice(),
            birth_baseline.as_slice(),
            death_baseline.as_slice(),
            carrying_capacity.as_slice(),
            migration_baseline,
            t_max,
        );
    }
}
