use derive_more::{AsMut, AsRef, Deref, DerefMut};
use extendr_api::{prelude::*, IntoRobj};
use itertools::Itertools;
use itertools::{izip, repeat_n};
use rand::distributions::WeightedIndex;
use rand::{rngs::SmallRng, Rng, SeedableRng};

/// Contains all the changes that occurred in the population on a patch and time level.
///
/// For a record that maintains total population, see [`PopulationRecord`].
#[derive(Debug, IntoRobj)]
pub(crate) struct PatchRecord {
    pub(crate) repetition: usize,
    pub(crate) time: Vec<f64>,
    pub(crate) id_patch: Vec<usize>,
    pub(crate) count: Vec<u32>,
}

impl PatchRecord {
    #[inline(always)]
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

    #[inline(always)]
    pub fn push_initial_state(&mut self, time: f64, n0: &[u32]) {
        // check if the records are empty before inserting initial states..
        assert!(
            self.time.is_empty() & self.id_patch.is_empty() & self.count.is_empty(),
            "cannot insert default in already used record"
        );
        self.time.extend(repeat_n(time, n0.len()));
        self.count.extend_from_slice(n0);
        self.id_patch.extend(0..n0.len());
    }

    #[inline(always)]
    pub fn push(&mut self, time: f64, patch_id: usize, n: u32) {
        self.time.push(time);
        self.id_patch.push(patch_id);
        self.count.push(n);
    }
}

/// Contains the total population count for all changes in the simulation.
///
/// For a pr. patch resolution recorder, see [`PatchRecorder`].
#[derive(Debug, IntoRobj)]
pub(crate) struct PopulationRecord {
    pub(crate) repetition: usize,
    pub(crate) time: Vec<f64>,
    pub(crate) count: Vec<u32>,
}

impl PopulationRecord {
    pub fn new(repetition: usize) -> Self {
        let time = Vec::new();
        let count = Vec::new();
        Self {
            repetition,
            time,
            count,
        }
    }

    pub fn push_initial_state(&mut self, time: f64, total_n0: u32) {
        // check if the records are empty before inserting initial states..
        assert!(
            self.time.is_empty() & self.count.is_empty(),
            "cannot insert default in already used record"
        );
        self.time.push(time);
        self.count.push(total_n0);
    }

    #[inline(always)]
    pub fn push(&mut self, time: f64, n: u32) {
        self.time.push(time);
        self.count.push(n);
    }
}

pub(crate) trait Recorder {
    fn push(&mut self, time: f64, patch_id: usize, n: u32);

    fn add_initial_state(&mut self, time: f64, n: &[u32]);
    fn record_birth(&mut self, time: f64, patch_id: usize, n_patch: u32);
    fn record_death(&mut self, time: f64, patch_id: usize, n_patch: u32);
    fn record_migration(
        &mut self,
        time: f64,
        source_patch_id: usize,
        target_patch_id: usize,
        source_n: u32,
        target_n: u32,
    );

    fn add_final_state(&mut self, t_max: f64, n: &[u32]);
}

impl Recorder for PatchRecord {
    fn push(&mut self, time: f64, patch_id: usize, n: u32) {
        self.push(time, patch_id, n);
    }

    fn add_initial_state(&mut self, time: f64, n: &[u32]) {
        // FIXME: check if it is already present in the records!
        // check if the records are empty before inserting initial states..
        assert!(
            self.time.is_empty() & self.id_patch.is_empty() & self.count.is_empty(),
            "cannot insert default in already used record"
        );
        self.time.extend(repeat_n(time, n.len()));
        self.count.extend_from_slice(n);
        self.id_patch.extend(0..n.len());
    }

    #[inline(always)]
    fn record_birth(&mut self, time: f64, patch_id: usize, n_patch: u32) {
        self.push(time, patch_id, n_patch);
    }

    #[inline(always)]
    fn record_death(&mut self, time: f64, patch_id: usize, n_patch: u32) {
        self.push(time, patch_id, n_patch);
    }

    #[inline(always)]
    fn record_migration(
        &mut self,
        time: f64,
        source_patch_id: usize,
        target_patch_id: usize,
        source_n: u32,
        target_n: u32,
    ) {
        self.push(time, source_patch_id, source_n);
        self.push(time, target_patch_id, target_n);
    }

    fn add_final_state(&mut self, t_max: f64, n: &[u32]) {
        for (patch_id, &last_n) in n.into_iter().enumerate() {
            self.push(t_max, patch_id, last_n);
        }
    }
}

impl Recorder for PopulationRecord {
    fn push(&mut self, time: f64, _patch_id: usize, total_n: u32) {
        self.push(time, total_n);
    }

    fn add_initial_state(&mut self, time: f64, n: &[u32]) {
        // FIXME: check if it is already present
        let total_n0 = n.iter().sum();
        self.push_initial_state(time, total_n0);
    }

    /// Records the change in the total population due to one birth.
    ///
    /// # Panics
    /// Assumes that initial states were recorded.
    #[inline(always)]
    fn record_birth(&mut self, time: f64, _patch_id: usize, _n: u32) {
        self.push(time, *self.count.last().unwrap() + 1);
    }

    /// Records the change in the total population due to one death.
    ///
    /// # Panics
    /// Assumes that initial states were recorded.
    #[inline(always)]
    fn record_death(&mut self, time: f64, _patch_id: usize, _n: u32) {
        self.push(time, *self.count.last().unwrap() - 1);
    }

    /// Does not record anything, as migration should not affect population count
    #[inline(always)]
    fn record_migration(
        &mut self,
        _time: f64,
        _source_patch_id: usize,
        _target_patch_id: usize,
        _source_n: u32,
        _target_n: u32,
    ) {
        // do nothing, because migration shouldn't affect population count
    }

    fn add_final_state(&mut self, t_max: f64, _n: &[u32]) {
        self.push(t_max, self.count.last().unwrap().clone());
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
) -> PatchRecord {
    assert_eq!(n0.len(), birth_baseline.len());
    assert_eq!(birth_baseline.len(), death_baseline.len());
    assert_eq!(death_baseline.len(), carrying_capacity.len());

    // extensive output list
    let mut record = PatchRecord::new(0);

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
    record.push_initial_state(current_t, n0);
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
    carrying_capacity: &[f64],
    migration_baseline: f64,
    t_max: f64,
) -> PatchRecord {
    assert!(n0.len() > 0);
    assert_eq!(n0.len(), birth_baseline.len());
    assert_eq!(birth_baseline.len(), death_baseline.len());
    assert_eq!(death_baseline.len(), carrying_capacity.len());

    assert!(migration_baseline.is_sign_positive());

    // extensive output list
    let mut record = PatchRecord::new(0);

    // FIXME: what's a better behaviour pattern?
    // if n0 is all zero to begin with or carrying capacity is zero (thus propensity would be fully zero)
    if n0.iter().all(|n| *n == 0) || carrying_capacity.iter().all(|x| x.abs() <= 0.0001) {
        return record;
    }

    let n_len = n0.len();
    // let mut rng = SmallRng::seed_from_u64(20240805);
    let mut rng = SmallRng::from_entropy();
    let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
    let cc = carrying_capacity;

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
        cc.iter(),
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
    record.push_initial_state(current_t, n0);
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
        if n[patch_id] as f64 > cc[patch_id] {
            birth_rate[patch_id] = death_baseline[patch_id];
            death_rate[patch_id] =
                death_baseline[patch_id] + g_div_cc * (n[patch_id] as f64 - cc[patch_id]);
        } else {
            birth_rate[patch_id] =
                death_baseline[patch_id] + g_div_cc * (cc[patch_id] - n[patch_id] as f64);
            death_rate[patch_id] = death_baseline[patch_id];
        }

        // migration rate
        // APPROACH: wedge
        migration_rate[patch_id] = ((n[patch_id] as f64 - (cc[patch_id] - 1.)).max(0.)
            * migration_baseline)
            / cc[patch_id];

        // TODO: APPROACH: smooth (untested)
        // let log2: f64 = 1_f64.ln();
        // migration_rate[patch_id] = ((n[patch_id] as f64 - cc[patch_id]).exp().ln_1p() * migration_baseline) / (cc[patch_id] * log2);

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
            if n[target_patch_id] as f64 > cc[target_patch_id] {
                birth_rate[target_patch_id] = death_baseline[target_patch_id];
                death_rate[target_patch_id] = death_baseline[target_patch_id]
                    + g_div_cc * (n[target_patch_id] as f64 - cc[target_patch_id]);
            } else {
                birth_rate[target_patch_id] = death_baseline[target_patch_id]
                    + g_div_cc * (cc[target_patch_id] - n[target_patch_id] as f64);
                death_rate[target_patch_id] = death_baseline[target_patch_id];
            }

            // migration rate
            // APPROACH: wedge
            migration_rate[target_patch_id] =
                ((n[target_patch_id] as f64 - (cc[target_patch_id] - 1.)).max(0.)
                    * migration_baseline)
                    / cc[target_patch_id];

            // TODO: APPROACH: smooth (untested)
            // let log2: f64 = 1_f64.ln();
            // migration_rate[target_patch_id] = ((n[target_patch_id] as f64 - cc[target_patch_id]).exp().ln_1p() * migration_baseline) / (cc[target_patch_id] * log2);

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
    patch_records: Vec<PatchRecord>,
}

#[derive(Debug)]
struct WildSSAConfiguration {
    n0: Box<[u32]>,
    n_len: usize,
    birth_baseline: Box<[f64]>,
    death_baseline: Box<[f64]>,
    carrying_capacity: Box<[f64]>,
    migration_baseline: f64,
}

#[derive(Debug, Clone)]
struct WildSSAInternalState {
    repetition: u32,
    // region: model state
    n: Box<[u32]>,
    birth_rate: Box<[f64]>,
    death_rate: Box<[f64]>,
    migration_rate: Box<[f64]>,
    // endregion
    g_div_cc_baseline: Box<[f64]>,
    // region: SSA algorithm internal state
    propensity: Box<[f64]>,
    total_propensity: f64,
    // endregion
    current_time: f64,
    which_patch_sampler: WeightedIndex<f64>,
    rng: SmallRng,
}

#[derive(Debug, Deref, DerefMut, AsRef, AsMut)]
struct WildSSA {
    #[as_ref]
    #[as_mut]
    #[deref]
    #[deref_mut]
    internal_state: WildSSAInternalState,
    // read-only
    configuration: WildSSAConfiguration,
    /// (internal field) save the internal state for resetting
    initial_internal_state: Option<WildSSAInternalState>,
}

impl WildSSA {
    pub fn new(
        n0: &[i32],
        birth_baseline: &[f64],
        death_baseline: &[f64],
        carrying_capacity: &[f64],
        migration_baseline: f64,
        seed: u64,
    ) -> Self {
        assert!(!n0.is_empty());
        let n_len = n0.len();
        assert_eq!(n_len, birth_baseline.len());
        assert_eq!(birth_baseline.len(), death_baseline.len());
        assert_eq!(death_baseline.len(), carrying_capacity.len());

        assert!(migration_baseline.is_sign_positive());

        // FIXME: what's a better behaviour pattern?
        // if n0 is all zero to begin with or carrying capacity is zero (thus propensity would be fully zero)
        if n0.iter().all(|n| *n == 0) || carrying_capacity.iter().all(|x| x.abs() <= 0.0001) {
            panic!("all are initialised to 0 or carrying capacity is all 0");
        }

        let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
        let n0: Box<[_]> = n0.into();

        // migration baseline must be normalised based on number of patches
        let migration_baseline = if n_len == 1 {
            // no migration can happen, set to zero.
            0.
        } else {
            migration_baseline / (n_len - 1) as f64
        };
        debug_assert!(migration_baseline.is_finite());

        // initialize state
        let n = n0.clone();
        let birth_baseline: Box<[_]> = birth_baseline.into();
        let death_baseline: Box<[_]> = death_baseline.into();
        let carrying_capacity: Box<[_]> = carrying_capacity.into();
        let migration_baseline = migration_baseline.into();

        let mut birth_rate = birth_baseline.clone();
        birth_rate.fill(0.);

        let mut death_rate = death_baseline.clone();
        death_rate.fill(0.);

        let migration_rate = death_rate.clone();
        // `death_rate` is already filled with 0_f64

        // in propensity [(birth rate + death rate + migration_rate(source)) * count, ...]
        let propensity = birth_rate.clone();

        let g_div_cc_baseline = death_baseline.clone();
        // `death_baseline` is already filled with 0_f64

        let total_propensity = 0.;
        // this is invalid, as the `propensity` is identically 0.
        let which_patch_sampler = WeightedIndex::new(propensity.clone()).unwrap();

        let configuration = WildSSAConfiguration {
            n0,
            carrying_capacity,
            birth_baseline,
            death_baseline,
            migration_baseline,
            n_len,
        };
        let internal_state = WildSSAInternalState {
            n,
            birth_rate,
            death_rate,
            migration_rate,
            propensity,
            total_propensity,
            g_div_cc_baseline,
            current_time: 0.,
            which_patch_sampler,
            repetition: 0,
            rng: SmallRng::seed_from_u64(seed),
        };
        let initial_internal_state = None;
        let mut uninitialised_self = Self {
            configuration,
            internal_state,
            initial_internal_state,
        };
        uninitialised_self.reset();
        let initialised_self = uninitialised_self;
        initialised_self
    }

    /// Resets the internal state of the simulation.
    ///
    /// # Note
    ///
    /// Does not alter the cached Random Number generator. Instead, it reuses it for later repetitions.
    ///
    pub fn reset(&mut self) {
        // are initial internals cached?
        if self.initial_internal_state.is_some() {
            self.internal_state = self.initial_internal_state.as_ref().unwrap().clone();
            return;
        }
        let WildSSAConfiguration {
            n0: _,
            n_len: _,
            ref birth_baseline,
            ref death_baseline,
            ref carrying_capacity,
            ref migration_baseline,
        } = &self.configuration;
        // initialise the internals...
        let WildSSAInternalState {
            ref mut repetition,
            ref n,
            ref mut birth_rate,
            ref mut death_rate,
            ref mut migration_rate,
            ref mut g_div_cc_baseline,
            ref mut propensity,
            ref mut total_propensity,
            current_time: _,
            which_patch_sampler: _,
            // don't reset the `rng`..
            rng: _,
        } = &mut self.internal_state;
        *repetition += 1;
        izip!(
            n.iter(),
            carrying_capacity.iter(),
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
            // TODO: replace with `f_birth_death`
            if n > cc {
                *mu = mu0 + ((n - cc) * (beta0 - mu0)) / cc;
                *beta = mu0
            } else {
                *mu = mu0;
                *beta = mu0 + ((cc - n) * (beta0 - mu0)) / cc
            };

            // migration rate
            // APPROACH: wedge
            // TODO: replace with f_migration_wedge
            *mig = ((n - (cc - 1.)).max(0.) * migration_baseline) / cc;

            // TODO: APPROACH: smooth (untested)
            // TODO: replace with `f_migration_smooth`
            // let log2: f64 = 1_f64.ln();
            // *mig = ((n - cc).exp().ln_1p() * migration_baseline) / (cc * log2);

            // TODO: debug assert if rates are positive..
            *prop = (*beta + *mu + *mig) * n;
            // dbg!(*mig);
            *total_propensity += *prop;

            *g_div_cc = (beta0 - mu0) / cc;
        });
        self.which_patch_sampler = WeightedIndex::new(propensity.as_ref()).unwrap();

        // TODO: initial time should be a result of Exp(1), as to have previous time...
        // let current_t = 0.0;

        // cache the internals for later
        let _ = self
            .initial_internal_state
            .insert(self.internal_state.clone());
    }

    fn run_until(mut self, t_max: f64, recorder: &mut impl Recorder) {
        assert!(self.current_time <= t_max);

        let WildSSAConfiguration {
            n0: _,
            n_len,
            birth_baseline,
            death_baseline,
            carrying_capacity,
            migration_baseline,
        } = self.configuration;
        let WildSSAInternalState {
            repetition: _,
            ref mut n,
            ref mut birth_rate,
            ref mut death_rate,
            ref mut migration_rate,
            ref mut g_div_cc_baseline,
            ref mut propensity,
            ref mut total_propensity,
            mut current_time,
            mut which_patch_sampler,
            ref mut rng,
        } = self.internal_state;
        // record initial state
        // FIXME: this will add another "record" if it is a re-run of an already half run sequence.
        recorder.add_initial_state(current_time, n.as_ref());
        'simulation_loop: loop {
            let delta_t: f64 = rng.sample(rand::distributions::Open01);
            let delta_t = -delta_t.ln() / *total_propensity;
            assert!(delta_t.is_finite());
            current_time += delta_t;

            if current_time >= t_max {
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
                    recorder.record_birth(current_time, patch_id, n[patch_id]);
                }
                // death
                1 => {
                    debug_assert_ne!(n[patch_id], 0);
                    n[patch_id] -= 1;

                    recorder.record_death(current_time, patch_id, n[patch_id]);
                }
                // migration
                2 => {
                    debug_assert_ne!(n[patch_id], 0);
                    n[patch_id] -= 1;

                    // determine the target patch_id (exclude current as an option)
                    target_patch_id = rng.gen_range(0..n_len - 1); // [0,..., n-2]
                    target_patch_id = if target_patch_id < patch_id {
                        target_patch_id
                    } else {
                        // don't return `patch_id`, but everything above it
                        target_patch_id + 1
                    };

                    n[target_patch_id] += 1;

                    recorder.record_migration(
                        current_time,
                        patch_id,
                        target_patch_id,
                        n[patch_id],
                        n[target_patch_id],
                    )
                }
                _ => unreachable!(),
            };
            let target_patch_id = target_patch_id;

            // Terminate due to extinction
            if n.iter().all(|&x| x == 0) {
                break 'simulation_loop;
            }

            // remove old propensity, but we don't have the new yet
            *total_propensity -= propensity[patch_id];

            // update birth/death rate for the changed patch..
            unsafe {
                f_birth_death_rate(
                    n[patch_id] as _,
                    birth_baseline[patch_id],
                    death_baseline[patch_id],
                    carrying_capacity[patch_id],
                    birth_rate.get_unchecked_mut(patch_id),
                    death_rate.get_unchecked_mut(patch_id),
                    g_div_cc_baseline.get_unchecked_mut(patch_id),
                );
            }

            // migration rate
            // APPROACH: wedge
            unsafe {
                f_migration_wedge(
                    n[patch_id] as _,
                    migration_baseline,
                    carrying_capacity[patch_id],
                    migration_rate.get_unchecked_mut(patch_id),
                );
            }

            // TODO: APPROACH: smooth (untested)
            // unsafe {
            //     f_migration_smooth(
            //         n[patch_id] as _,
            //         configuration.migration_baseline,
            //         configuration.carrying_capacity[patch_id],
            //         migration_rate.get_unchecked_mut(patch_id),
            //     );
            // }

            // now we can update the new patch propensity
            propensity[patch_id] =
                (birth_rate[patch_id] + death_rate[patch_id] + migration_rate[patch_id])
                    * n[patch_id] as f64;
            // and total propensity can be updated now
            *total_propensity += propensity[patch_id];

            if flag_bdm == 2 {
                // migration happened, so...

                // region: update everything for `target_patch_id` as well

                // remove old propensity, but we don't have the new yet
                *total_propensity -= propensity[target_patch_id];

                // update birth/death rate for the changed patch..
                unsafe {
                    f_birth_death_rate(
                        n[target_patch_id] as _,
                        birth_baseline[target_patch_id],
                        death_baseline[target_patch_id],
                        carrying_capacity[target_patch_id],
                        birth_rate.get_unchecked_mut(target_patch_id),
                        death_rate.get_unchecked_mut(target_patch_id),
                        g_div_cc_baseline.get_unchecked_mut(target_patch_id),
                    );
                }

                // migration rate
                // APPROACH: wedge
                unsafe {
                    f_migration_wedge(
                        n[target_patch_id] as _,
                        migration_baseline,
                        carrying_capacity[target_patch_id],
                        migration_rate.get_unchecked_mut(target_patch_id),
                    );
                }

                // TODO: APPROACH: smooth (untested)
                // unsafe {
                //     f_migration_smooth(
                //         n[target_patch_id] as _,
                //         configuration.migration_baseline,
                //         configuration.carrying_capacity[target_patch_id],
                //         migration_rate.get_unchecked_mut(target_patch_id),
                //     );
                // }

                // now we can update the new patch propensity
                propensity[target_patch_id] = (birth_rate[target_patch_id]
                    + death_rate[target_patch_id]
                    + migration_rate[target_patch_id])
                    * n[target_patch_id] as f64;
                // and total propensity can be updated now
                *total_propensity += propensity[target_patch_id];
                // endregion

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
        recorder.add_final_state(t_max, n.as_ref());
    }
}

#[inline(always)]
fn f_birth_death_rate(
    n: f64,
    beta0: f64,
    mu0: f64,
    cc: f64,
    beta: &mut f64,
    mu: &mut f64,
    g_div_cc: &mut f64,
) {
    *g_div_cc = (beta0 - mu0) / cc;

    if n > cc {
        *mu = mu0 + ((n - cc) * (beta0 - mu0)) / cc;
        *beta = mu0
    } else {
        *mu = mu0;
        *beta = mu0 + ((cc - n) * (beta0 - mu0)) / cc
    };
}

#[inline(always)]
fn f_migration_wedge(n: f64, migration_baseline: f64, cc: f64, mig: &mut f64) {
    *mig = ((n - (cc - 1.)).max(0.) * migration_baseline) / cc;
}

#[inline(always)]
fn f_migration_smooth(n: f64, migration_baseline: f64, cc: f64, mig: &mut f64) {
    let log2: f64 = 1_f64.ln();
    *mig = ((n - cc).exp().ln_1p() * migration_baseline) / (cc * log2);
}

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
    fn wild_ssa_init() {
        let n0 = &[1];
        let birth_baseline = &[2.];
        let death_baseline = &[3.];
        let carrying_capacity = &[4.];
        let migration_baseline = 4.;

        let mut wild_ssa = WildSSA::new(
            n0,
            birth_baseline,
            death_baseline,
            carrying_capacity,
            migration_baseline,
            20202020,
        );

        dbg!(&wild_ssa);
        wild_ssa.death_rate[0] = 1000.;
        wild_ssa.propensity[0] = 11234.;
        dbg!(&wild_ssa);
    }

    // #[test]
    // fn test_indexing() {
    //     with_r(|| {});
    // }

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
        let carrying_capacity = [4.];
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
