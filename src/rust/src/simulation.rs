use derive_more::{AsMut, AsRef, Deref, DerefMut};
use extendr_api::{prelude::*, IntoRobj};
use itertools::repeat_n;
#[allow(unused_imports)]
use itertools::Itertools;
use rand::distributions::WeightedIndex;
use rand::{rngs::SmallRng, SeedableRng};
#[cfg(feature = "rayon")]
#[allow(unused_imports)]
use rayon::prelude::*;

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
        for (patch_id, &last_n) in n.iter().enumerate() {
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
        self.push(t_max, *self.count.last().unwrap());
    }
}

// TODO: Implement a recorder that is essentially `approx(method = "constant", f = 0)`
// from R. This one should just record the last used .

/// Records the total population at fixed time points.
///
/// Similar to `approx(method = "constant", f = 0)` in R.
///
#[derive(Debug, IntoRobj)]
pub(crate) struct PopulationFixedRecord {
    pub(crate) repetition: usize,
    /// Fixed time points that, where the total population is recorded at.
    pub(crate) time: Vec<f64>,
    pub(crate) count: Vec<u32>,

    // internal fields
    current_time: f64,
    current_count: u32,
    // this is like an iterator going through `time`,
    // and it is the current `time` and `count` entry we want to fill in.
    current_time_index: usize,
}

impl PopulationFixedRecord {
    pub fn new(repetition: usize, fixed_time_points: &[f64]) -> Self {
        // TODO: check that it is non-decreasing time points
        assert!(!fixed_time_points.is_empty());

        // these are the fixed time points
        let time = Vec::from(fixed_time_points);
        let count = Vec::with_capacity(fixed_time_points.len());

        let current_time = time[0]; // invalid state, must be rectified before using in algorithm
        let current_count = 0; // invalid state, must be rectified before using in algorithm
        let current_time_index = 0; // good! we need to record the first time point.
        Self {
            repetition,
            time,
            count,
            current_time,
            current_count,
            current_time_index,
        }
    }
}
impl Recorder for PopulationFixedRecord {
    fn push(&mut self, _time: f64, _patch_id: usize, _n: u32) {
        panic!("Doesn't make sense here...")
    }

    fn add_initial_state(&mut self, time: f64, n: &[u32]) {
        // assert!(
        //     (self.current_time - time).abs() <= 0.00001,
        //     "initial requested time must match the initial simulated time"
        // );
        assert!(
            self.time[self.current_time_index] >= time,
            "we didn't skip anything.."
        );
        let total_n0 = n.iter().sum();
        self.current_count = total_n0;
        self.current_time = time;
        // is this initial time of interest?
        loop {
            if self.time[self.current_time_index] <= time {
                // we crossed the time point we are interested in
                self.count.push(self.current_count);
                self.current_time_index += 1;
            }
            if self.time[self.current_time_index] > time {
                break;
            }
        }
    }

    fn record_birth(&mut self, time: f64, _patch_id: usize, _n_patch: u32) {
        self.current_time = time;
        // is this initial time of interest?
        loop {
            if self.time[self.current_time_index] <= time {
                // we crossed the time point we are interested in
                self.count.push(self.current_count);
                self.current_time_index += 1;
            }
            if self.time[self.current_time_index] > time {
                break;
            }
        }
        self.current_count += 1;
    }

    fn record_death(&mut self, time: f64, _patch_id: usize, _n_patch: u32) {
        self.current_time = time;
        // is this initial time of interest?
        loop {
            if self.time[self.current_time_index] <= time {
                // we crossed the time point we are interested in
                self.count.push(self.current_count);
                self.current_time_index += 1;
            }
            if self.time[self.current_time_index] > time {
                break;
            }
        }
        self.current_count -= 1;
    }

    fn record_migration(
        &mut self,
        time: f64,
        _source_patch_id: usize,
        _target_patch_id: usize,
        _source_n: u32,
        _target_n: u32,
    ) {
        self.current_time = time;
        // the population count doesn't change with a migration event,
        // but we might have crossed the time points that we are interested in
        loop {
            if self.time[self.current_time_index] <= time {
                // we crossed the time point we are interested in
                self.count.push(self.current_count);
                self.current_time_index += 1;
            }
            if self.time[self.current_time_index] > time {
                break;
            }
        }
    }

    fn add_final_state(&mut self, t_max: f64, _n: &[u32]) {
        // TODO: what is it that needs to be done here anyways?
        self.current_time = t_max;
        // self.current_count; // unchanged
        loop {
            if self.time[self.current_time_index] <= t_max {
                // we crossed the time point we are interested in
                self.count.push(self.current_count);
                self.current_time_index += 1;
            }
            if self.current_time_index >= self.time.len() {
                break;
            }
            if self.time[self.current_time_index] > t_max {
                break;
            }
        }
        // debug assert?
        assert!(t_max >= *self.time.last().unwrap());
        // sanity check: did we record everything we set out to do?
        // but also ensures the output is uniform.
        // dbg!(&self);
        assert_eq!(
            self.count.len(),
            self.time.len(),
            "not all fixed time points were recorded."
        );
    }
}

#[derive(Debug, Clone)]
struct WildSSAConfiguration {
    n0: Box<[u32]>,
    n_len: usize,
    birth_baseline: Box<[f64]>,
    death_baseline: Box<[f64]>,
    carrying_capacity: Box<[f64]>,
    migration_intercept: f64,
    migration_baseline: f64,
}

#[derive(Debug, Clone)]
struct WildSSAInternalState {
    // region: model state
    n: Box<[u32]>,
    // endregion
    // region: SSA algorithm internal state
    /// Matrix of emigration (and death in the diagonal)
    emigration_propensity: Box<[f64]>,
    /// Matrix of immigration (and birth in the diagonal)
    immigration_propensity: Box<[f64]>,
    total_emigration_propensity: f64,
    total_immigration_propensity: f64,
    total_propensity: f64,
    // endregion
    current_time: f64,
}

/// Simulates birth, death and migration process of a multi-patch system.
///
/// @param migration_baseline Double that is m_0 in the formulas and is normalised
/// by (n-1) internally.
///
/// Similar is done to `m_intercept`.
///
/// @details
///
/// The migration-baseline must be normalised
///
///
///
/// It is assumed that birth-rate exceeds death-rate, i.e. beta >= mu
///
#[derive(Debug, Clone, Deref, DerefMut, AsRef, AsMut)]
struct WildSSA {
    #[as_ref]
    #[as_mut]
    #[deref]
    #[deref_mut]
    internal_state: WildSSAInternalState,
    // read-only
    configuration: WildSSAConfiguration,
}

#[extendr]
impl WildSSA {
    pub fn new(
        n0: &[i32],
        birth_baseline: &[f64],
        death_baseline: &[f64],
        carrying_capacity: &[f64],
        migration_intercept: f64,
        migration_baseline: f64,
        // missing the softening index...
    ) -> Self {
        assert!(
            migration_intercept.abs() <= 0.00001,
            "not implemented yet, set to zero please"
        );
        assert!(!n0.is_empty());
        let n_len = n0.len();
        assert_eq!(n_len, birth_baseline.len());
        assert_eq!(birth_baseline.len(), death_baseline.len());
        assert_eq!(death_baseline.len(), carrying_capacity.len());

        assert!(migration_intercept.is_sign_positive());
        assert!(migration_baseline.is_sign_positive());

        // FIXME: what's a better behaviour pattern?
        // if n0 is all zero to begin with or carrying capacity is zero (thus propensity would be fully zero)
        // if n0.iter().all(|n| *n == 0) || carrying_capacity.iter().all(|x| x.abs() <= 0.0001) {
        if n0.iter().all(|n| *n == 0) {
            panic!("all are initialised to 0 or carrying capacity is all 0");
        }

        let n0 = as_u32(n0).expect("`n0` must be all non-negative integers");
        let n0: Box<[_]> = n0.into();

        let birth_baseline: Box<[_]> = birth_baseline.into();
        let death_baseline: Box<[_]> = death_baseline.into();

        // migration baseline must be normalised based on number of patches
        let migration_baseline = if n_len == 1 {
            // no migration can happen, set to zero.
            0.
        } else {
            migration_baseline / (n_len - 1) as f64
        };
        debug_assert!(migration_baseline.is_finite());

        let migration_intercept = if n_len == 1 {
            // no migration can happen, set to zero.
            0.
        } else {
            migration_intercept / (n_len - 1) as f64
        };
        debug_assert!(migration_intercept.is_finite());

        // initialize state
        let n = n0.clone();
        let carrying_capacity: Box<[_]> = carrying_capacity.into();

        let mut total_emigration_propensity = 0.;
        let mut total_immigration_propensity = 0.;

        let tmp = vec![0.; n_len.pow(2)];
        let mut emigration_propensity: Box<[_]> = tmp.clone().into();
        let mut immigration_propensity: Box<[_]> = tmp.into();
        let m0 = migration_baseline;
        for k_ij in 0..(n_len.pow(2)) {
            let (i, j) = (k_ij % n_len, k_ij / n_len);
            let n_i = n[i] as f64;
            let n_j = n[j] as f64;

            if i == j {
                // diagonals means birth/death
                let g_rate_ratio = (birth_baseline[i] - death_baseline[i]) / (carrying_capacity[i]);

                let dd_birth_rate =
                    death_baseline[i] + g_rate_ratio * (carrying_capacity[i] - n_i).max(0.);
                let dd_death_rate =
                    death_baseline[i] + g_rate_ratio * (n_i - carrying_capacity[i]).max(0.);
                // note: the `k_ij` vs  `k_ji` distinction does not matter here, as i == j.
                emigration_propensity[k_ij] = dd_death_rate * n_j;
                immigration_propensity[k_ij] = dd_birth_rate * n_i;
            } else {
                // APPROACH: SMOOTH
                // m_(j i)
                emigration_propensity[k_ij] = (m0 * (n_i - carrying_capacity[i]).exp().ln_1p())
                    / ((1_f64).ln_1p() * carrying_capacity[i]);
                emigration_propensity[k_ij] *= n_i;
                // m_(i j)
                immigration_propensity[k_ij] = (m0 * (n_j - carrying_capacity[j]).exp().ln_1p())
                    / ((1_f64).ln_1p() * carrying_capacity[j]);
                immigration_propensity[k_ij] *= n_j;
            }
            total_emigration_propensity += emigration_propensity[k_ij];
            total_immigration_propensity += immigration_propensity[k_ij];
        }
        let total_propensity = total_emigration_propensity + total_immigration_propensity;

        // TODO: initial time should be a result of Exp(1), as to have previous time...
        let current_time = 0.;
        Self {
            internal_state: WildSSAInternalState {
                n,
                total_propensity,
                current_time,
                emigration_propensity,
                immigration_propensity,
                total_emigration_propensity,
                total_immigration_propensity,
            },
            configuration: WildSSAConfiguration {
                n0,
                n_len,
                birth_baseline,
                death_baseline,
                carrying_capacity,
                migration_intercept,
                migration_baseline,
            },
        }
    }

    // fn generic_run_until(
    //     mut self,
    //     t_max: f64,
    //     f_birth_rate: fn(f64, f64, f64, f64, &mut f64, &mut f64, &mut f64),
    //     f_migration_rate: fn(f64, f64, f64, &mut f64),
    //     recorder: &mut impl Recorder,
    // ) {}

    pub fn run_and_record_patch(&self, t_max: f64, repetitions: usize, seed: u64) -> List {
        let mut rng = SmallRng::seed_from_u64(seed);

        // TODO: add ".from_capacity" version, that uses the largest size of simulation as the capacity..
        List::from_iter((0..repetitions).map(|repetition| {
            let mut patch_recorder = PatchRecord::new(repetition);
            self.clone().run_until(t_max, &mut rng, &mut patch_recorder);

            //TODO: if `.with_capacity`, do also shrink the storage after the process is done...
            patch_recorder
        }))
    }

    pub fn run_and_record_population(&self, t_max: f64, repetitions: usize, seed: u64) -> List {
        let mut rng = SmallRng::seed_from_u64(seed);
        // TODO: add ".from_capacity" version, that uses the largest size of simulation as the capacity..
        List::from_iter((0..repetitions).map(|repetition| {
            let mut population_recorder = PopulationRecord::new(repetition);
            self.clone()
                .run_until(t_max, &mut rng, &mut population_recorder);

            //TODO: if `.with_capacity`, do also shrink the storage after the process is done...
            population_recorder
        }))
    }

    pub fn run_and_record_fixed_time_population(
        &self,
        fixed_time_points: &[f64],
        t_max: f64,
        repetitions: usize,
        seed: u64,
    ) -> List {
        let mut rng = SmallRng::seed_from_u64(seed);
        List::from_iter((0..repetitions).map(|repetition| {
            let mut fixed_interval_population_recorder =
                PopulationFixedRecord::new(repetition, fixed_time_points);
            self.clone()
                .run_until(t_max, &mut rng, &mut fixed_interval_population_recorder);

            fixed_interval_population_recorder
        }))
    }

    // #[cfg(feature = "rayon")]
    pub fn run_and_record_fixed_time_population_par(
        &self,
        fixed_time_points: &[f64],
        t_max: f64,
        repetitions: usize,
        seed: u64,
    ) -> List {
        List::from_values(
            (0..repetitions)
                .into_par_iter()
                .map(|repetition| {
                    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
                    rng.set_stream(repetition as _);
                    let mut fixed_interval_population_recorder =
                        PopulationFixedRecord::new(repetition, fixed_time_points);
                    self.clone().run_until(
                        t_max,
                        &mut rng,
                        &mut fixed_interval_population_recorder,
                    );

                    fixed_interval_population_recorder
                })
                .collect::<Vec<_>>(),
        )
    }

    fn internal_debug_display(&self) -> String {
        format!("{self:#?}")
    }
}

impl WildSSA {
    #[inline(always)]
    fn run_until(mut self, t_max: f64, rng: &mut impl rand::Rng, recorder: &mut impl Recorder) {
        assert!(self.current_time <= t_max);

        let WildSSAConfiguration {
            n0: _,
            n_len,
            birth_baseline,
            death_baseline,
            carrying_capacity,
            migration_intercept: _,
            migration_baseline,
        } = self.configuration;
        let WildSSAInternalState {
            ref mut n,
            mut current_time,
            ref mut emigration_propensity,
            ref mut immigration_propensity,
            mut total_emigration_propensity,
            mut total_immigration_propensity,
            mut total_propensity,
        } = self.internal_state;
        // record initial state
        // FIXME: this will add another "record" if it is a re-run of an already half run sequence.
        recorder.add_initial_state(current_time, n.as_ref());
        'simulation_loop: loop {
            let delta_t: f64 = rng.sample(rand::distributions::Open01);
            let delta_t = -delta_t.ln() / total_propensity;
            assert!(delta_t.is_finite());
            current_time += delta_t;

            if current_time >= t_max {
                break;
            }

            // did emigration (0) or immigration occur?
            let is_emigration = rng.gen_bool(
                total_emigration_propensity
                    / (total_immigration_propensity + total_emigration_propensity),
            );
            let (dest_patch, src_patch) = if is_emigration {
                // (j i)
                let k_event =
                    rng.sample(WeightedIndex::new(emigration_propensity.as_ref()).unwrap());
                // note: k => (i,j): (k % n, k / n)
                (k_event / n_len, k_event % n_len)
            } else {
                // (i j)
                let k_event =
                    rng.sample(WeightedIndex::new(immigration_propensity.as_ref()).unwrap());
                (k_event % n_len, k_event / n_len)
            };

            match (is_emigration, dest_patch == src_patch) {
                // birth
                (false, true) => {
                    // src_patch === dest_patch
                    n[src_patch] += 1;
                    recorder.record_birth(current_time, src_patch, n[src_patch]);
                }
                // death
                (true, true) => {
                    // src_patch === dest_patch
                    n[src_patch] -= 1;
                    recorder.record_death(current_time, src_patch, n[src_patch]);
                }
                // emigration
                (true, false) => {
                    debug_assert_ne!(
                        n[src_patch], 0,
                        "({src_patch}, {dest_patch}), => {n:#?}\n{immigration_propensity:#?}\n{emigration_propensity:#?}"
                    );
                    n[src_patch] -= 1;
                    n[dest_patch] += 1;
                    recorder.record_migration(
                        current_time,
                        src_patch,
                        dest_patch,
                        n[src_patch],
                        n[dest_patch],
                    )
                }
                // note: duplicate of the branch before, but helps with readability.
                // immigration
                (false, false) => {
                    debug_assert_ne!(n[src_patch], 0, "({src_patch}, {dest_patch}), => {n:#?}\n{immigration_propensity:#?}\n{emigration_propensity:#?}");
                    n[src_patch] -= 1;
                    n[dest_patch] += 1;
                    recorder.record_migration(
                        current_time,
                        src_patch,
                        dest_patch,
                        n[src_patch],
                        n[dest_patch],
                    )
                }
            }
            // dbg!(is_emigration);
            // dbg!(src_patch, dest_patch, &n);

            // update the columns, as there are new `N`s and thus their value has changed.
            // let mut changed_patches = Vec::with_capacity(2);
            // if dest_patch != src_patch {
            //     changed_patches.push(src_patch); // or `dest_patch`
            // } else {
            //     changed_patches.push(dest_patch);
            //     changed_patches.push(src_patch);
            // };
            // let changed_patches = [src_patch, dest_patch];
            // alternative is `let changed_patches = [src_patch, dest_patch];`
            // and accept double updating the cases of (birth|death).
            // dbg!(&changed_patches);
            let criss_cross = (0..n_len)
                .map(|x| (x, src_patch))
                .chain((0..n_len).map(|x| (src_patch, x)))
                .chain((0..n_len).map(|x| (x, dest_patch)))
                .chain((0..n_len).map(|x| (dest_patch, x)));
            for (i, j) in criss_cross {
                let k_ij = i + j * n_len;
                // these are now the _new_ values..
                let n_i = n[i] as f64;
                let n_j = n[j] as f64;

                // remove the old values...
                total_emigration_propensity -= emigration_propensity[k_ij];
                total_immigration_propensity -= immigration_propensity[k_ij];

                if i == j {
                    // diagonals means birth/death
                    let g_rate_ratio =
                        (birth_baseline[i] - death_baseline[i]) / (carrying_capacity[i]);

                    let dd_birth_rate =
                        death_baseline[i] + g_rate_ratio * (carrying_capacity[i] - n_i).max(0.);
                    let dd_death_rate =
                        death_baseline[i] + g_rate_ratio * (n_i - carrying_capacity[i]).max(0.);
                    // dbg!(dd_birth_rate, dd_death_rate, dd_birth_rate - dd_death_rate);
                    // note: k_ij and k_ji distinction should not matter here.. as i == j
                    emigration_propensity[k_ij] = dd_death_rate * n_i;
                    immigration_propensity[k_ij] = dd_birth_rate * n_j;
                    // debug_assert_eq!(k_ij, k_ij);
                } else {
                    let m0 = migration_baseline;
                    // APPROACH: SMOOTH
                    // m_(j i)
                    emigration_propensity[k_ij] = (m0 * (n_i - carrying_capacity[i]).exp().ln_1p())
                        / ((1_f64).ln_1p() * carrying_capacity[i]);
                    emigration_propensity[k_ij] *= n_i;
                    // eprintln!("{}", &emigration_propensity);
                    // m_(i j)
                    immigration_propensity[k_ij] = (m0
                        * (n_j - carrying_capacity[j]).exp().ln_1p())
                        / ((1_f64).ln_1p() * carrying_capacity[j]);
                    immigration_propensity[k_ij] *= n_j;
                    debug_assert!(
                        immigration_propensity[k_ij].is_finite(),
                        "{m0},{n_j},{carrying_capacity:?},{j}"
                    );
                }
                total_immigration_propensity += immigration_propensity[k_ij];
                total_emigration_propensity += emigration_propensity[k_ij];
                debug_assert!(
                    !total_immigration_propensity.is_nan(),
                    "{immigration_propensity:#?}"
                );
                debug_assert!(!total_emigration_propensity.is_nan());
            }
            // update total propensity
            total_propensity = total_emigration_propensity + total_immigration_propensity;

            // Terminate due to extinction
            if n.iter().all(|&x| x == 0) {
                break 'simulation_loop;
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

//TODO: this `f_migration_smooth` is not used anywhere

#[inline(always)]
fn f_migration_smooth(n: f64, migration_baseline: f64, cc: f64, mig: &mut f64) {
    let log2: f64 = 1_f64.ln_1p();
    *mig = ((n - cc).exp().ln_1p() * migration_baseline) / (cc * log2);
}

/// Checks if all elements of `integer` are non-negative, thus returns a transmuted unsigned integer slice back.
pub(crate) fn as_u32(integer: &[i32]) -> Option<&[u32]> {
    let any_negative = integer.iter().any(|x| x.is_negative());
    if any_negative {
        return None;
    }
    Some(unsafe { std::mem::transmute::<&[i32], &[u32]>(integer) })
}

extendr_module! {
    mod simulation;

    impl WildSSA;
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use extendr_engine::with_r;

    use super::*;

    #[test]
    fn wild_ssa_init() {
        let n0 = &[1];
        let birth_baseline = &[2.];
        let death_baseline = &[3.];
        let carrying_capacity = &[4.];
        let migration_baseline = 4.;
        let migration_intercept = 0.;

        #[allow(unused_mut)]
        let mut wild_ssa = WildSSA::new(
            n0,
            birth_baseline,
            death_baseline,
            carrying_capacity,
            migration_intercept,
            migration_baseline,
        );

        dbg!(&wild_ssa);
    }
    #[test]
    fn two_patch() {
        with_r(|| {
            let n0 = &[0, 4];
            let birth_baseline = &[4., 4.];
            let death_baseline = &[1., 1.];
            let carrying_capacity = &[4., 4.];
            let migration_baseline = 1. / (8. / 12.);
            let migration_intercept = 0.;

            #[allow(unused_mut)]
            let mut wild_ssa = WildSSA::new(
                n0,
                birth_baseline,
                death_baseline,
                carrying_capacity,
                migration_intercept,
                migration_baseline,
            );
            let repetitions = 100;
            // let t_max = 1.;
            let t_max = 25.;
            let seed = 20240817;

            let output = wild_ssa.run_and_record_patch(t_max, repetitions, seed);
        });
    }

    #[test]
    fn three_patch_setup() {
        with_r(|| {
            let n0 = &[1, 4, 8];
            let birth_baseline = &[4., 4., 4.];
            let death_baseline = &[1., 1., 1.];
            let carrying_capacity = &[4., 4., 5.];
            let migration_baseline = 1. / (8. / 12.);
            let migration_intercept = 0.;

            #[allow(unused_mut)]
            let mut wild_ssa = WildSSA::new(
                n0,
                birth_baseline,
                death_baseline,
                carrying_capacity,
                migration_intercept,
                migration_baseline,
            );
            let repetitions = 100;
            // let t_max = 1.;
            let t_max = 25.;
            let seed = 20240817;

            let output = wild_ssa.run_and_record_patch(t_max, repetitions, seed);
        });
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
    }
}
