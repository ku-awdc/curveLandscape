use derive_more::derive::Debug;
use extendr_api::{prelude::*, IntoRobj};
use rand::SeedableRng;

#[derive(Debug, Clone)]
struct BirthDeathMix {
    n0: i32,
    birth_baseline: f64,
    death_baseline: f64,
    carrying_capacity: f64,
    alpha_mix: f64,
}

#[extendr]
impl BirthDeathMix {
    pub(crate) fn new(
        n0: i32,
        birth_baseline: f64,
        death_baseline: f64,
        carrying_capacity: f64,
        alpha_mix: f64,
    ) -> Self {
        Self::new_internal(
            n0,
            birth_baseline,
            death_baseline,
            carrying_capacity,
            alpha_mix,
        )
    }

    pub(crate) fn run(&self, t_max: f64, replicates: usize, seed: u64) -> List {
        assert!(t_max > 0.);
        let duration = t_max;

        let mut rng = rand_chacha::ChaChaRng::seed_from_u64(seed);
        let mut recorder = Record::new(duration);
        for _replicate in 0..replicates {
            self.run_internal(t_max, &mut rng, &mut recorder);
        }
        List::from_values([recorder])
    }
}

impl BirthDeathMix {
    fn new_internal(
        n0: i32,
        birth_baseline: f64,
        death_baseline: f64,
        carrying_capacity: f64,
        alpha_mix: f64,
    ) -> Self {
        Self {
            n0,
            birth_baseline,
            death_baseline,
            carrying_capacity,
            alpha_mix,
        }
    }

    fn run_internal(&self, t_max: f64, rng: &mut impl rand::Rng, recorder: &mut impl Recorder) {
        let mut current_time = 0.;
        // let f_update_scheme = update_birth_death_rate_paper2p;
        let f_update_scheme = update_birth_death_rate;

        let Self {
            n0: N,
            birth_baseline,
            death_baseline,
            carrying_capacity: K,
            alpha_mix: alpha,
        } = self.clone();

        let mut N = N as _;
        let mut N_double = N as f64;
        let mut birth_rate = 0.;
        let mut death_rate = 0.;
        f_update_scheme(
            birth_baseline,
            death_baseline,
            alpha,
            N_double,
            K,
            &mut birth_rate,
            &mut death_rate,
        );
        let mut total_propensity = (birth_rate + death_rate) * N_double;

        recorder.record_initial(N);
        'simulation_loop: loop {
            let delta_t: f64 = rng.sample(rand::distributions::Open01);
            let delta_t = -delta_t.ln() / total_propensity;
            debug_assert!(delta_t.is_finite());
            current_time += delta_t;

            if current_time >= t_max {
                break;
            }

            let is_birth = rng.gen_bool((birth_rate * N_double) / (total_propensity));
            if is_birth {
                N += 1;
                N_double += 1.;
                recorder.record_birth(N);
            } else {
                // is death...
                N -= 1;
                N_double -= 1.;
                recorder.record_death(N)
            }
            // update things...
            f_update_scheme(
                // &mut signed_birth_rate,
                birth_baseline,
                death_baseline,
                alpha,
                N_double,
                K,
                &mut birth_rate,
                &mut death_rate,
            );
            total_propensity = (birth_rate + death_rate) * N_double;

            // extinction?
            if N == 0 {
                break 'simulation_loop;
            }
        }

        recorder.record_final(N);
    }
}

#[inline(always)]
fn update_birth_death_rate(
    birth_baseline: f64,
    death_baseline: f64,
    alpha: f64,
    N_double: f64,
    K: f64,
    birth_rate: &mut f64,
    death_rate: &mut f64,
) {
    // birth = pmax(0, birth_baseline - ((birth_baseline - death_baseline) * alpha * N) / K),
    // death = death_baseline + ((birth_baseline - death_baseline) * (1 - alpha) * N) / K,
    // death = death + pmax(0, (((birth_baseline - death_baseline) * alpha * N) / K)  - birth_baseline),
    let signed_birth_rate =
        birth_baseline - ((birth_baseline - death_baseline) * alpha * N_double) / K;
    *birth_rate = signed_birth_rate.max(0.);
    *death_rate =
        death_baseline + ((birth_baseline - death_baseline) * (1. - alpha) * N_double) / K;
    *death_rate += (-signed_birth_rate).max(0.);
}

#[inline(always)]
fn update_birth_death_rate_paper2p(
    birth_baseline: f64,
    death_baseline: f64,
    _alpha: f64,
    N_double: f64,
    K: f64,
    birth_rate: &mut f64,
    death_rate: &mut f64,
) {
    let g_rate_ratio = (birth_baseline - death_baseline) / K;
    *birth_rate = death_baseline + g_rate_ratio * (K - N_double).max(0.);
    *death_rate = death_baseline + g_rate_ratio * (N_double - K).max(0.);
}

#[derive(IntoRobj)]
struct Record {
    replicates: usize,
    // non-negative, non-NAN
    duration: f64,
    extinct: usize,
    survived: usize,
}

impl Record {
    fn new(duration: f64) -> Self {
        let extinct = 0;
        let survived = 0;
        let replicates = 0;
        Self {
            replicates,
            duration,
            extinct,
            survived,
        }
    }
}

pub(crate) trait Recorder {
    fn record_initial(&mut self, current_n: u32);

    fn record_birth(&mut self, current_n: u32);
    fn record_death(&mut self, current_n: u32);

    fn record_final(&mut self, current_n: u32);
}

impl Recorder for Record {
    #[inline]
    fn record_initial(&mut self, current_n: u32) {
        assert_ne!(current_n, 0, "cannot initiate this recorder with 0 animals");
        self.replicates += 1;
    }

    #[inline(always)]
    fn record_birth(&mut self, _current_n: u32) {}

    #[inline(always)]
    fn record_death(&mut self, _current_n: u32) {}

    #[inline]
    fn record_final(&mut self, current_n: u32) {
        if current_n == 0 {
            self.extinct += 1;
        } else {
            self.survived += 1;
        }
    }
}

extendr_module! {
    mod ssa_birth_death_only;
    impl BirthDeathMix;
}
