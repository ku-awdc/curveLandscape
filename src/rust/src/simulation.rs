use extendr_api::prelude::*;
use itertools::{izip, Itertools};
use rand::{rngs::SmallRng, Rng, SeedableRng};

#[derive(Debug)]
struct PopConfig {
    // initial_count: [],
}
/// Simulates birth, death and migration process of a multi-patch system.
///
///
#[extendr]
fn sim_bdm(n0: &[i32], birth_baseline: &[f64], death_baseline: &[f64], t_max: f64) -> Vec<u32> {
    assert_eq!(n0.len(), birth_baseline.len());
    assert_eq!(birth_baseline.len(), death_baseline.len());
    let mut rng = SmallRng::seed_from_u64(20240506);
    let mut t = 0.0;
    let n0: Vec<u32> = n0.iter().map(|&x| x.try_into().unwrap()).collect_vec();
    let mut n: Vec<u32> = Vec::with_capacity(n0.len());
    // n.fill(0);
    n.extend_from_slice(n0.as_slice());
    assert_eq!(n.len(), n0.len());

    loop {
        let delta_t: f64 = rng.sample(rand::distributions::Open01);
        //TODO: calculate birth and death rates according to the density... 
        let total_propensity: f64 = izip!(
            n.iter(),
            birth_baseline.iter(),
            death_baseline.iter()
        )
        .map(|(&n, b, d)| (n as f64) * (b + d))
        .sum1()
        .unwrap();
        let delta_t = -delta_t.ln() / total_propensity;
        t += delta_t;

        if t >= t_max {
            break;
        }
        
        // 

    }

    // birth_baseline.iter().sum::<f64>() +
    // death_baseline.iter().sum::<f64>()

    // n0.iter().map(|x| *x as f64).sum1().unwrap();
    n
}

extendr_module! {
    mod simulation;
    fn sim_bdm;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {}
}
