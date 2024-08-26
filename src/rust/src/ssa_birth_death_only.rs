use derive_more::derive::Debug;
use extendr_api::prelude::*;

#[derive(Debug)]
struct BirthDeathMix;

#[extendr]
impl BirthDeathMix {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn run(&self) {}
}

impl BirthDeathMix {
    fn new_internal() -> Self {
        Self
    }
    fn run_internal(&self) {}
}

extendr_module! {
    mod ssa_birth_death_only;
    impl BirthDeathMix;
}
