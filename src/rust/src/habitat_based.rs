use extendr_api::extendr_module;
use extendr_api::prelude::*;

#[extendr]
fn get_habitat_island_ids(neighbourhood: List) -> Integers {
    let neighbourhood: &[Integers] = unsafe { neighbourhood.as_typed_slice_raw() };

    // neighbourhood[0].clone();
    // Integers::new(10)
    todo!()
}

extendr_module! {
    mod habitat_based;
    fn get_habitat_island_ids;
}
