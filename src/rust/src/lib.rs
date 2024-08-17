#![allow(non_snake_case)]
// disable periodically
#![allow(dead_code)]

use extendr_api::prelude::*;

mod full_migration_simulation; // work in progress
mod linspace;
mod logspace;
mod simulation;
// mod ssa_source_only_migration;
mod triangle_indexing;

mod habitat_based;

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod curveLandscape;
    // use ssa_source_only_migration;
    use simulation;
    use triangle_indexing;
    use habitat_based;
}
