#![allow(non_snake_case)]
// disable periodically
#![allow(dead_code)]

use extendr_api::prelude::*;

mod full_migration_simulation; // work in progress
mod linspace;
mod logspace;
mod simulation;
mod triangle_indexing;

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod curveLandscape;
    use simulation;
    use triangle_indexing;
}
