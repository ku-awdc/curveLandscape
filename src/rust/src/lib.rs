#![allow(non_snake_case)]
// disable periodically
#![allow(dead_code)]

use extendr_api::prelude::*;

#[deprecated]
mod birth_death_discrete;
mod linspace;
mod logspace;
mod simulation;

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod curveLandscape;
    use simulation;
}
