use extendr_api::prelude::*;



/// Simulates birth, death and migration process of a multi-patch system.
/// 
/// 
#[extendr]
fn sim_bdm() -> () {

}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {
        
    }
}

extendr_module! {
    mod simulation;
    fn sim_bdm;
}