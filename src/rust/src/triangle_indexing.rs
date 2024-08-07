use extendr_api::prelude::*;

#[extendr]
/// Returns the row and column from a 0-indexed, column-wise linear index `k` for a square matrix of dimension `n` x `n`
pub(crate) fn get_row_col(k: usize, n: usize) -> [usize; 2] {
    // let j = (((n - 1).pow(2) - 2 * k) as f64).sqrt() as usize - n + 1;
    // todo!();

    // kp := k_max - k

    let kp = (n * (n - 1)) / 2 - 1 - k;
    let kp = kp as f64;
    let p = (((1. + 8. * kp).sqrt() - 1.) / 2.).floor();
    let n_float = n as f64;
    let i = n_float - (kp - p * (p + 1.) / 2.) - 1.;
    let j = n_float - p - 2.;
    [i as _, j as _]
}

#[extendr]
/// Returns the linear, 0-index id for (i,j) for n x n matrix.
pub(crate) fn get_linear_id(i: usize, j: usize, n: usize) -> usize {
    (n * (n - 1) - (n - j) * (n - j - 1)) / 2 + i - j - 1
}

#[extendr]
/// Return the total number of elements in lower-triangular matrix (without diagonal)
pub(crate) fn get_total_number_of_elements(n: usize) -> usize {
    // FIXME: make safe when n = 0
    n * (n - 1) / 2
}

extendr_module! {
    mod triangle_indexing;

    // internal functions, remove eventually
    fn get_row_col;
    fn get_linear_id;
    fn get_total_number_of_elements;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tri_u_index() {
        // n = 2
        assert_eq!(get_row_col(0, 2), [1, 0]);

        // n = 3
        let ij = get_row_col(0, 3);
        assert_eq!(ij, [1, 0]);
        let ij = get_row_col(1, 3);
        assert_eq!(ij, [2, 0]);
        let ij = get_row_col(2, 3);
        assert_eq!(ij, [2, 1]);

        //TODO: make tests
        // index_to_i_j_colwise_nodiag(1 - 1, 4) # 1 0
        // index_to_i_j_colwise_nodiag(2 - 1, 4) # 2 0
        // index_to_i_j_colwise_nodiag(3 - 1, 4) # 3 0
        // index_to_i_j_colwise_nodiag(4 - 1, 4) # 2 1
        // index_to_i_j_colwise_nodiag(5 - 1, 4) # 3 1
        // index_to_i_j_colwise_nodiag(6 - 1, 4) # 3 2
    }

    /// if this test passes, we know atleast that get_row_col and get_linear_id are consistent with
    /// each other.
    #[test]
    fn test_triangular_index_and_back() {
        let n = 5;
        // dbg!(n);
        for k in 0..get_total_number_of_elements(n) {
            let [i, j] = get_row_col(k, n);
            // dbg!(i, j);
            let kk = get_linear_id(i, j, n);
            // dbg!(k, kk);
            assert_eq!(kk, k);
        }

        // for a few n
        for n in [2, 3, 4, 5, 6, 7, 8] {
            for k in 0..get_total_number_of_elements(n) {
                let [i, j] = get_row_col(k, n);
                let kk = get_linear_id(i, j, n);
                assert_eq!(kk, k);
            }
        }
    }
}
