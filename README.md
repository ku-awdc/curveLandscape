# `curveLandscape`

## Installation

You must have the Rust compiler installed in order to build this package from source.
See [extendr: Getting Started guide](https://extendr.github.io/get-started.html), with Step 1 being sufficient
for building this package.

```r
remotes::install_github("ku-awdc/curveLandscape)
```

## Parallelism

The underlying rust crate makes use of `rayon`, which means,
that this package will you use as many threads as available, or
adhere to the limit set by the environment variable `RAYON_NUM_THREADS`.
See [`rayon::num_threads`](https://docs.rs/rayon/latest/rayon/struct.ThreadPoolBuilder.html#method.num_threads)
for more details.

## Developer Guide

