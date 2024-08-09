devtools::load_all()

patch_sizes <- c(1, 2, 5, 10, 25) # km^2

naive_grids <- map(patch_sizes, \(patch_sizes_in_km2) create_naive_grid(patch_sizes_in_km2))
