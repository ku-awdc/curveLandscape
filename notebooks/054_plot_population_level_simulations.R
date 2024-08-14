devtools::load_all()

TimeMax <- 50
DeltaT <- 1 / 12

stopifnot(DeltaT <= TimeMax)
all_landscapes_trans <- purrr::transpose(all_landscapes)

seed_point <- all_landscapes$grid[[2]][1283, ] %>% st_centroid()
seed_point

seed_indices <- all_landscapes$grid %>%
  map_int(\(grid) {
    st_filter(grid %>% rowid_to_column(), seed_point)$rowid
    # browser()
    # grid[st_within(grid, seed_point),]
  })
seed_indices
map2(
  all_landscapes$grid, seed_indices, \(x, y) {
    x[y, ]
  }
) %>%
  bind_rows()



naive_landscape <- all_landscapes_trans[[5]]
naive_seed_point <- seed_indices[5]
naive_landscape$grid[naive_seed_point, ]

n0 <- integer(naive_landscape$n_len)
n0[naive_seed_point] <- 1L
n0

wild_ssa_model <- WildSSA$new(
  # n0 = n0,
  n0 = naive_landscape$grid$Capacity %>% as.integer(),
  birth_baseline = rep.int(4, times = naive_landscape$n_len),
  death_baseline = rep.int(1, times = naive_landscape$n_len),
  carrying_capacity = naive_landscape$grid$Capacity,
  migration_baseline = 1 / (8 / 12)
)

# WildSSA$run_and_record_patch
# WildSSA$run_and_record_population
# wild_ssa_model$run_and_record_patch(
#   t_max = 25,
#   repetitions = 1000,
#   20002224
# ) -> wild_ssa_output

# wild_ssa_output %>% map_int(
#   \(output) {
#     output$count %>% length()
#   }
# ) %>%
#   print() %>%
#   # table() %>%
#   sum() %>%
#   print()

wild_ssa_model$run_and_record_population(
  t_max = 25,
  repetitions = 100,
  20002224
) -> wild_ssa_output

wild_ssa_output[[50]] %>%
  as_tibble()

wild_ssa_output %>%
  map_int(
    \(output) {
      output$count %>% length()
    }
  ) %>%
  print() %>%
  # table() %>%
  sum() %>%
  print()


# bench::press(
#   repetitions = c(250, 500, 750, 1000),
#   {
#     bench::mark(
#       wild_ssa_patch_recorder = wild_ssa_model$run_and_record_patch(
#         t_max = 25,
#         repetitions = repetitions,
#         20002224
#       ),
#       check = FALSE
#     )
#   }
# ) -> bp_wild_ssa_runs

# plot(bp_wild_ssa_runs) + facet_grid(~.)

# WildSSA$internal_debug_display()
# wild_ssa_model$internal_debug_display()
# print(wild_ssa_model)

# pdf("figures/050_run_ssa_on_all_landscapes_minimum_n0.pdf")

# pdf("figures/050_run_ssa_on_all_landscapes_eq_cc.pdf")
# pdf("figures/050_run_ssa_on_all_landscapes_gt_cc.pdf")
# pdf("figures/050_run_ssa_on_all_landscapes_lt_cc.pdf")

# for (id_grid in seq_along(all_landscapes_trans)) {
# with(all_landscapes_trans[[id_grid]], {

# landscape <- all_landscapes_trans[[5]]

# ssa_source_only_wedge(
#   n0 = pmin(landscape$grid$Capacity, 1) %>% as.integer(), # minimum

#   n0 = ceiling(landscape$grid$Capacity) %>% as.integer(), # eq_cc
#   # n0 = pmax(0, ceiling(grid$Capacity - 1)) %>% as.integer(), # lt_cc
#   # n0 = pmax(0, ceiling(grid$Capacity + 1)) %>% as.integer(), # gt_cc

#   # n0 = pmin(grid$Capacity, 1) %>% as.integer(),
#   # n0 = grid$Capacity,
#   birth_baseline = rep.int(4., nrow(grid)),
#   death_baseline = rep.int(1., nrow(grid)),
#   # growth_rate = 4 - 1,
#   carrying_capacity = landscape$grid$Capacity,
#   migration_baseline = 1 / (8 / 12),
#   # delta_t = 1 / 52,
#   t_max = TimeMax,
#   reps = 100
# ) -> ssa_output

naive_landscape$total_cc

wild_ssa_output <- wild_ssa_output %>%
  bind_rows()
wild_ssa_output %>%
  glimpse() %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, count, group = repetition) +
      geom_step(aes(alpha = count, color = repetition), show.legend = FALSE) +
      labs(x = "time [years]") +
      geom_hline(
        aes(yintercept = naive_landscape$total_cc),
        linetype = "dotted",
        linewidth = 1.1
      ) +
      # labs(caption = glue("Landscape type {landscape_type}, and total patches {n_len}")) +
      ggpubr::theme_pubclean(15) +
      # theme_light(15) +
      NULL
  }
