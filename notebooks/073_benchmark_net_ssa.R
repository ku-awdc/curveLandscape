devtools::load_all()

#' All landscape will represent this total carrying capacity
total_cc <- 500

landscape_n_len <- seq.default(from = 50, to = 3000, by = 1)
landscape_n_len <- landscape_n_len[sample.int(length(landscape_n_len), size = 25, replace = FALSE)]
landscape_n_len <- sort(landscape_n_len, decreasing = FALSE)

all_cc <- map(landscape_n_len, \(n_len) {
  cc <- LaplacesDemon::rdirichlet(1, alpha = rep.int(1, times = n_len)) * total_cc
  # assume n = 1, so vector
  tibble(
    cc = as.numeric(cc)
  )
})

tibble(
  total_cc = total_cc,
  n_len = landscape_n_len,
  cc = all_cc
) -> configuration
transpose(configuration)[[1]]$cc$cc

t_max <- 10
fixed_time_points <- seq.default(from = 0, to = t_max, by = 1 / 12)
fixed_time_points %>% tail()
repetitions <- 25


# // SANITY CHECK
landscape <- transpose(configuration)[[1]]
# landscape$cc$cc %>% round() %>% as.integer() %>% sum()

WildSSA$new(
  n0 = landscape$cc$cc %>% round() %>% as.integer(),
  birth_baseline = rep.int(4, times = landscape$n_len),
  death_baseline = rep.int(1, times = landscape$n_len),
  carrying_capacity = landscape$cc$cc,
  migration_intercept = 0,
  migration_baseline = 1 / (8 / 12)
  # migration_baseline = 0
) -> wild_ssa_model

wild_ssa_model$run_and_record_fixed_time_population(
  t_max = t_max,
  fixed_time_points = fixed_time_points,
  repetitions = repetitions,
  20002224
)





bench::press(
  landscape = transpose(configuration),
  {
    bench::mark(
      # max_iterations = 1, # testing
      # dud = {
      #   landscape[[1]]$n_len %>% print()
      # }
      check = FALSE,
      # constructing_wild_ssa = with(landscape, {
      #   landscape <- landscape[[1]]
      #   # browser()
      #   WildSSA$new(
      #     n0 = landscape$cc$cc %>% round() %>% as.integer(),
      #     birth_baseline = rep.int(4, times = landscape$n_len),
      #     death_baseline = rep.int(1, times = landscape$n_len),
      #     carrying_capacity = landscape$cc$cc,
      #     migration_intercept = 0,
      #     migration_baseline = 1 / (8 / 10)
      #   )
      # }),
      running_100_ssa = with(landscape, {
        landscape <- landscape[[1]]
        # browser()
        WildSSA$new(
          n0 = landscape$cc$cc %>% round() %>% as.integer(),
          birth_baseline = rep.int(4, times = landscape$n_len),
          death_baseline = rep.int(1, times = landscape$n_len),
          carrying_capacity = landscape$cc$cc,
          migration_intercept = 0,
          migration_baseline = 1 / (8 / 12)
        ) -> wild_ssa_model
        wild_ssa_model$run_and_record_fixed_time_population_par(
          t_max = t_max,
          fixed_time_points = fixed_time_points,
          repetitions = repetitions,
          20002224
        )
      })
    )
  }
) -> bm_press_result

bm_press_result %>%
  unnest_wider(landscape) %>%
  mutate(landscape = n_len) %>%
  mutate(time_len = lengths(time)) %>%
  print(width = Inf, n = Inf) %>%
  identity() %>%
  {
    ggplot(.) +
      geom_line() +
      geom_point() +
      aes(n_len, median) +
      labs(x = "Total patches", y = "median [seconds]") +
      ggpubr::theme_pubclean(15) +
      NULL
  }


  fs::dir_create("figures")
  ggsave(
    filename = "figures/073_benchmarks.svg",
    device = svglite::svglite,
    # scaling = 3,
    scale = 1.9,
    # width = 2*4.27,
    # height = 2.5
  )



# plot()
# mutate(landscape = seq_len(length(landscape))) %>%
# plot() %>% {
#   . +

#     facet_wrap(NULL)
# }
# bm_press_result %>% mutate(landscape = landscape %>% map_int(\(landscape) seq_len(nrow(landscape$n_len[1]))))


# all_cc %>%
#   bind_rows()

# print(round(LaplacesDemon::rdirichlet(5, alpha = c(1,1,1)) * total_cc))  %>% rowSums()


# s_cc <- LaplacesDemon::rdirichlet(1, alpha = c(1,0))


# s_cc[1,] %>% sum()
# s_cc %>% rowSums() # === 1

# s_cc %>% colSums() # WRONG
