devtools::load_all()
sim_bd_only(
  n0 = c(3L),
  birth_baseline = 2.5,
  death_baseline = 0.15,
  carrying_capacity = 5L,
  t_max = 5
) %>%
  as_tibble() %>%
  rename(N = state) %>%
  tail()

expand_grid(
  carrying_capacity = as.integer(c(1, 2, 3, 4, 5, 10)),
  n0 = as.integer(c(1, 2, 3, 4, 5, 6, 7, 8)),
  t_max = c(1, 2, 3, 5, 10)
) %>%
  rowid_to_column("id_config") ->
df_config
#'
#'
reps <- 100
df_config %>%
  mutate(
    extinction = pmap_dbl(select(., n0, carrying_capacity, t_max), \(n0, carrying_capacity, t_max) {
      vapply(
        seq_len(reps), \(rep) {
          sim_bd_only(
            n0 = n0,
            # birth_baseline = 4,
            # death_baseline = 1,
            birth_baseline = 2.5,
            death_baseline = 0.15,
            carrying_capacity = carrying_capacity,
            t_max = t_max
          ) %>%
            as_tibble() %>%
            rename(N = state) ->
          sim_output
          stopifnot(nrow(sim_output) != 0)
          extinct <- sim_output %>%
            tail(1) %>%
            with({
              N == 0
            })
          extinct
        }, logical(1)
      ) %>% mean()
    })
  ) ->
df_extinction_calc

df_extinction_calc %>%
  glimpse() %>%
  identity() %>%
  {
    ggplot(.) +
      aes(n0, extinction, group = str_c("cc: ", carrying_capacity, "t_max: ", t_max)) +
      geom_step(aes(color = fct(str_c(t_max, " yr"))), linewidth = 1.2) +
      facet_wrap(~carrying_capacity) +
      scale_color_viridis_d(option = "mako") +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      labs(
        color = "Duration in years",
        y = "Probability of stochastic extinction",
        x = "Initial number of sows"
      ) +
      NULL
  }


fs::dir_create("figures")
ggsave(
  filename = "figures/038_birth_death_only_stochastic_extinction.svg",
  device = svglite::svglite,
  scale = 2.4
  # scaling = 3,
  # scale = 3,
  # width = 2*4.27,
  # height = 2.5
)
