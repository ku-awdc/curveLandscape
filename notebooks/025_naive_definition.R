#'
#'
#'
expand_grid(
  beta_0 = 3,
  mu_0 = 0.1,
  K = 5
) %>%
  rowid_to_column("config") %>%
  mutate(
    N = map(max(K),
            \(K) seq.default(0, K * 2, by = .1))
  ) %>%
  # glimpse()
  unnest(N) %>%
  mutate(
    # beta = (beta_0) * pmax(0, 1 - N / K),
    # mu = (mu_0) * pmax(0, -(1 - N / K))

    # beta = (beta_0 - mu_0) * pmax(0, 1 - N / K),
    # mu = (beta_0 - mu_0) * pmax(0, -(1 - N / K))

    #  APPROACH
    #     beta =
    #       (beta_0 + mu_0) * pmax(0, 1 - N / K) +
    #       (beta_0 + mu_0) * pmax(0, (N / K) - 1),
    #     mu =
    #       (mu_0 + beta_0)/K * pmax(0, 1 - N / K) +
    #       (mu_0 + beta_0)/K * pmax(0, N / K - 1)

    # JUNK
    # beta = mu_0 + (beta_0 - mu_0) * pmax(0, K - N) / K,
    # mu = mu_0 + (beta_0 - mu_0) * pmax(0, N - K) / K,
    # # mu = -mu,


    beta = mu_0 + (beta_0 - mu_0) * pmax(0, -(N - K)) / K,
    mu = mu_0 + (beta_0 - mu_0) * pmax(0, N - K) / K,
    # mu = -mu,
    # growth = beta + mu,

    # alternatively
    growth = beta - mu,
  ) %>%
  pivot_longer(c(beta, mu, growth), names_to = "rate") %>%
  mutate(is_growth = rate == "growth",
         rate_label = dplyr::if_else(
           is_growth, "growth",
           "birth / death"
         )
  ) %>%
  # glimpse() %>%
  identity() %>%  {
    ggplot(.) +
      aes(N, value, group = str_c(rate, config)) +
      geom_line(aes(color = rate)) +
      # geom_step(aes(color = factor(config))) +

      # facet_grid(is_growth~.) +
      # facet_grid(rate_label~.) +

      geom_line(
        aes(y = (beta_0 - mu_0) * (1 - N / K),
            color = "r(1 - N/K)"),
        linetype = "dotted",
        linewidth = 1.1,
        data = . %>% dplyr::filter(is_growth)
      ) +
      # geom_line(
      #   aes(y = abs((beta_0 - mu_0) * (1 - N / K))),
      #   linetype = "dotted",
      #   linewidth = 1.1,
      #   data = . %>% dplyr::filter(is_growth)
      # ) +

      geom_hline(yintercept=0, alpha = .5) +

      geom_hline(
        aes(yintercept = beta_0, color = "beta_0"),
        linetype = "dashed",
        data = . %>% dplyr::filter(!is_growth)
      ) +
      geom_hline(
        aes(yintercept = mu_0, color = "mu_0"),
        linetype = "dashed",
        data = . %>% dplyr::filter(!is_growth)
      ) +

      facet_wrap(~rate_label, ncol = 1,
                 scales = "free_y") +

      labs(y = "rate", color = NULL) +
      # lims(y = c(0, NA)) +
      theme_bw(20) +
      theme(legend.position = "left") +
      theme(aspect.ratio = 1/2) +
      # guides(color = "none") +
      theme(
        legend.margin = margin(),
      ) +
      guides(color = guide_legend(
        direction = "vertical",
        # theme = theme(legend.direction = "horizontal"),
        override.aes = list(linewidth = 1.5))) +
      NULL
  }
ggsave(
  # height = 14.5,
  device = svglite::svglite,
  scale = 2,
  filename = "figures/025_naive_definition.svg"
)
#' Let's simulate this
#'
#'
#'
#'
#'
#'
rate_to_probability <- function(rate) {
  1 - exp(-rate)
}

# set.seed(20240412)

beta_0 <- 4
mu_0   <- 1

#' carrying capacity
k0 <- 15
u0 <- seq.default(1, ceiling(1.5 * k0), by = 1)

reps <- 100
reps <- 50
max_t_years <- 25

if (any(k0 > u0)) {
  warning("Starting population larger than carrying capacity")
}

# max_t_weeks <- max_t_years * 52

# stopifnot(length(u0) == 1)
# TODO: maybe sort it
current_u <- u0 %>% rep.int(times = reps)
current_t <- numeric(length(current_u))
id_rep <- seq_along(current_u)
results <- cbind(rep = integer(0), t = numeric(0), u = numeric(0))
results <- rbind(
  results,
  cbind(rep = id_rep, t = current_t, u = current_u)
)
results_info <- list(
  u0 = tibble(id_u0 = seq_along(u0) %>% rep.int(reps),
              rep = id_rep, u0 = current_u)
)

repeat {

  stopifnot(all(!is.na(current_u),
                !is.na(current_t)))

  current_n <- length(current_u)
  next_unif <- runif(current_n)

  stopifnot(
    vapply(next_unif, \(x) !isTRUE(all.equal.numeric(x, 0)), FUN.VALUE = logical(1))
  )
  # birth <- growth_baseline * pmax(0, 1 - current_u / k0)
  # death <- growth_baseline * pmax(0, current_u / k0 - 1)

  # birth <- beta_0 * pmax(0, (1 - (current_u * (beta_0 - mu_0)) / (k0 * (beta_0 + mu_0))))
  # death <- mu_0 * (1 + (current_u * (beta_0 - mu_0)) / (k0 * (beta_0 + mu_0)))

  birth <- mu_0 + (beta_0 - mu_0) * pmax(0, -(current_u / k0 - 1))
  death <- mu_0 + (beta_0 - mu_0) * pmax(0, current_u / k0 - 1)

  stopifnot(all(birth >= 0), all(death >= 0))

  delta_t <- -log(next_unif) / ((birth + death) * current_u)
  delta_t[current_u == k0] <- 0

  # DEBUG
  # (death == 0) & (birth == 0) & (current_u!=k0)

  # ALTERNATIVE:
  # delta_t <- rexp(n = current_n, rate = (birth + death) * current_u)
  stopifnot(all(!is.infinite(delta_t)))
  stopifnot(all(delta_t >= 0))
  next_t <- current_t + delta_t

  # IDEA: use rexp instead you crazy person
  birth <- rate_to_probability(birth)
  death <- rate_to_probability(death)
  stopifnot(all(birth >= 0), all(death >= 0),
            all(birth <= 1), all(death <= 1))

  delta_u <- rbinom(n = current_n,
                    size = 1,
                    prob = birth / (birth + death))
  delta_u[is.na(delta_u)] <- 0
  delta_u <- 2 * delta_u - 1
  next_u <- current_u + delta_u

  # shift
  current_u <- next_u
  current_t <- next_t

  # record
  results <- rbind(
    results,
    cbind(rep = id_rep, t = current_t, u = current_u)
  )

  # retire elapsed trajectories
  elapsed_t <- which(current_t >= max_t_years)
  elapsed_u <- which(current_u == 0)
  elapsed_k0 <- which(current_u == k0) # or delta_t == 0
  # elapsed_k0 <- integer()
  elapsed_reps <- unique(c(elapsed_t, elapsed_u, elapsed_k0))

  if (length(elapsed_reps) > 0) {
    # message(glue("{length(elapsed_u)}"))

    #add `t_max` to the `elapsed_u` group
    elapsed_u_and_k0 <- unique(c(elapsed_u, elapsed_k0))
    results <- rbind(
      results, cbind(rep = id_rep[elapsed_u_and_k0],
                     t = rep.int(max_t_years, length(elapsed_u_and_k0)),
                     u = current_u[elapsed_u_and_k0])
    )

    # remove the elapsed simulations from consideration
    current_u <- current_u[-elapsed_reps]
    current_t <- current_t[-elapsed_reps]
    id_rep <- id_rep[-elapsed_reps]
  }
  stopifnot(length(current_u) == length(current_t),
            length(current_t) == length(id_rep)
  )
  if (length(current_u) == 0) {
    break
  }
}
stopifnot(length(id_rep) == 0)

# current_t
# current_u
# id_rep
results <- results |>
  as_tibble() %>%
  left_join(
    results_info %>% bind_rows(),
    by = join_by(rep)
  )
u0
#'
#' Cache the result
fs::dir_create(".cache")
results %>% write_rds(".cache/025_results_with_info.RDS")

#'
p_traj <- results |>
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step() +
  # geom_line(color = "grey90", show.legend = FALSE) +
  geom_line(aes(alpha = u), show.legend = FALSE) +
  # geom_step(aes(alpha = u), show.legend = FALSE) +
  geom_hline(aes(yintercept = k0), linetype = "dotted") +

  scale_alpha_continuous(range = c(0.15)) +
  # xlim(c(NA, 4)) +
  theme_bw()
#'
p_traj
#'
#'
results |>
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step(aes(color = u0)) +
  geom_line(aes(color = u0), alpha = 0.25) +
  geom_hline(aes(yintercept = k0), linetype = "dotted") +

  scale_alpha_continuous(range = c(0.15)) +
  scale_color_binned() +
  xlim(c(NA, 11)) +
  ylim(c(0, NA)) +

  theme_bw() +
  theme(legend.position = "bottom") +
  NULL
#'
results %>%
  # glimpse()
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step() +
  # geom_line(color = "grey90", show.legend = FALSE) +
  geom_line(aes(alpha = u0), show.legend = FALSE) +
  # geom_step(aes(alpha = u), show.legend = FALSE) +
  geom_hline(aes(yintercept = k0), linetype = "dotted") +

  scale_alpha_continuous(range = c(0.15)) +

  theme_bw()

# p_traj +
#   facet_grid(~u0) +
#   lims(u0 = c(0,10))
#'
# ode_times <- results$t %>% zapsmall() %>%  unique() %>% sort()
#'
#'

ode_results <- deSolve::ode(
  y = c(u = u0), times = seq.default(0, 25, by = 0.001),
  parms = list(r = beta_0 - mu_0, k0 = k0,
               birth_baseline = beta_0,
               death_baseline = mu_0),
  func = function(times, y, parameters) {
    with(parameters, {
      dN <- r * y * (1 - y / k0)
      list(dN)
    })
  }
)
ode_results <-
  ode_results %>%
  unclass() %>%
  as_tibble() %>%
  rename(t = time) %>%
  pivot_longer(
    -t,
    names_pattern = "u(\\d+)",
    names_to = c("id_u0"),
    values_to = "u") %>%
  mutate(u0 = u0[as.integer(id_u0)],
         id_u0 = NULL)
#TODO


p_traj_and_ode <- p_traj +
  aes(color = id_u0) +
  geom_line(aes(color = u0, group = u0), data = ode_results) +

  labs(color = NULL) +
  theme(legend.position = "bottom") +
  NULL
p_traj_and_ode


results |>
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step(aes(color = u0)) +
  geom_line(aes(color = u0), alpha = 0.15) +
  geom_hline(aes(yintercept = k0), linetype = "dotted") +


  # ODE
  geom_line(aes(color = u0, group = u0),
            linewidth = 0.75, linetype = "dashed",
            data = ode_results) +

  labs(color = NULL, y = "N", x = "time") +
  scale_alpha_continuous(range = c(0.15)) +
  scale_color_binned() +
  xlim(c(NA, 11)) +
  ylim(c(0, NA)) +
  theme_bw(base_size = 14) +
  theme(aspect.ratio = 1) +
  theme(legend.position = "bottom") +
  NULL

fs::dir_create("figures")
ggsave(
  width = 10*2,
  filename = "figures/025_balanced_birth_death_with_ode.svg",
  scale = 2,
  device = svglite::svglite
)

#'
# fs::dir_create("figures")
# pdf(file = "figures/017_gillespie_growth_with_ode.pdf")
# results |>
#   group_by(u0) %>%
#   group_map(\(data, key) {
#     ggplot(data) +
#       aes(t, u, group = rep) +
#       # geom_step() +
#       # geom_line(color = "grey90", show.legend = FALSE) +
#       geom_line(aes(alpha = u), show.legend = FALSE) +
#       # geom_step(aes(alpha = u), show.legend = FALSE) +
#       geom_hline(aes(yintercept = k0), linetype = "dotted") +
#
#       scale_alpha_continuous(range = c(0.15)) +
#
#       theme_bw() +
#       geom_line(aes(color = u0, group = u0), data = ode_results) +
#
#       labs(color = NULL) +
#       theme(legend.position = "bottom") +
#       NULL
#   })
# dev.off()

#'
# VALIDATION: must be zero / empty
# results %>%
#   dplyr::filter(t < 0)
#'
results %>%
  # dplyr::filter(id_u0 == 5) %>%
  ggplot() +
  aes(t, u, group = rep) +
  # geom_line(aes(color = identity(u0)), show.legend = TRUE) +

  # `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  geom_smooth(aes(group = u0, color = u0)) +
  geom_line(aes(color = u0, group = u0), data = ode_results) +

  # scale_color_viridis_c(direction = -1) +
  scale_color_viridis_b(direction = -1) +

  labs(color = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  NULL
#'
#' Plotting P(extinction, t | u0)
#'
results %>%
  slice_max(t, by = rep) %>%
  summarise(
    `P(extinction)` = mean(u < 1),
    .by = c(u0)
  ) %>%
  add_row(
    u0 = 0, `P(extinction)` = 1
  ) %>%
  identity() %>%
  glimpse() %>%
  ggplot() +
  aes(u0, `P(extinction)`) +
  geom_line() +
  # geom_line(aes(color = u0)) +
  # scale_colour_viridis_c(direction = -1) +
  # scale_color_viridis_b(direction = -1, n.breaks = 9) +

  labs(y = "P(extinct)",
       x = "u0",
       color = NULL) +
  theme_bw(base_size = 14) +
  ylim(y = c(0, NA)) +
  NULL
#'
#'
#'
#TODO: I need to do this for each of these lines, and calculate the difference
#' between the two, although they do look very similar...
#'
results_approxfun <-
  tapply(
    X = results[, c("t", "u")],
    INDEX = results$rep,
    FUN = \(x) {
      # rule = 1:2 for extinct numbers make sense.
      approxfun(x$t, y = x$u, method = "constant")
    }
  )
#'
max_t_years
#'
# DEBUG
# curve(
#   results_approxfun[[40]](x), to = 100
# )
results_info %>% str()
traj_approx_df <-
  results_approxfun %>%
  enframe("id_rep", "traj_approxfun") %>%
  bind_cols(results_info)
stopifnot(!anyNA(traj_approx_df))
stopifnot(all(traj_approx_df$id_rep == traj_approx_df$rep))
traj_approx_df
# traj_approx_df %>% View()

t_linspace <- seq.default(0, max_t_years, length.out = 100)
id_time <- seq_along(t_linspace)
id_time_linspace <- rep.int(id_time, length(results_info$u0$u0))

outcome_linspace <-
  vapply(results_approxfun,
         \(traj_fun) traj_fun(t_linspace),
         FUN.VALUE = double(length(t_linspace)))
# outcome_linspace %>%
#   Matrix::Matrix() %>% `[`(1:10, 1:10)
outcome_linspace %>% dim()
# id_time_linspace %>% dim()
id_time_linspace %>% length()
u0_linspace <- rep(results_info$u0$u0, each = length(t_linspace))
u0_linspace %>% length()

prob_ext <- tapply(
  X = outcome_linspace,
  INDEX = list(u0 = u0_linspace, time = id_time_linspace),
  FUN = \(x) mean(x < 1)
)
prob_ext %>%
  str()
prob_ext %>%
  as_tibble()
prob_ext %>%
  as.data.frame.table() %>%
  as_tibble() %>%
  mutate(u0 = as.numeric(u0),
         time = as.numeric(time),
         time = t_linspace[time]) %>%
  identity() %>%
  ggplot() +
  aes(time, Freq, group = u0) +
  geom_line(aes(color = u0)) +
  # scale_colour_viridis_c(direction = -1) +
  scale_color_viridis_b(direction = -1, n.breaks = 9) +

  labs(y = "P(extinct)",
       x = "time",
       color = NULL) +
  theme_bw(base_size = 14) +
  NULL
#'
#'
#'
#'

