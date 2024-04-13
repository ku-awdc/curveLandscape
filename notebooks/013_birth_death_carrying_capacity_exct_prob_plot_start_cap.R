yearly_prob_to_weekly_prob <- function(p_annual) {
  1 - (1 - p_annual)**(1 / 52)
}

rate_to_probability <- function(rate) {
  1 - exp(-rate)
}

# set.seed(20240412)


litter_size_distr <- c(2, 4, 13, 22.5, 17.5, 14, 9, 3) / 100
avg_litter_size <- sum(seq_along(litter_size_distr) * litter_size_distr)
avg_litter_size <- avg_litter_size / 2

# litter_size_distr %>% sum()
# litter_size0 <- c(`0` = 1 - sum(litter_size_distr), litter_size_distr)
# sample_litter_size <- function(n) {
#   sample.int(length(litter_size0),
#              size = n, replace = TRUE, prob = litter_size0) - 1
# }

birth <-  avg_litter_size
death <- 0.6540
# birth <- yearly_prob_to_weekly_prob(rate_to_probability(birth))
# death <- yearly_prob_to_weekly_prob(death)
c(birth = birth, death = death)

birth_baseline <- birth
death_baseline <- death

(birth_baseline - death_baseline) / (birth_baseline + death_baseline)

#' carrying capacity
k0 <- 80
u0 <- 1

if (any(k0 > u0)) {
  warning("Starting population larger than carrying capacity")
}

reps <- 250
max_t_years <- 25
# max_t_weeks <- max_t_years * 52

stopifnot(length(u0) == 1)
current_u <- rep.int(u0, reps)
current_t <- rep.int(0, reps)
id_rep <- seq_len(reps)
results <- cbind(rep = integer(0), t = numeric(0), u = numeric(0))
results <- rbind(
  results,
  cbind(rep = id_rep, t = current_t, u = current_u)
)

repeat {

  stopifnot(all(!is.na(current_u),
                !is.na(current_t)))

  current_n <- length(current_u)
  next_unif <- runif(current_n)

  stopifnot(
    vapply(next_unif, \(x) !isTRUE(all.equal.numeric(x, 0)), FUN.VALUE = logical(1))
  )
  # stopifnot(current_u < k0)

  birth <- birth_baseline * (1 - (birth_baseline - death_baseline) / (birth_baseline + death_baseline) * current_u / k0)
  death <- death_baseline * (1 + (birth_baseline - death_baseline) / (birth_baseline + death_baseline) * current_u / k0)
  stopifnot(all(birth > 0), all(death > 0))

  delta_t <- -log(next_unif) / ((birth + death) * current_u)
  next_t <- current_t + delta_t

  # IDEA: use rexp instead you crazy person
  # birth <- yearly_prob_to_weekly_prob(rate_to_probability(birth))
  # death <- yearly_prob_to_weekly_prob(death)
  birth <- rate_to_probability(birth)
  death <- rate_to_probability(death)
  delta_u <- rbinom(n = current_n, size = 1, prob = birth / (birth + death))
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
  # elapsed_t <- which(current_t >= max_t_weeks)
  elapsed_t <- which(current_t >= max_t_years)
  elapsed_u <- which(current_u == 0)
  elapsed_reps <- unique(c(elapsed_t, elapsed_u))

  if (length(elapsed_reps) > 0) {
    # message(glue("{length(elapsed_u)}"))

    #add `t_max` to the `elapsed_u` group
    results <- rbind(
      results, cbind(rep = id_rep[elapsed_u],
                     # t = current_t[elapsed_u],
                     t = rep.int(max_t_years, length(elapsed_u)),
                     u = current_u[elapsed_u])
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
  as_tibble()

p_traj <- results |>
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step() +
  geom_line(aes(alpha = u), show.legend = FALSE) +
  # geom_step(aes(alpha = u), show.legend = FALSE) +
  geom_hline(aes(yintercept = k0), linetype = "dotted") +

  scale_alpha_continuous(range = c(0.15)) +

  theme_bw()
p_traj
#'
ode_times <- results$t %>% zapsmall() %>%  unique() %>% sort()
#'
#'

ode_results <- deSolve::ode(
  y = c(u = u0), times = seq.default(0, 25, by = 0.001),
  parms = list(r = birth_baseline - death_baseline, k0 = k0,
               birth_baseline = birth_baseline,
               death_baseline = death_baseline),
  func = function(times, y, parameters) {
    with(parameters, {
      dN <- r * y * (1 - y / k0)
      list(dN)
    })
  }
)
ode_results <- ode_results %>%
  as_tibble() %>%
  rename(t = time)

p_traj +
  # `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  geom_smooth(aes(group = NA, color = "smooth")) +
  geom_line(aes(color = "ODE", group = NA), data = ode_results) +

  labs(color = NULL) +
  theme(legend.position = "bottom") +
  NULL
#'
#'
results_approxfun <-
  tapply(
    X = results[, c("t", "u")],
    INDEX = results$rep,
    FUN = \(x) {
      approxfun(x$t, y = x$u, method = "constant")
      # approxfun(x$t, y = x$u, method = "linear")
      # approxfun(x$t, y = x$u, method = "constant", rule = 1:2)
      # approxfun(x$t, y = x$u, method = "linear", rule = 1:2)
    }
  )
#'
max_t_years
#'
# DEBUG
# curve(
#   results_approxfun[[40]](x), to = 100
# )

t_linspace <- seq.default(0, max_t_years, length.out = 100)
outcome_linspace <-
  vapply(results_approxfun,
         \(traj_fun) traj_fun(t_linspace),
         FUN.VALUE = double(100))

prob_ext <- apply(outcome_linspace, MARGIN = 1, \(x) mean(x<1))
prob_ext
# stopifnot(prob_ext %>% names() %>% as.numeric() %>% is.unsorted() %>% `!`())

tibble(
  t = t_linspace,
  prob_ext = prob_ext,
) %>%
  ggplot() +
  aes(t, prob_ext) +
  geom_line() +
  labs(y = "P(extinct)",
       x = "time") +
  theme_bw(base_size = 14) +
  NULL
#'
#'
#'
#'

