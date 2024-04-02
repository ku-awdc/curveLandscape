#'
#'
#' 
yearly_prob_to_weekly_prob <- function(p_annual) { 
  1 - (1 - p_annual)**(1/52)
}
#'
#'
probability_to_rate <- function(p, period = 1) {
  -(log(1 - p)) / period
}

# Assuming initial population
initial_population <- 100
# Assuming the ratio of males in the initial population
initial_male_ratio <- 0.5
# Assuming initial age distribution: 0-1 years, 1-2 years, >=2 years
initial_age_distribution <- c(0.3, 0.2, 0.5) # Adjust based on actual data if available
initial_males <- initial_population * initial_male_ratio * initial_age_distribution
initial_females <- (initial_population - sum(initial_males)) * c(0.5, 0.5)

stopifnot(
  all.equal(sum(initial_males, initial_females), initial_population)
)

male_mortality_0_1 <- 0.49
male_mortality_1_2 <- 0.729
male_mortality_2plus <- 0.655
# litter distribution
male_breeding_proportion <- 0.2
female_breeding_proportion <- 1 - male_breeding_proportion

female_mortality_0_1 <- 0.4720
female_mortality_2plus <- 0.6540

rate_mortality <- c(
  female_mortality_0_1,
  female_mortality_2plus,
  male_mortality_0_1,
  male_mortality_1_2,
  male_mortality_2plus
)
rate_mortality <- probability_to_rate(rate_mortality)
# rexp(100, rate_mortality)

litter_size_distr <- c(2, 4, 13, 22.5, 17.5, 14, 9, 3)
sample_litter_size <- function(n) {
  sample.int(length(litter_size_distr), size = n, replace = TRUE, prob = litter_size_distr)
}

rate_breeding <- 1/52

# probability_to_rate(1, period = 52)
# 1

# sample_litter_size(10000)
# VALIDATION
# sample_litter_size(10000) |>
#   hist.default(probability = TRUE)

# rate_aging <- c(
#   1/52,
#   1/52,
#   1/52,
# )

u0 <- c(initial_females, initial_males)

max_t_years <- 20
max_t_weeks <- max_t_years * 52

reps <- 100
# stopifnot(length(u0) == 1)
current_u <- rep.int(u0, reps)
current_t <- rep.int(0, reps)
u_prototype <- rbind(c(u=u0))[NULL,]
u_prototype
results <- cbind(rep = integer(0), t = numeric(0), u = u_prototype)
results

id_rep <- seq_len(reps)

repeat {
  current_n <- length(current_u)
  next_unif <- runif(current_n)

  stopifnot(
    vapply(next_unif, \(x) !isTRUE(all.equal.numeric(x, 0)), FUN.VALUE = logical(1))
  )

  delta_t <- -log(next_unif) / ((birth + death) * current_u)
  next_t <- current_t + delta_t

  delta_u <- rbinom(n = current_n, size = 1, prob = death / (birth + death))
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
  elapsed_t <- which(current_t >= max_t_weeks)
  elapsed_u <- which(current_u == 0)
  elapsed_reps <- unique(c(elapsed_t, elapsed_u))

  if (length(elapsed_reps) > 0) {
    # message(glue("{length(elapsed_u)}"))

    # remove the elapsed simulations from consideration
    current_u <- current_u[-elapsed_reps]
    current_t <- current_t[-elapsed_reps]
    id_rep <- id_rep[-elapsed_reps]
  }
  stopifnot(
    length(current_u) == length(current_t),
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

results |>
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step() +
  geom_line(aes(alpha = u), show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.15)) +
  theme_bw()

#'
#'
#'
