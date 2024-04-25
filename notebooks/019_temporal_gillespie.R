
# Define the constants
beta_0 = 1
beta_1 = 0.5

# Define the function beta(t)
td_beta <- function(t, beta_0, beta_1) {
  beta_0 * (1 + beta_1 * cos(2 * pi * t))
}


curve(td_beta(x, beta_0, beta_1), to = 4)

# Define the constants
beta_0 = 1
beta_1 = 0.5

# Define the function beta(t)
td_beta <- function(t, beta_0, beta_1, offset) {
  beta_0 * (1 + beta_1 * cos(2 * pi * (t - offset)))
}


curve(td_beta(x, beta_0, beta_1, offset = 11/12), to = 4)
