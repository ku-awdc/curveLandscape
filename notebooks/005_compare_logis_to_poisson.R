
rate_to_probability <- function(rate) {
  1 - exp(-rate)
}

probability_to_rate <- function(prob) {
  # p = 1 - exp(-rate)
  # 1 - p = exp(-rate)
  # log(1-p) = -rate
  # rate = -log(1-p)
  -log(1-prob)
}

options(device = "windows")

plot.new()
curve(probability_to_rate(x), to = 1)
curve(qlogis(x), to = 1, add = TRUE)

plot.new()
curve(rate_to_probability(x), to = 5)
curve(plogis(x), to = 5, add = TRUE)
