#' Let's talk about optimisation.
#'
#' Simulate poisson-distributed data with offset.
#'
#'
model <- structure(list(), class = "OffsetPoisson")

model$simulate <- function(n, offset, rate) {
  offset + rpois(n = n, lambda = rate)
}

sim_data <- model$simulate(1000, offset = 0.2, rate = 1)
sim_data %>% hist()
sim_data %>% density() %>% plot()
#' So these tools still think there is coverage within
#' (0, 0.2) which is not possible.
#'


model$likelihood <-function(data, offset, rate) {
  -prod(as.numeric(data > offset) *
          dpois(x = round(data), lambda = rate))
}
model$likelihood <- Vectorize(model$likelihood, vectorize.args = c("offset","rate"))
model$likelihood(sim_data, offset = c(0.2, 1), rate = c(1,0.2))

curve(
  (\(x) model$likelihood(sim_data, offset=x, rate = 1))(x),
  to = 10,
  from = 0, n = 100
)

model$likelihood(sim_data, offset = 0.2, rate = 1)

model$fn <- function(par) {
  model$likelihood(data = sim_data, offset = par[1], rate = par[1])
}
optim(
  c(offset = 0.3, rate = 0.1),
  # fn = model$likelihood, data = sim_data
  # fn = partial(model$likelihood, data = sim_data)
  fn = model$fn
)

model$likelihood <-function(data, offset, rate) {
  -prod(as.numeric(data > offset) *
          dpois(x = round(data - offset), lambda = rate))
}
model$likelihood <- Vectorize(model$likelihood, vectorize.args = c("offset","rate"))
model$likelihood(sim_data, offset = c(0.2, 1), rate = c(1,0.2))

curve(
  # partial(model$likelihood, data = sim_data, rate = 1)
  (\(x) model$likelihood(sim_data, offset=x, rate = 0.5))(x)
)

model$likelihood(sim_data, offset = 0.2, rate = 1)

model$fn <- function(par) {
  model$likelihood(data = sim_data, offset = par[1], rate = par[1])
}
optim(
  c(offset = 0.3, rate = 0.1),
  # fn = model$likelihood, data = sim_data
  # fn = partial(model$likelihood, data = sim_data)
  fn = model$fn
)
#' Let's try with the log-likelihood instead
#'
model$loglikelihood <-function(data, offset, rate) {
  -sum(log(as.numeric(data > offset)),
       dpois(x = round(data - offset), lambda = rate, log = TRUE))
}
model$loglikelihood <- Vectorize(model$loglikelihood, vectorize.args = c("offset","rate"))
model$loglikelihood(sim_data, offset = c(0.2, 1), rate = c(1,0.2))

curve(
  # partial(model$loglikelihood, data = sim_data, rate = 1)
  (\(x) model$loglikelihood(sim_data, offset=x, rate = 0.5))(x)
)

model$loglikelihood(sim_data, offset = 0.2, rate = 1)

model$fn <- function(par) {
  model$loglikelihood(data = sim_data, offset = par[1], rate = par[1])
}
model$fn(c(0.3, 0.1))
optim(
  c(offset = 0.3, rate = 0.1),
  # fn = model$likelihood, data = sim_data
  # fn = partial(model$likelihood, data = sim_data)
  fn = model$fn,
  # lower = c(0,0),
  # method = "L-BFGS-B",
  control = list(
    trace = 6
  )
) %>% tryCatch(error = \(x) "didn't work")
#'
#'
#'
#' Turns out, this non-sense doesn't work.
#'
model2<-structure(list(), class = "offsetPoisModel")
model2$likelihood <- function(offset, rate, yi) {
  prod(exp(-rate) * (rate ** (yi - offset)) / factorial(yi - offset))
}
model2$likelihood <- Vectorize(model2$likelihood, vectorize.args = c("offset","rate"))

# factorial(0.5)
model2$likelihood(offset = 0.2, rate = 2, yi = sim_data)



optim(
  par = c(offset = 0.2, rate = 20) %>% log(),
  \(par) -model2$likelihood(yi = sim_data,
                            offset = exp(par[1]),
                            rate = exp(par[2])),
  control = list(trace=6)
) %>%
  print() -> optim_result
optim_result$par
optim_result$par %>% exp()

curve(expm1(x), from = - 10, to = 10)
curve(log1p(x), from = 0, to = 200)

#' Now, let us try with the log-likelihood instead
#'
#'
model2$loglikelihood <- function(offset, rate, yi) {
  # browser()
  c(0, -Inf)[as.numeric(offset < 0 | rate < 0) + 1]
  - sum(-length(yi) * rate,
        (yi - offset) * log(rate),
        -log(factorial(yi - offset)))
}
model2$loglikelihood <- Vectorize(model2$loglikelihood, vectorize.args = c("offset","rate"))

model2$loglikelihood(offset = 0.2, rate = 1, yi = sim_data)
optim(
  par = c(offset = 0.2, rate = 20) %>% log(),
  \(par) model2$loglikelihood(yi = sim_data,
                              offset = exp(par[1]),
                              rate = exp(par[2])),
  control = list(trace = 6)
) %>%
  print() -> optim_result
optim_result$par
optim_result$par %>% exp() %>%
  print() -> par_result

# plot.new()
plot(sim_data)
abline(h = par_result["offset"] + par_result["rate"], lw = 2, lty = "dotted")

par_result
#' Let's do it with `dpois` now...
#'
model2$loglikelihood_dpois <- function(offset, rate, yi) {
  # browser()
  c(0, -Inf)[as.numeric(offset < 0 | rate < 0) + 1]
  - sum(dpois(round(yi - offset), lambda = rate, log = TRUE))
}
model2$loglikelihood_dpois <- Vectorize(model2$loglikelihood_dpois, vectorize.args = c("offset","rate"))

model2$loglikelihood_dpois(offset = 0.2, rate = 1, yi = sim_data)
optim(
  par = c(offset = 0.2, rate = 20) %>% log(),
  \(par) model2$loglikelihood_dpois(yi = sim_data,
                                    offset = exp(par[1]),
                                    rate = exp(par[2])),
  control = list(trace = 6)
) %>%
  print() -> optim_result
optim_result$par
optim_result$par %>% exp() %>%
  print() -> par_result

# plot.new()
plot(sim_data)
abline(h = par_result["offset"] + par_result["rate"], lw = 2, lty = "dotted")

