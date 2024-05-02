## Birth vs mortality
### Matt Denwood, 2024-04-22

library("tidyverse")
theme_set(theme_light())

## We should base this on PigDensity, which is pigs per square km
## of forest-equivalent habitat (where e.g. 1km2 arable land is
## equivalent to 0.1km2 forest or similar)

## Then if we say that a group size of 25 wild boar usually has
## a home range of 5km, that implies that 5 pigs per km2 forest
## equivalent is a "carrying capacity" of sorts

## Birth parameters:  intercept, linear term, quadratic term (on log scale)
b0 <- -1
b1 <- -0.1
b2 <- -0.01

## Death parameters:  intercept, linear term, quadratic term (on log scale)
d0 <- -3
d1 <- 0.2
d2 <- 0.01

## Effective carrying capacity is given by:
kfun <- function(k) abs(exp(b0 + b1*k + b2*k^2) - exp(d0 + d1*k + d2*k^2))
optimize(kfun, interval=c(0,100))

tibble(PigDensity = seq_len(100L)/10) |>
  mutate(BirthRate = exp(b0 + b1*PigDensity + b2*PigDensity^2)) |>
  mutate(DeathRate = -exp(d0 + d1*PigDensity + d2*PigDensity^2)) |>
  mutate(OverallRate = BirthRate + DeathRate) |>
  pivot_longer(-PigDensity) |>
  ggplot(aes(x=PigDensity, y=value, col=name)) +
  geom_line() +
  geom_vline(xintercept=5, lty="dashed") +
  ylab("Rates")


## Interpretation and restrictions:

### b0:  log of birth rate in an unrestricted population
### d0:  log of death rate in an unrestricted population

### b1:  log-linear change in birth rate with increasing density - must be negative
### d1:  log-linear change in mortality rate with increasing density - must be positive

### b2:  log-linear quadratic change in birth rate with increasing density - must be negative
### d2:  log-linear quadratic change in mortality rate with increasing density - must be positive

## Note: b2 and d2 are "optional" in the sense that the exponentiation gives us a non-linear system anyway:

## Birth parameters:  intercept, linear term (on log scale)
b0 <- -1
b1 <- -0.15

## Death parameters:  intercept, linear term (on log scale)
d0 <- -3
d1 <- 0.25

## Effective carrying capacity is given by:
kfun <- function(k) abs(exp(b0 + b1*k) - exp(d0 + d1*k))
optimize(kfun, interval=c(0,100))

tibble(PigDensity = seq_len(100L)/10) |>
  mutate(BirthRate = exp(b0 + b1*PigDensity)) |>
  mutate(DeathRate = -exp(d0 + d1*PigDensity)) |>
  mutate(OverallRate = BirthRate + DeathRate) |>
  pivot_longer(-PigDensity) |>
  ggplot(aes(x=PigDensity, y=value, col=name)) +
  geom_line() +
  geom_vline(xintercept=5, lty="dashed") +
  ylab("Rates")

