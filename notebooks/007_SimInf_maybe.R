

library(SimInf)
library(tidyverse)
transitions <- c("S -> beta*S*I/(S+I+R) -> I",
                 "I -> gamma*I -> R")
compartments <- c("S", "I", "R")

n <- 4
u0 <- data.frame(S = rep(99, n), I = rep(5, n), R = rep(0, n))
u0
model <- mparse(transitions = transitions,
                compartments = compartments,
                gdata = c(beta = 0.16, gamma = 0.077),
                u0 = u0,
                tspan = 0:150)
model
model_run <- SimInf::run(model)
model_run %>% class
SimInf::trajectory(model_run) %>%
  as_tibble() %>%
  pivot_longer(c(S, I, R)) %>%
  ggplot() +
  aes(time, value, group = str_c(name, node)) +
  geom_line(aes(color=name)) +
  theme_bw() +
  NULL

SimInf::trajectory(model_run) %>%
  as_tibble()


SimInf::trajectory(model_run) %>% plot(c("S"))
