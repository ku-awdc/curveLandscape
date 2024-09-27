library(curveLandscape)
library(tidyverse)
theme_set(theme_light())


ccs <- c(20,30,50)
bind_rows(
  all_models_patch_level(ccs, c(0,0,ccs[3]), t_max=10, rep=250) |> mutate(Inits = "Largest CC"),
  all_models_patch_level(ccs, ceiling(ccs/2), t_max=10, rep=250) |> mutate(Inits = "Half CC"),
  all_models_patch_level(ccs, ccs*2, t_max=10, rep=250) |> mutate(Inits = "Double CC"),
) ->
  res

tribble(~Patch, ~CC,
  "Patch 1", ccs[1],
  "Patch 2", ccs[2],
  "Patch 3", ccs[3],
  "Total", sum(ccs)
) -> ccint

res |>
  filter(Iteration==0, Time <= 5, Model!="Wedge") |>
  mutate(Patch = case_when(
    Patch == 0 ~ "Total",
    TRUE ~ str_c("Patch ", Patch)
  )) |>
  mutate(Model = case_when(
    Model == "NoMigration" ~ "Scenario 1",
    Model == "Static" ~ "Scenario 2",
    Model == "Smooth" ~ "Scenario 3",
  )) |>
  mutate(Inits = factor(Inits, levels=c("Double CC","Half CC","Largest CC"))) |>
  ggplot(aes(x=Time, y=N, ymin=LCI, ymax=UCI, col=Inits, lty=Model)) +
  geom_hline(aes(yintercept=CC), ccint, lty="dashed") +
  geom_line() +
  facet_grid(Patch ~ Type, scales="free_y") +
  labs(x="Time", y="Number of animals", col=element_blank(), lty=element_blank()) +
  theme(legend.position = "bottom")
ggsave("landscapes_A1.pdf", width=7, height=7)


ccs <- c(20,30,50)/10
bind_rows(
  all_models_patch_level(ccs, c(0,0,ccs[3]), t_max=10, rep=250) |> mutate(Inits = "Largest CC"),
  all_models_patch_level(ccs, ceiling(ccs/2), t_max=10, rep=250) |> mutate(Inits = "Half CC"),
  all_models_patch_level(ccs, ccs*2, t_max=10, rep=250) |> mutate(Inits = "Double CC"),
) ->
  res

tribble(~Patch, ~CC,
  "Patch 1", ccs[1],
  "Patch 2", ccs[2],
  "Patch 3", ccs[3],
  "Total", sum(ccs)
) -> ccint

res |>
  filter(Iteration==0, Time <= 5, Model!="Wedge") |>
  mutate(Patch = case_when(
    Patch == 0 ~ "Total",
    TRUE ~ str_c("Patch ", Patch)
  )) |>
  mutate(Model = case_when(
    Model == "NoMigration" ~ "Scenario 1",
    Model == "Static" ~ "Scenario 2",
    Model == "Smooth" ~ "Scenario 3",
  )) |>
  mutate(Inits = factor(Inits, levels=c("Double CC","Half CC","Largest CC"))) |>
  ggplot(aes(x=Time, y=N, ymin=LCI, ymax=UCI, col=Inits, lty=Model)) +
  geom_hline(aes(yintercept=CC), ccint, lty="dashed") +
  geom_line() +
  facet_grid(Patch ~ Type, scales="free_y") +
  labs(x="Time", y="Number of animals", col=element_blank(), lty=element_blank()) +
  theme(legend.position = "bottom")
ggsave("landscapes_A2.pdf", width=7, height=7)


