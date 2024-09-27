devtools::load_all()

library(tidyverse)
theme_set(theme_light())

## 3-patch scenario:
### patches have 0.75 a/km2, 0.25 a/km2, 0.1 a/km2
dens <- c(0.75, 0.5, 0.25)
### Sizes are 1,2,5,10,25
sizes <- c(1,2,5,10,25)
lapply(sizes, \(x) x*dens)

#ccs <- c(19.9,7.9,9.9)
#n0s <- c(10,0,0)

ccs <- dens*sizes[2]
ccs <- c(10,0.5,0.5)
n0s <- c(sum(ccs),1,1)

res <- all_models_patch_level(ccs, n0s)
ggplot(res |> filter(Iteration==0), aes(x=Time, y=N, col=Type)) +
  geom_line() +
  facet_grid(Patch~Model, scales="free_y")


ccs <- c(1,2,5,10,25)
n0s <- pmax(ccs/2, 1)
res <- all_models_patch_level(ccs, n0s, t_max=10, rep=250)
ggplot(res |> filter(Iteration==0), aes(x=Time, y=N, ymin=LCI, ymax=UCI, col=Type)) +
  geom_ribbon() + #alpha=0.25, col="transparent", fill="grey") +
  geom_line() +
  facet_grid(Patch~Model, scales="free_y")



ccs <- c(1,2,5,12,25)
n0s <- pmax(ccs/2, 1)
res <- all_models_patch_level(ccs, n0s, t_max=10, rep=250)
ggplot(res |> filter(Iteration==0), aes(x=Time, y=N, ymin=LCI, ymax=UCI, col=Type)) +
  geom_ribbon() + #alpha=0.25, col="transparent", fill="grey") +
  geom_line() +
  facet_grid(Patch~Model, scales="free_y")


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


