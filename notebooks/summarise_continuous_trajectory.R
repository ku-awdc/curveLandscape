library("tidyverse")
 
## Some random data as an example:
expand_grid(Replicate = seq_len(10L), Patch = seq_len(50L), Obs = seq_len(100L)) |>
  mutate(Time = runif(n(), 0, 120)) |>
  select(-Obs) |>
  mutate(N = rpois(n(), 50)) |>
  arrange(Replicate, Patch, Time) ->
  continuous_data
 
## Assume we know the data at time=0 (I guess this is in the model output?)
continuous_data |>
  group_by(Replicate, Patch) |>
  arrange(Time) |>
  slice(1L) |>
  ungroup() |>
  mutate(Time = 0) |>
  bind_rows(
    continuous_data
  ) ->
  continuous_data
 
## Converting model output to something plottable:
continuous_data |>
  distinct(Replicate, Patch) |>
  expand_grid(
    Time = seq(0, 120, by=10)
  ) |>
  ## Remove time=0, as that is already in the continuous data:
  filter(Time > 0) |>
  mutate(Using = TRUE) |>
  bind_rows(
    continuous_data |> mutate(Using = FALSE)
  ) |>
  group_by(Replicate, Patch) |>
  arrange(Time) |>
  fill(N, .direction = "down") |>
  ungroup() |>
  filter(Using) |>
  select(-Using) ->
  discrete_data
 
discrete_data |>
  group_by(Replicate, Time) |>
  summarise(MeanN = mean(N), .groups="drop")|>
  ggplot(aes(x=Time, y=MeanN, col=factor(Replicate))) +
  geom_step() +
  geom_point() +
  facet_wrap(~Replicate)
