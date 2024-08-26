devtools::load_all()



bdmix_model <- BirthDeathMix$new(
  n0 = 1,
  birth_baseline = 4,
  death_baseline = 1,
  carrying_capacity = 3,
  alpha_mix = 0.1
)


bdmix_model$run(
  t_max = 1., replicates = 250, seed = 20240826
)
