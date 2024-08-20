devtools::load_all()

new_model_outputs <- read_rds(".cache/061_different_m0_1_new_model.rds")


new_model_outputs %>%
  mutate(landscape_type = landscape_type %>% fct_inorder()) %>%
  select(landscape_type, set_patch_size, total_cc, n_len) %>%
  bind_cols(
    effective_cc =
      new_model_outputs$stat_binned_output %>%
        map(
          . %>% slice_tail(n = 1)
        ) %>%
        bind_rows(),
    n_len_gt_one_capacity = new_model_outputs$grid %>% map(
      . %>% st_drop_geometry() %>% summarise(gt_one_cc = sum(Capacity >= 1))
    ) %>%
      bind_rows()
  ) %>%
  select(-n_len) %>%
  select(set_patch_size, landscape_type, total_cc, -time, ci_lower, mean, ci_upper, gt_one_cc) %>%
  arrange(set_patch_size) %>%
  # tinytable::tt(theme = "void") %>%
  # print("typst")
  # (digits = 8)
