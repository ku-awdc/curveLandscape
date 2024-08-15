devtools::load_all()

expand_grid(
  m0 = 1 / (8 / 12),
  cc = 5,
  N = seq.default(0, cc * 2, by = 0.1),
) %>%
  mutate(
    mj_wedge = (
      m0 * pmax(N - (cc - 1), 0)
    ) / cc,
    mj_smooth = (m0 * log1p(exp(N - cc))) / (log1p(1) * cc)
  ) %>%
  identity() %>%
  {
    ggplot(.) +
      # aes(x = N / cc) +
      aes(x = N) + 
      geom_line(aes(y = mj_wedge, color = "wedge")) +
      geom_line(aes(y = mj_smooth, color = "smooth")) +
      labs(y = "Baseline Migration Rate") +
      labs(x = "N / K") +
      # geom_vline(aes(xintercept = cc / cc), linetype = "dotted") +
      geom_vline(aes(xintercept = cc), linetype = "dotted") +
      geom_hline(aes(yintercept = m0), linetype = "dotted") +
      labs(color = NULL) +
        ggpubr::theme_pubclean(15) +
          theme(legend.position = "bottom") +
      NULL
  }


  fs::dir_create("figures")
  ggsave(
    filename = "figures/062_dd_migration_baseline_plot.svg",
    device = svglite::svglite,
    # scaling = 3,
    scale = 1.9
    # width = 2*4.27,
    # height = 2.5
  )



# expand_grid(
#   m0 = 1 / (8 / 12),
#   cc = 1:10,
# ) %>%
#   mutate(N = cc %>% map(\(cc) seq.default(0, cc * 2, length.out = 25))) %>%
#   unnest(N) %>%
#   mutate(
#     mj_wedge = (
#       m0 * pmax(N - (cc - 1), 0)
#     ) / cc,
#     mj_smooth = (m0 * log1p(exp(N - cc))) / (log1p(1) * cc)
#   ) %>%
#   pivot_longer(
#     starts_with("mj_"),
#     names_sep = "_",
#     names_to = c(NA, "type"),
#     values_to = "mj"
#   ) %>%
#   mutate(type = fct_inorder(type)) %>%
#   identity() %>%
#   {
#     ggplot(.) +
#       aes(group = str_c(cc)) +
#       # aes(x = N / cc) +
#       aes(x = N) +
#       geom_line(aes(y = mj, color = type)) +
#       # geom_line(aes(y = mj_wedge, color = "wedge")) +
#       # geom_line(aes(y = mj_smooth, color = "smooth")) +
#       labs(y = "Baseline Migration Rate") +
#       labs(x = "N / K") +
#       geom_vline(aes(xintercept = cc / cc), linetype = "dotted") +
#       geom_hline(aes(yintercept = m0), linetype = "dotted") +
#       facet_grid(vars(type)) +
#       labs(color = NULL) +
#       ggpubr::theme_pubclean() +
#       NULL
#   }


expand_grid(
  m0 = 1 / (8 / 12),
  cc = 5,
  N = seq.default(0, cc * 2, by = 0.1),
  omega_wedge = 1,
  # omega_exp = log(cc),
  omega_exp = 1
) %>%
  mutate(
    mj_wedge = (
      m0 * pmax(N - (cc - 1 + omega_wedge), 0)
    ) / (cc + omega_wedge),
    mj_smooth = (m0 * log1p(exp(N - (cc + omega_exp)))) / (log1p(1) * (cc + omega_exp))
  ) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(x = N / cc) +
      geom_line(aes(y = mj_wedge, color = "wedge")) +
      geom_line(aes(y = mj_smooth, color = "smooth")) +
      labs(y = "Baseline Migration Rate") +
      labs(x = "N / K") +
      geom_vline(aes(xintercept = cc / cc), linetype = "dotted") +
      geom_hline(aes(yintercept = m0), linetype = "dotted") +
      labs(color = NULL) +
        ggpubr::theme_pubclean(15) +
          theme(legend.position = "bottom") +
      NULL
  }

