devtools::load_all()

all_landscapes %>%
  mutate(
    cc = grid %>% map(. %>% st_drop_geometry() %>% `[`("Capacity"))
  ) %>%
  select(-grid) %>%
  unnest(cc) %>%
  mutate(landscape_type = fct_inorder(landscape_type)) %>%
  {
    ggplot(.) +
      aes(group = str_c(landscape_type, set_patch_size)) +
      # geom_density(aes(Capacity, color = factor(set_patch_size))) +
      # stat_density(aes(Capacity, color = factor(set_patch_size)), geom = "line", bounds = c(0, Inf)) +
      stat_bin(aes(Capacity,y=after_stat(density),  color = fct(set_patch_size %>% str_c(" km²"))), 
        binwidth = 0.5, geom = "step", 
        pad = TRUE,
      ) +
      facet_wrap(
        # set_patch_size ~ landscape_type,
        landscape_type ~ .,
        scales = "free",
        ncol = 1,
        strip.position = "right",
        # cols = vars(set_patch_size),
        # rows = vars(landscape_type),
        labeller = labeller(landscape_type = c(naive = "grid-based", habic = "habitat-based"))
      ) +
      labs(color = "Patch-sizes") +
      labs(x = "Carrying capacity") +
      labs(y = "Proportion of Habitable Patches") +
      ggpubr::theme_pubclean(15) +
      NULL
  }

fs::dir_create("figures")
ggsave(
  filename = "figures/070_carrying_capacity_of_landscapes.svg",
  device = svglite::svglite,
  # scaling = 3,
  scale = 1.9
  # width = 2*4.27,
  # height = 2.5
)
