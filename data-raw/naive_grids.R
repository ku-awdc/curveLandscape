## code to prepare `naive_grids` dataset goes here
devtools::load_all()

patch_sizes <- c(1, 2, 5, 10, 25) # km^2

naive_grids <- map(patch_sizes, \(patch_sizes_in_km2) create_naive_grid(patch_sizes_in_km2, square = FALSE))

dk_patches <- hexscape::load_map("DK032")


# ggplot() +
#   geom_sf(data = dk_patches, fill = NA, linetype="dashed") +
#   geom_sf(
#     data =
#       naive_grids[[4]], fill = NA
#   ) +
#   theme_light(15) +
#   NULL
naive_grids %>% lengths()
naive_grids %>% map(nrow)

# map(naive_grids, \(naive_grid) {
#   p_naive_grid <- ggplot() +
#     geom_sf(data = dk_patches, fill = NA, linetype="dashed") +
#     geom_sf(data = naive_grid, fill = NA) +
#     theme_light(15) +
#     NULL
# }) %>%
#   walk(print)

ggplot() +
  geom_sf(data = dk_patches, fill = NA, linetype = "dashed") +
  geom_sf(data = habitat_map, aes(fill = Habitat)) +
  scale_habitat_fill_manual() +
  geom_sf(
    data =
      naive_grids[[1]], fill = NA
  ) +
  labs(caption = "Patch size: 1 km^2") +
  coord_sf(expand = FALSE) +
  theme_light(15) +
  theme(legend.position = "bottom") +
  NULL

ggplot() +
  geom_sf(data = dk_patches, fill = NA, linetype = "dashed") +
  geom_sf(data = habitat_map, aes(fill = Habitat)) +
  scale_habitat_fill_manual() +
  geom_sf(
    data =
      naive_grids[[2]], fill = NA
  ) +
  labs(caption = "Patch size: 2 km^2") +
  coord_sf(expand = FALSE) +
  theme_light(15) +
  theme(legend.position = "bottom") +
  NULL

ggplot() +
  geom_sf(data = dk_patches, fill = NA, linetype = "dashed") +
  geom_sf(data = habitat_map, aes(fill = Habitat)) +
  scale_habitat_fill_manual() +
  geom_sf(
    data =
      naive_grids[[3]], fill = NA
  ) +
  labs(caption = "Patch size: 5 km^2") +
  coord_sf(expand = FALSE) +
  theme_light(base_size = 15) +
  theme(legend.position = "bottom") +
  NULL

naive_grids <- structure(
  list(
    set_patch_size = patch_sizes,
    naive_grids = naive_grids
  )
)

usethis::use_data(naive_grids, overwrite = TRUE)
