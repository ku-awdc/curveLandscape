
# library(sf)
library(units)
# read_rds("patches.rds")

# patches <- read_rds("patches.rds")
# patches$MainPatch <- as.integer(patches$MainPatch)
# patches$Centroid <- sf::st_centroid(patches$geometry)
# patches$Centroid %>% unclass %>% class

boar_patches <- read_rds("~/Documents/GitHub/hexscape/boar_centric_patches.rds")

boar_patches

boar_patches %>% glimpse()
boar_patches$MainPatch %>% table(useNA = "always")
boar_patches$MainPatch <- as.integer(boar_patches$MainPatch)


ggplot() +
  geom_sf(
    data = patches,
    aes(fill = Area)
  ) + 
  coord_sf(expand = FALSE)
