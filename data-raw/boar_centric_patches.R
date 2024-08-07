## code to prepare `boar_centric_patches` dataset goes here

#' Hacky retrieval from `notebooks/044_copy_hexscape_boar_centric_script.R`
boar_centric_patches <- new_patches

usethis::use_data(boar_centric_patches, overwrite = TRUE)
