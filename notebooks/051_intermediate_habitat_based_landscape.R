devtools::load_all()
library(sf)
library(sfdep)
library(igraph)

habitat_map

habitat_only <- habitat_map %>% 
  filter(Habitat != "Non") %>% 
  st_cast("POLYGON") %>% 
  rename(TotalArea = Area, TotalArea_simplified = Area_simplified)
  # sf:::group_split.sf(geometry)
  # unnest(geometry)

nb <- st_contiguity(st_make_valid(habitat_only$geometry))

edge_list <- tibble::tibble(id = 1:length(nb), nbs = nb) |> 
  tidyr::unnest(nbs) |> 
  filter(nbs != 0)

g <- graph_from_edgelist(as.matrix(edge_list), FALSE)

n_contiguous <- lengths(neighborhood(g, order = Inf))

table(n_contiguous)
#> n_contiguous
#>   1   2   3   4 
#> 781 140  33   8


# boar_centric_patches %>% 
#   count(MainPatch)
