devtools::load_all()
library(sf)
library(sfdep)
library(igraph)

habitat_map
habitat_map %>%
  st_as_sfc() %>%
  st_cast("POLYGON")

habitat_only <- habitat_map %>%
  dplyr::filter(Habitat != "Non") %>%
  identity() %>%
  {
    pmap(select(., Habitat), \(Habitat, geometry){
      geometry %>%
        st_sfc() %>%
        st_cast("POLYGON") %>%
        st_sf() %>%
        mutate(Habitat = Habitat)
    })
  } %>%
  bind_rows()

ggplot() +
  # geom_sf(data = habitat_only, aes(fill = Habitat)) + 

  theme_bw() +
  NULL

nb <- st_contiguity(st_make_valid(habitat_only$geometry), queen = TRUE)

edge_list <- tibble::tibble(id = 1:length(nb), nbs = nb) |>
  tidyr::unnest(nbs) |>
  filter(nbs != 0)

g <- graph_from_edgelist(as.matrix(edge_list), directed = FALSE)

n_contiguous <- lengths(neighborhood(g, order = Inf))

table(n_contiguous)
# n_contiguous
#   1   2   3   4   5   6   7   8  10  14  21  22  25
# 416  98  72  40  20  18  21  24  10  14  21  22  25


habitat_only <- habitat_only %>%
  group_by(Habitat) %>%
  group_map(\(habitat_df, key)  {
    dissolve_boundaries(habitat_df) %>%
      st_sf() %>%
      bind_cols(key)
  }) %>%
  bind_rows()


ggplot() +
  geom_sf(data = habitat_only, aes(fill = Habitat)) +
    geom_sf(data = habitat_only, aes(fill = Habitat)) +
  
  NULL

# dissolve_boundaries(x = habitat_only %>% filter(Habitat == "High"))


# neighborhood(g, order = Inf) %>%
#   get_habitat_island_ids()


# neighborhood(g, order = Inf) %>%
#   enframe() %>%
#   mutate(is_island = lengths(value) > 1) %>%
#   mutate(value = value %>% map(sort)) %>%
#   mutate(HabitatIslandID = {
#     HabitatIslandID <- name
#     HabitatIslandID[is_island] <- stringr::str_c(value[is_island]) %>% unique
#     HabitatIslandID
#   }) %>%
#   mutate(value = NULL)
#   print(n = Inf)

# neighborhood(g, order = Inf) %>%
#   head()

# neighborhood(g, order = Inf) %>%
#   # class() %>%
#   `[[`(1) %>% class()
# identity()

# methods(class = "igraph.vs")

# neighborhood


# neighborhood(g, order=Inf) %>%
#   enframe() %>%
#   # mutate(value = value %>% map(as_ids)) %>%
#   # unnest_wider(value, names_sep = "_")
#   mutate(value = value %>% map_chr(graph_id)) %>%
#   distinct(value)
#   # mutate(HabitatIslandID = order(value)) %>%
#   count(HabitatIslandID,sort = TRUE)

# V(make_neighborhood_graph(g, order = Inf))

# make_ego_graph(g, order = Inf)

# neighborhood(g, order = Inf) %>%
#   graph_from_data_frame(directed=FALSE)

# make_undirected_graph()
#   enframe() %>%
#   mutate(value = value %>% map(unclass)) %>%
#   distinct(value)
# # get.edge.ids(neighborhood(g, order = Inf))
# # make_ego_graph(g, order = Inf) %>%
# # edge_attr_names()
# # boar_centric_patches %>%
# #   count(MainPatch)
