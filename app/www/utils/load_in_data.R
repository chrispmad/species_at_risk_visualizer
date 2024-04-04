# 1. Aquatic SAR (from Conservation Data Center's layer on BC Data Catalogue)
sar = read_sf('species_ecosystems_at_risk_publically_available_no_insects.gpkg') |>
  dplyr::filter(TAX_CLASS %in% c('amphibians','bivalves','Lampreys','ray-finned fishes')) |>
  st_transform(4326) |>
  dplyr::filter(!sf::st_is_empty(geom)) |>
  sf::st_make_valid()

# 2. Natural Resource Districts
nr_d = read_sf('boundaries/nr_districts.gpkg') |> st_transform(4326)

# 3. Natural Resource Regions
nr_r = read_sf('boundaries/nr_regions.gpkg') |> st_transform(4326)

# 4. DFO SARA-listed species (simplified geometries)
dfo_sara = read_sf('DFO_SARA_occ_data_QGIS_very_simplified.gpkg') |> st_transform(4326) |>
  dplyr::filter(!sf::st_is_empty(geom))

# 5. Federal critical habitat for SARA-listed species (cut to just BC)
crit_hab_bc = read_sf('federal_critical_habitat_just_BC.gpkg') |> st_transform(4326) |>
  dplyr::filter(!sf::st_is_empty(geom))

