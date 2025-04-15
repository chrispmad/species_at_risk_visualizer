library(tidyverse)

dat = data.frame(
  wb_name = c("Lower Arrow Lake","Lower Arrow Lake","Duncan Lake","Duncan Lake",
             "Kootenay River","Kootenay River","Kootenay River"),
  site = c("Syringa Creek Provincial Park boat launch","Scotties Marina",
          "Howser Recreation Site boat launch","Glacier Creek Regional Park boat launch",
          "Boat launch downstream of Taghum beach","Slocan Pool boat put-in",
          "Glade Regional Park boat launch"),
  easting = c(436696.784,439743.104,503987.708,505399.594,
             472108.968,462112.573,461367.488),
  northing = c(5465740.587,5465897.517,5572243.066,
              5569640.24,5481576.761,5476421.594,5470801.309)
)

sar = sf::read_sf("C:/Users/CMADSEN/OneDrive - Government of BC/data/CNF/DFO_SARA_occ_data_QGIS_simplified_SouthCoastBullTrout.gpkg")

sar = sf::st_make_valid(sar)

# Test for overlaps.# Tesarst for overlaps.

dat_sf = dat |>
  sf::st_as_sf(coords = c("easting","northing"), crs = 32611) |>
  sf::st_transform(4326)

bc = bcmaps::bc_bound()

ggplot() + geom_sf(data = bcmaps::bc_bound()) + geom_sf(data = dat_sf)

dat_sar_join = dat_sf |>
  sf::st_join(sar |> dplyr::select(Common_Name_EN, Population_EN)) |>
  dplyr::mutate(latitude = sf::st_coordinates(geometry)[,2],
                longitude = sf::st_coordinates(geometry)[,1]) |>
  sf::st_drop_geometry()

dat_sar_join = dat_sar_join |>
  dplyr::group_by(wb_name, site, latitude, longitude) |>
  dplyr::reframe(SAR_names_and_pops = paste0(Common_Name_EN, " (",Population_EN,")")) |>
  dplyr::group_by(wb_name, site, latitude, longitude) |>
  dplyr::reframe(SAR_names_and_pops = paste0(SAR_names_and_pops, collapse = ", "))

# Add additional overlap test - find nearest feature.
nonoverlapping_dat_sf = dat_sar_join |>
  dplyr::filter(SAR_names_and_pops == "NA (NA)") |>
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

sar_w_rn = sar |>
  dplyr::mutate(row_id = row_number()) |>
  dplyr::select(row_id, dplyr::everything())

nearest_features = nonoverlapping_dat_sf |>
  sf::st_join(
    sar_w_rn |>
      dplyr::select(row_id, Common_Name_EN, Population_EN),
    sf::st_nearest_feature
  )

nonoverlapping_dat_sf$distance_to_overlap = 0
nonoverlapping_dat_sf[1,]$distance_to_overlap = as.numeric(sf::st_distance(nonoverlapping_dat_sf[1,], sar_w_rn[541,]))
nonoverlapping_dat_sf[2,]$distance_to_overlap = as.numeric(sf::st_distance(nonoverlapping_dat_sf[2,], sar_w_rn[3492,]))
nonoverlapping_dat_sf[1,]$SAR_names_and_pops = paste0(sar_w_rn[541,]$Common_Name_EN, " (",sar_w_rn[541,]$Population_EN,")")
nonoverlapping_dat_sf[2,]$SAR_names_and_pops = paste0(sar_w_rn[3492,]$Common_Name_EN, " (",sar_w_rn[3492,]$Population_EN,")")

dat_final = dat_sar_join |>
  dplyr::mutate(distance_to_overlap = 0) |>
  dplyr::filter(SAR_names_and_pops != "NA (NA)") |>
  dplyr::bind_rows(nonoverlapping_dat_sf |>
                     dplyr::mutate(latitude = sf::st_coordinates(geometry)[,2],
                                   longitude = sf::st_coordinates(geometry)[,1]) |>
                     sf::st_drop_geometry())

library(leaflet)

sar_in_context = sar_w_rn |>
  sf::st_cast("MULTIPOLYGON") |>
  sf::st_intersection(
    sf::st_buffer(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(dat_sf))),100)
  )

my_tbls = leafpop::popupTable(
  dat_final |>
    dplyr::select(wb_name,site,SAR_names_and_pops,distance_to_overlap)
)

sar_colours = leaflet::colorFactor(palette = "Spectral", domain = sar_in_context$Common_Name_EN)

l = leaflet() |>
  addTiles() |>
  addLayersControl(position = 'bottomleft',
                   overlayGroups = unique(sar_in_context$Common_Name_EN),
                   options = layersControlOptions(collapsed = F)) |>
  addMarkers(
    data = dat_final |>
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326),
    label = lapply(my_tbls, htmltools::HTML)
  ) |>
  leaflet::addScaleBar('bottomright') |>
  leaflet::addLegend('topright', pal = sar_colours, values = unique(sar_in_context$Common_Name_EN))

l

for(i in 1:length(unique(sar_in_context$Common_Name_EN))){
  species_to_add = unique(sar_in_context$Common_Name_EN)[i]
  print(species_to_add)
  l = l |>
    addPolygons(
      data = sar_in_context |> dplyr::filter(Common_Name_EN == species_to_add),
      fillColor = ~sar_colours(Common_Name_EN),
      group = species_to_add,
      weight = 1,
      color = 'black',
      label = ~paste0(Common_Name_EN, " (",Population_EN,"; row: ",row_id,")")
    )
}

l

openxlsx::write.xlsx(dat_final, "one_off_sideproject_scripts/output/sar_overlap_with_Eleanors_sites.xlsx")


