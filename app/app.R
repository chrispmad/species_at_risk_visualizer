
source('server.R')
source('ui.R')

shinyApp(ui, server)

# sar = sf::read_sf("C:/Users/CMADSEN/OneDrive - Government of BC/data/CNF/dfo_sara_and_crit_hab_bulltrout_and_sockeye_data.gpkg")
#
# sar2 = sf::read_sf("C:/Users/CMADSEN/OneDrive - Government of BC/data/CNF/DFO_SARA_occ_data_QGIS_simplified.gpkg")
#
# ggplot() + geom_sf(data = sar)
# ggplot() + geom_sf(data = sar2)
#
# # Add bull trout polygon.
# sar2 = sar2 |>
#   dplyr::bind_rows(
#     sar |>
#       dplyr::filter(Common_Name_EN == 'Bull Trout', Population_EN == 'South Coast')
#   )
#
# sf::write_sf(sar2, "C:/Users/CMADSEN/OneDrive - Government of BC/data/CNF/DFO_SARA_occ_data_QGIS_simplified_SouthCoastBullTrout.gpkg")
#
