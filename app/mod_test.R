#minimal testing group for modules.

library(shiny)
library(bslib)

sel_shape = reactive('Province')
sar_sp = reactive(sf::read_sf('www/species_ecosystems_at_risk_publically_available_no_insects.gpkg'))
dfo_sara_sp = reactive(sf::read_sf('www/DFO_SARA_occ_data_QGIS_very_simplified.gpkg'))
crit_hab_bc_sp = reactive(sf::read_sf('www/federal_critical_habitat_just_BC.gpkg'))
shape_type_input = reactive('nr_r')

source('modules/summary_boxes_mod.R')

ui <- fluidPage(
  summary_boxes_mod_UI('test')
)

server <- function(input, output, session) {
  summary_boxes_mod_Server('test',
                           sel_shape,
                           dfo_sara_sp,
                           crit_hab_bc_sp,
                           shape_type_input)
}

shinyApp(ui, server)

