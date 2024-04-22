
library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)
library(shinyWidgets)

source('modules/leaflet_map_mod.R')
source('modules/summary_boxes_mod.R')
source('www/utils/walkthough_content.R')

# Select which kind of boundaries to plot (regions or districts)
shape_type_selector = shinyWidgets::pickerInput(
  'shape_type_input',
  'Administrative Boundaries',
  choices = c(
    "Natural Resource Regions" = 'nr_r',
    "Natural Resource Districts" = 'nr_d'),
  options = list(
    style = "btn-primary")
  )

# Select ONE of the shapes
shape_specific_selector = shinyWidgets::pickerInput(
  'shape_select_input',
  '',
  choices = c('Province'),
  options = list(
    style = "btn-primary",
    `live-search` = TRUE))

# Select one or more species to focus on in the map
species_selector = shinyWidgets::pickerInput(
  'species_select_input',
  '',
  choices = 'None',
  options = list(
    style = 'btn-success',
    size = 6,
    `live-search` = TRUE
  ),
  multiple = F
)

species_selector_bit = bslib::layout_columns(
  col_widths = c(9,3),
  species_selector,
  actionButton(
    'reset_species_selection',
    'Reset',
    class = 'btn-success',
    style = 'margin-top:3.5vh;'
  )
)

reset_shape_selection_button = actionButton('reset_shape_sel','Reset',class = 'btn-primary')

shape_selection_feedback = bslib::layout_columns(
  col_widths = c(8,4),
  div(
    shape_specific_selector,
    style = 'margin-top:-1.5rem;'
      ),
  reset_shape_selection_button
)

# Download buttons
cdc_occ_data_xlsx_download_button = downloadButton(
  'cdc_occ_data_xlsx_download_button',
  shiny::HTML('Spreadsheet'),
  style = 'padding:10px;display:grid;height:9vh;',
  class = "btn-primary"
)

cdc_occ_data_gpkg_download_button = downloadButton(
  'cdc_occ_data_gpkg_download_button',
  '\n.GPKG',
  style = 'padding:10px;display:grid;height:9vh;',
  class = "btn-success"
)

dfo_occ_data_xlsx_download_button = downloadButton(
  'dfo_occ_data_xlsx_download_button',
  shiny::HTML('Spreadsheet'),
  style = 'padding:10px;display:grid;height:9vh;',
  class = "btn-primary"
)

dfo_occ_data_gpkg_download_button = downloadButton(
  'dfo_occ_data_gpkg_download_button',
  '\n.GPKG',
  style = 'padding:10px;display:grid;height:9vh;',
  class = "btn-success"
)

dfo_crithab_xlsx_download_button = downloadButton(
  'dfo_crithab_xlsx_download_button',
  shiny::HTML('Spreadsheet'),
  style = 'padding:10px;display:grid;height:9vh;',
  class = "btn-primary"
)

dfo_crithab_gpkg_download_button = downloadButton(
  'dfo_crithab_gpkg_download_button',
  '\n.GPKG',
  style = 'padding:10px;display:grid;height:9vh;',
  class = "btn-success"
)

download_buttons = tagList(
  h5("Data Downloads", style = 'margin-bottom:-1rem;'),
  p("CDC", style = 'margin-bottom:-0.75rem;text-align:center;font-size:large;'),
  # cdc_occ_data_xlsx_download_button,
  # cdc_occ_data_gpkg_download_button,
  layout_column_wrap(
    1/2,
    cdc_occ_data_xlsx_download_button,
    cdc_occ_data_gpkg_download_button
  ),
  p("DFO Occurrence Data", style = 'margin-bottom:-0.75rem;text-align:center;font-size:large;'),
  # dfo_occ_data_xlsx_download_button,
  # dfo_occ_data_gpkg_download_button,
  layout_column_wrap(
    1/2,
    dfo_occ_data_xlsx_download_button,
    dfo_occ_data_gpkg_download_button
  ),
  p("DFO Critical Habitat", style = 'margin-bottom:-0.75rem;text-align:center;font-size:large;'),
  # dfo_crithab_xlsx_download_button,
  # dfo_crithab_gpkg_download_button
  layout_column_wrap(
    1/2,
    dfo_crithab_xlsx_download_button,
    dfo_crithab_gpkg_download_button
  )
)

# Summary card combo
summary_cards = summary_boxes_mod_UI('summary_boxes')

sidebar = sidebar(
  width = '30%',
  h4('Species Highlight', style = 'margin-top:-2rem;margin-bottom:-2rem;align-self:center;'),
  species_selector_bit,
  h4('Spatial Scope', style = 'margin-bottom:-0.5rem;align-self:center;'),
  shape_type_selector,
  shape_selection_feedback,
  h4('Summaries', style = 'align-self:center;margin-top:-1.5rem;margin-bottom:-0.5rem;'),
  summary_cards,
  download_buttons
)

main = leaflet_map_mod_UI('map')

metadata = card(
  h3("Metadata and Description of Data"),
  h5("Conservation Data Center Aquatic SAR"),
  p(HTML("1. This dataset comes from the <a href='https://catalogue.data.gov.bc.ca/dataset/species-and-ecosystems-at-risk-publicly-available-occurrences-cdc'>BC Data Catalogue</a>. It is maintained up-to-date as data are added to the CDC layer")),
  h5("Department of Fisheries and Oceans Canada (DFO) Species-at-risk-act listed species"),
  p(HTML("1. Occurrence Data: this dataset was accessed from <a href='https://open.canada.ca/data/en/dataset/e0fabad5-9379-4077-87b9-5705f28c490b'>Open Canada</a> in January of 2024 to download federal-level data for species occurrences of SARA-listed species.")),
  p(HTML("2. Critical Habitat Polygons: This dataset was accessed from <a href='https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c'>Open Canada</a> in January of 2024 to download federal-level critical habitat polygons for SARA-listed species.")),
  p("\nNote: DFO Critical Habitat for SARA-listed species displayed in this tool is only a subset of such habitat: habitat characteristics have been identified for many species (e.g. riffles in streams) while spatially delineated polygons may not yet exist.")
)

ui = page_navbar(
  title = 'Species-at-Risk Data Visualizer',
  includeCSS('www/my_style.css'),
  includeScript('www/my_js.js'),
  shinyjs::useShinyjs(),
  theme = bs_theme(bootswatch = 'flatly',
                   secondary = '#dbc8ea'),
  sidebar = sidebar,
  nav_panel(
    title = 'Tool',
    main,
    walkthrough_boxes
  ),
  nav_panel(
    title = "Metadata",
    metadata
  ),
  nav_item(
    actionButton(
      'click_instr',
      "How to Use Tool",
      class = 'instructions_button'
    )
  ),
  nav_item(
    p(paste0('(Data last updated in ',
             'Jan. 2024',
             # format(file.mtime('www/federal_critical_habitat_just_BC.gpkg'),'%d-%b-%Y'),
             ')'),
      style = 'position:relative; top: 1vh; left:40vh; color:white;')
  ),
  nav_item(
    div(
      class = 'bc_logo'
    )
  )
)
