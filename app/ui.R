
library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)
library(shinyWidgets)

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
  choices = c("All"),
  options = list(
    style = 'btn-success',
    size = 6,
    `actions-box` = TRUE,
    `live-search` = TRUE
  ),
  multiple = T
)

# Summary cards
selected_boundary_card = card(
  textOutput('selected_boundary'),
  theme_color = 'success'
)

number_unique_SAR_species_card = card(
  p('Unique Aquatic SAR species\n(CDC and DFO SARA)'),
  h3(textOutput('sar_unique_species')),
  class = "bg-secondary"
)

sq_km_critical_habitat = card(
  card_title('Critical Habitat'),
  h3(uiOutput('km_2_crit_hab')),
  class = "skyblue-box"
)

# card(
#   card_title('Test'),
#   HTML('<h5>Hello<sup>2</sup></h5>'),
#   class = 'bg-info'
# )

reset_shape_selection_button = actionButton('reset_shape_sel','Reset',class = 'btn-primary')

shape_selection_feedback = bslib::layout_columns(
  col_widths = c(8,4),
  div(
    shape_specific_selector,
    style = 'margin-top:-1.5rem;'
      ),
  reset_shape_selection_button
)

# Summary card combo
summary_cards = layout_column_wrap(
  width = 1/2,
  number_unique_SAR_species_card,
  sq_km_critical_habitat
)

sidebar = sidebar(
  width = '30%',
  h4('Species Selection', style = 'margin-top:-2rem;margin-bottom:-2rem;align-self:center;'),
  species_selector,
  h4('Spatial Scope', style = 'margin-bottom:-0.5rem;align-self:center;'),
  shape_type_selector,
  shape_selection_feedback,
  h4('Summaries', style = 'align-self:center;'),
  summary_cards
)

main = tagList(
  leafletOutput('my_leaf'),
  div(id = 'loading_stuff',
      div(
        id = 'loading_fish',
        class = 'swimming-fish'
      ),
      div(
        p("Loading...",
          class = 'loading_text')
      )
  )
)

details = card(
  h5("Metadata and Description of Data"),
  p("1. CDC Aquatic SAR"),
  p(HTML("This dataset comes from the <a href='https://catalogue.data.gov.bc.ca/dataset/species-and-ecosystems-at-risk-publicly-available-occurrences-cdc'>BC Data Catalogue</a>. It is maintained up-to-date as data are added to the CDC layer")),
  p("2. DFO SARA ('Distributions')"),
  p(HTML("This dataset was accessed from <a href='https://open.canada.ca/data/en/dataset/e0fabad5-9379-4077-87b9-5705f28c490b'>Open Canada</a> in January of 2024 to download federal-level data for species occurrences of SARA-listed species.")),
  p("3. Critical Habitat"),
  p(HTML("This dataset was accessed from <a href='https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c'>Open Canada</a> in January of 2024 to download federal-level critical habitat for SARA-listed species."))
)

ui = page_navbar(
  title = 'Species-at-Risk Visualizer',
  includeCSS('www/my_style.css'),
  includeScript('www/my_js.js'),
  shinyjs::useShinyjs(),
  theme = bs_theme(bootswatch = 'flatly',
                   secondary = '#dbc8ea'),
  sidebar = sidebar,
  nav_panel(
    title = 'Tool',
    main,
  ),
  nav_panel(
    title = "Details",
    details
  ),
  nav_item(
    div(
      class = 'bc_logo'
    )
  )
)
