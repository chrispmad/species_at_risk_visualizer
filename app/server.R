
library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)
library(shinyWidgets)

server <- function(input, output, session) {

  if(!str_detect(getwd(),'/www$')) {
    setwd(paste0(getwd(),'/www/'))
  }

  # =======================
  #   Load in Data
  # =======================

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

  # =======================
  #  Update Inputs with Data
  # =======================

  species_list = dplyr::bind_rows(
    sar |> dplyr::select(eng_name = ENG_NAME,
                         sci_name = SCI_NAME),
    dfo_sara |> dplyr::select(eng_name = Common_Name_EN,
                              sci_name = Scientific_Name),
    crit_hab_bc|> dplyr::select(eng_name = Common_Name_EN,
                                sci_name = Scientific_Name)
  ) |>
    sf::st_drop_geometry() |>
    # Remove instances in species name that include 'pop.' or 'sp.'
    dplyr::mutate(sci_name = str_remove_all(sci_name, ' (pop|sp)\\..*')) |>
    # Remove instances in species common name of commas, parentheses or hyphens
    dplyr::mutate(eng_name = str_remove_all(eng_name, '(,| \\(| -).*')) |>
    dplyr::mutate(eng_name = case_when(
      sci_name == 'Gasterosteus aculeatus' ~ 'Threespine Stickleback',
      sci_name == 'Gasterosteus' ~ 'Stickleback',
      T ~ eng_name
    )) |>
    dplyr::distinct() |>
    dplyr::arrange(eng_name)

  observe({
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'species_select_input',
      choices = c(species_list$eng_name)
    )
  })

  observe({
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'shape_select_input',
      choices = c("Province",shapes()$ADMIN_NAME),
      selected = "Province"
    )
  })

  # =======================
  #  Reactives
  # =======================

  # Which delineating shape to use for map.
  shapes = reactive({
    dat = switch(input$shape_type_input,
           nr_d = nr_d,
           nr_r = nr_r)

    if(input$shape_type_input == 'nr_d'){
      dat = dat |>
        dplyr::select(ADMIN_NAME = DISTRICT_NAME)
    }
    if(input$shape_type_input == 'nr_r'){
      dat = dat |>
        dplyr::select(ADMIN_NAME = REGION_NAME)
    }
    dat
  })

  # Set up reactiveVal for selected shape ADMIN NAME.
  sel_shape = reactiveVal('Province')
  sel_type = reactiveVal('nr_r')

  output$selected_boundary = renderText({
    if(sel_shape() == 'Province'){
      paste0("Province")
    } else {
      paste0("Focus: ",sel_shape())
    }
  })

  # Get click from leaflet map to inform the sel_shape() reactiveVal
  observeEvent(my_leaf_shape_click(), {
    req(!is.null(my_leaf_shape_click()$id))
    if(stringr::str_detect(my_leaf_shape_click()$id,'Natural Resource')){

      sel_shape(my_leaf_shape_click()$id)

      # Also update the dropdown input for specific shape selection
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = 'shape_select_input',
        selected = sel_shape()
      )
    }
  })

  # React to user picking a specific shape from the dropdown.
  observeEvent(input$shape_select_input, {
    req(input$shape_select_input != 'Province')
    sel_shape(input$shape_select_input)
  })

  # Reset sel_shape() reactiveVal when clicked.
  observeEvent(input$reset_shape_sel, {

    sel_shape('Province')

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'shape_select_input',
      selected = 'Province'
    )

  })

  # Reset sel_shape() when user switches between administrative boundary shapes.
  observeEvent(input$shape_type_input, {

    sel_shape('Province')

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'shape_select_input',
      selected = 'Province'
    )

    sel_type(input$shape_type_input)

  })

  # Species-filtered versions of SAR, DFO, and critical habitat
  sar_sp = reactive({
    if(is.null(input$species_select_input)) return(sar)
    # Otherwise...
    sar |> dplyr::filter(ENG_NAME %in% input$species_select_input)
  })

  dfo_sara_sp = reactive({
    if(is.null(input$species_select_input)) return(dfo_sara)
    # Otherwise...
    dfo_sara |> dplyr::filter(Common_Name_EN %in% input$species_select_input)
  })

  crit_hab_bc_sp = reactive({
    if(is.null(input$species_select_input)) return(crit_hab_bc)
    # Otherwise...
    crit_hab_bc |> dplyr::filter(Common_Name_EN %in% input$species_select_input)
  })

  summary_boxes_mod_Server(
    'summary_boxes',
    reactive(sel_shape()),
    sar_sp,
    dfo_sara_sp,
    crit_hab_bc_sp,
    reactive(input$shape_type_input),
    shapes
  )

  # =======================
  #  Renderings
  # =======================

  my_leaf_shape_click <- leaflet_map_mod_Server(
    'map',
    sar, dfo_sara, crit_hab_bc, sel_type, sel_shape,
    reactive(input$shape_type_input), shapes, reactive(input$species_select_input),
    sar_sp, dfo_sara_sp, crit_hab_bc_sp)

}

