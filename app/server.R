
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

  source('utils/load_in_data.R')

  # =======================
  #  Update Inputs with Data
  # =======================

  # Clean species names in sar, dfo_sara, and crit_hab_bc.
  sar = sar |>
    dplyr::rename(eng_name = ENG_NAME,
                  sci_name = SCI_NAME) |>
    dplyr::mutate(sci_name = str_remove_all(sci_name, ' (pop|sp)\\..*')) |>
    dplyr::mutate(eng_name = str_remove_all(eng_name, '(,| \\(| -).*')) |>
    dplyr::mutate(eng_name = case_when(
      sci_name == 'Gasterosteus aculeatus' ~ 'Threespine Stickleback',
      sci_name == 'Gasterosteus' ~ 'Stickleback',
      T ~ eng_name
    )) |>
    dplyr::mutate(eng_name = stringr::str_squish(eng_name),
                  sci_name = stringr::str_squish(sci_name))

  # Any polygons of SAR with completely duplicated geometries should
  # have ENG_NAME pasted together for a label column.
  sar = sar |>
    dplyr::group_by(geom) |>
    dplyr::mutate(eng_name_label = paste0(eng_name, collapse = ', ')) |>
    dplyr::ungroup()

  dfo_sara = dfo_sara |>
    dplyr::rename(eng_name = Common_Name_EN,
                  sci_name = Scientific_Name) |>
    dplyr::mutate(sci_name = str_remove_all(sci_name, ' (pop|sp)\\..*')) |>
    dplyr::mutate(eng_name = str_remove_all(eng_name, '(,| \\(| -).*')) |>
    dplyr::mutate(eng_name = case_when(
      sci_name == 'Gasterosteus aculeatus' ~ 'Threespine Stickleback',
      sci_name == 'Gasterosteus' ~ 'Stickleback',
      T ~ eng_name
    )) |>
    dplyr::mutate(eng_name = stringr::str_squish(eng_name),
                  sci_name = stringr::str_squish(sci_name))

  crit_hab_bc = crit_hab_bc |>
    dplyr::rename(eng_name = Common_Name_EN,
                  sci_name = Scientific_Name) |>
    dplyr::mutate(sci_name = str_remove_all(sci_name, ' (pop|sp)\\..*')) |>
    dplyr::mutate(eng_name = str_remove_all(eng_name, '(,| \\(| -).*')) |>
    dplyr::mutate(eng_name = case_when(
      sci_name == 'Gasterosteus aculeatus' ~ 'Threespine Stickleback',
      sci_name == 'Gasterosteus' ~ 'Stickleback',
      T ~ eng_name
    )) |>
    dplyr::filter(eng_name != '') |>
    dplyr::mutate(eng_name = stringr::str_squish(eng_name),
                  sci_name = stringr::str_squish(sci_name))

  species_list = dplyr::bind_rows(
    sar |> dplyr::select(eng_name,
                         sci_name),
    dfo_sara |> dplyr::select(eng_name,
                              sci_name),
    crit_hab_bc|> dplyr::select(eng_name,
                                sci_name)
  ) |>
    sf::st_drop_geometry() |>
    # # Remove instances in species name that include 'pop.' or 'sp.'
    # dplyr::mutate(sci_name = str_remove_all(sci_name, ' (pop|sp)\\..*')) |>
    # # Remove instances in species common name of commas, parentheses or hyphens
    # dplyr::mutate(eng_name = str_remove_all(eng_name, '(,| \\(| -).*')) |>
    # dplyr::mutate(eng_name = case_when(
    #   sci_name == 'Gasterosteus aculeatus' ~ 'Threespine Stickleback',
    #   sci_name == 'Gasterosteus' ~ 'Stickleback',
    #   T ~ eng_name
    # )) |>
    dplyr::filter(!eng_name %in% c("Inconnu","Killer Whale")) |>
    dplyr::distinct() |>
    dplyr::filter(!duplicated(eng_name)) |>
    dplyr::arrange(eng_name)

  observe({
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'species_select_input',
      choices = c('None',species_list$eng_name)
    )
  })

  # If clicked on 'Reset' button for species selection, reset to 'None'
  observeEvent(input$reset_species_selection, {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'species_select_input',
      selected = 'None'
    )
  })

  observe({
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'shape_select_input',
      choices = c("Province",shapes()$ADMIN_NAME),
      selected = shapes()$ADMIN_NAME
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

  # output$selected_boundary = renderText({
  #   if(sel_shape() == 'Province'){
  #     paste0("Province")
  #   } else {
  #     paste0("Focus: ",sel_shape())
  #   }
  # })

  # Listen for mouse click on map; identify which shape has been clicked
  # and assign that to the reactiveVal tracking selected shape.
  observe({
    req(!is.null(map_click_coords()))

    coords_df = as.data.frame(map_click_coords())

    coords_sf = sf::st_as_sf(coords_df, coords = c("lng","lat"), crs = 4326) |>
      dplyr::mutate(in_click = TRUE)

    clicked_shape_name = shapes() |>
      sf::st_join(coords_sf) |>
      dplyr::filter(in_click == TRUE) |>
      dplyr::pull(ADMIN_NAME)

    if(length(clicked_shape_name) > 0){

      sel_shape(clicked_shape_name)

      # When the user clicks on the map, update the shape selector UI
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = 'shape_select_input',
        selected = clicked_shape_name
      )

    }
  })

  # React to user picking a specific shape from the dropdown.
  observeEvent(input$shape_select_input, {
    sel_shape(input$shape_select_input)
  })


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
    if(input$species_select_input == 'None') return(sar)
    # Otherwise...
    sar |> dplyr::filter(eng_name %in% input$species_select_input)
  })

  dfo_sara_sp = reactive({
    if(input$species_select_input == 'None') return(dfo_sara)
    # Otherwise...
    dfo_sara |> dplyr::filter(eng_name %in% input$species_select_input)
  })

  crit_hab_bc_sp = reactive({
    if(input$species_select_input == 'None') return(crit_hab_bc)
    # Otherwise...
    crit_hab_bc |> dplyr::filter(eng_name %in% input$species_select_input)
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

  observe({
    print(input$species_select_input)
  })

  leaflet_mod_output <- leaflet_map_mod_Server(
    'map',
    sar, dfo_sara, crit_hab_bc, sel_type, sel_shape,
    reactive(input$shape_type_input), shapes, reactive(input$species_select_input),
    sar_sp, dfo_sara_sp, crit_hab_bc_sp)

  # Pull out the reactives from the module
  my_leaf_shape_click = leaflet_mod_output$clicked_shape

  map_click_coords = leaflet_mod_output$map_click_coords

  # Walkthrough instructions
  observeEvent(input$click_instr, {
    shinyjs::show(id = 'walkthrough-1')
  })

  # If the user clicks 'Close', hide all the walkthrough elements
  observeEvent(c(input$close_tips,input$close_tips_2,input$close_tips_3,input$close_tips_4), {
    shinyjs::hide(id = 'walkthrough-1')
    shinyjs::hide(id = 'walkthrough-2')
    shinyjs::hide(id = 'walkthrough-3')
    shinyjs::hide(id = 'walkthrough-4')
  })

  observeEvent(input$wt_1_to_2, {
    shinyjs::hide(id = 'walkthrough-1')
    shinyjs::show(id = 'walkthrough-2')
  })

  observeEvent(input$wt_2_to_3, {
    shinyjs::hide(id = 'walkthrough-2')
    shinyjs::show(id = 'walkthrough-3')
  })

  observeEvent(input$wt_2_to_1, {
    shinyjs::hide(id = 'walkthrough-2')
    shinyjs::show(id = 'walkthrough-1')
  })

  observeEvent(input$wt_3_to_4, {
    shinyjs::hide(id = 'walkthrough-3')
    shinyjs::show(id = 'walkthrough-4')
  })

  observeEvent(input$wt_3_to_2, {
    shinyjs::hide(id = 'walkthrough-3')
    shinyjs::show(id = 'walkthrough-2')
  })

  observeEvent(input$wt_4_to_3, {
    shinyjs::hide(id = 'walkthrough-4')
    shinyjs::show(id = 'walkthrough-3')
  })
}

