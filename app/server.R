
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
  observeEvent(input$my_leaf_shape_click, {
    req(!is.null(input$my_leaf_shape_click$id))
    if(stringr::str_detect(input$my_leaf_shape_click$id,'Natural Resource')){

      sel_shape(input$my_leaf_shape_click$id)

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

  output$sar_unique_species = renderText({
    if(sel_shape() == 'Province'){
      sar_output = sar_sp()$ENG_NAME
      dfo_sara_output = dfo_sara_sp()$Common_Name_EN
    } else {
      if(input$shape_type_input == 'nr_r'){

        sar_output = sar_sp() |>
          sf::st_drop_geometry() |>
          dplyr::filter(stringr::str_detect(REGION_NAME,sel_shape())) |>
          dplyr::pull(ENG_NAME)

        dfo_sara_output = dfo_sara_sp() |>
          sf::st_drop_geometry() |>
          dplyr::filter(stringr::str_detect(REGION_NAME,sel_shape())) |>
          dplyr::pull(Common_Name_EN)

      } else {

        sar_output = sar_sp() |>
          sf::st_drop_geometry() |>
          dplyr::filter(stringr::str_detect(DISTRICT_NAME,sel_shape())) |>
          dplyr::pull(ENG_NAME)

        dfo_sara_output = dfo_sara_sp() |>
          sf::st_drop_geometry() |>
          dplyr::filter(stringr::str_detect(DISTRICT_NAME,sel_shape())) |>
          dplyr::pull(Common_Name_EN)

      }
    }
    length(unique(c(sar_output, dfo_sara_output)))
  })

  output$km_2_crit_hab = renderUI({
    if(sel_shape() == 'Province'){
      output = round(as.numeric(sum(st_area(crit_hab_bc_sp())))/1000,0)
    } else {
      if(input$shape_type_input == 'nr_r'){
        crit_hab_in_area = crit_hab_bc_sp() |>
          dplyr::filter(stringr::str_detect(REGION_NAME,sel_shape()))

        crit_hab_in_area_intersected = sf::st_intersection(crit_hab_in_area, shapes()[shapes()$ADMIN_NAME == sel_shape(),])

        output = sf::st_area(crit_hab_in_area_intersected)
      } else {
        crit_hab_in_area = crit_hab_bc_sp() |>
          dplyr::filter(stringr::str_detect(DISTRICT_NAME,sel_shape()))

        crit_hab_in_area_intersected = sf::st_intersection(crit_hab_in_area, shapes()[shapes()$ADMIN_NAME == sel_shape(),])

        output = sf::st_area(crit_hab_in_area_intersected)
      }
      output = round(as.numeric(sum(output))/1000,0)
    }
    output = shiny::HTML(paste0(output,' km<sup>2</sup>'))
    output
  })

  # =======================
  #  Renderings
  # =======================

  # Fill opacity for polygons added to map initially; NOT responsive to species selector.
  bg_opacity = 0.3
  # Fill opacity for polygons of SELECTED SPECIES
  fg_opacity = 0.8

  # Start leaflet map.
  output$my_leaf = renderLeaflet({
    l = leaflet() |>
      addTiles(group = 'OpenStreetMap') |>
      addProviderTiles(providers$CartoDB, group = 'CartoDB') |>
      addMapPane('CDC-Aquatic-SAR', zIndex = 300) |>
      addMapPane('DFO-SARA', zIndex = 400) |>
      addMapPane('Critical-Habitat', zIndex = 500) |>
      # clearGroup(c('CDC Aquatic SAR','DFO SARA','Critical Habitat')) |>
      addPolygons(
        data = sar,
        fillColor = 'orange',
        color = 'orange',
        fillOpacity = bg_opacity,
        weight = 1.5,
        group = 'CDC Aquatic SAR',
        label = ~paste0(ENG_NAME),
        options = pathOptions(pane = 'CDC-Aquatic-SAR', id = 'cdc_aquatic_sar_layer')
      ) |>
      addPolygons(
        data = dfo_sara,
        fillColor = 'purple',
        color = 'purple',
        fillOpacity = bg_opacity,
        weight = 1.5,
        group = 'DFO SARA',
        label = ~paste0(Common_Name_EN,' (',SARA_Status,')'),
        options = pathOptions(pane = 'DFO-SARA', id = 'dfo_sara_layer')
      ) |>
      addPolygons(
        data = crit_hab_bc,
        fillColor = 'lightblue',
        color = 'lightblue',
        fillOpacity = bg_opacity,
        weight = 1.5,
        group = 'Critical Habitat',
        label = ~paste0(Common_Name_EN,' (',Population_EN,')'),
        options = pathOptions(pane = 'Critical-Habitat', id = 'critical_habitat_layer')
      ) |>
      addLegend(title = 'Species-at-Risk',
                labels = c('CDC Aquatic SAR',
                           'DFO SARA',
                           'Critical Habitat'),
                colors = c('orange','purple','lightblue')) |>
      addLayersControl('bottomleft',
                       baseGroups = c('OpenStreetMap','CartoDB'),
                       overlayGroups = c("Boundaries","CDC Aquatic SAR",
                                         "DFO SARA","Critical Habitat"),
                       options = layersControlOptions(collapsed = FALSE)) |>
      addScaleBar('bottomright') |>
      leaflet.extras::addResetMapButton()

    shinyjs::toggle(id = 'loading_stuff', anim = TRUE, time = 8, animType = 'fade')

    l
  })

  # Fire up the short javascript I wrote to prevent the 3 SAR layers from being clickable,
  # but still showing their labels.
  observeEvent(
    c(input$my_leaf_cdc_aquatic_sar_layer_click,
      input$my_leaf_dfo_sara_layer_click,
      input$my_leaf_critical_habitat_layer_click), {
    session$sendCustomMessage("disableLayerBClickPropagation")
  })

  observe({
    # Fire up the leaflet proxy to add things quickly to the map,

    # Only let this mapping go ahead once all the reactiveVals are updated with
    # current values from the user's input.
    req(sel_type() == input$shape_type_input)

    l = leafletProxy('my_leaf') |>
      clearGroup('Boundaries') |>
      clearGroup('boundary_highlight') |>
      clearGroup('CDC Aquatic SAR highlight') |>
      clearGroup('DFO SARA highlight') |>
      clearGroup('Critical Habitat highlight') |>
      addPolygons(
        data = shapes() |>
          dplyr::mutate(highlight = ADMIN_NAME == sel_shape()) |>
          dplyr::mutate(leaf_cols = ifelse(highlight, 'transparent','black')),
        group = 'Boundaries',
        layerId = ~ADMIN_NAME,
        label = ~ADMIN_NAME,
        fillColor = 'transparent',
        color = ~leaf_cols,
        weight = 2
    )

    if(sel_shape() != 'Province'){

      hl_bbox = as.data.frame(sf::st_bbox(shapes()[shapes()$ADMIN_NAME == sel_shape(),]))$x

      l = l |>
        addPolygons(
          data = shapes() |>
            dplyr::filter(ADMIN_NAME != sel_shape()) |>
            dplyr::mutate(leaf_cols = 'black',
                          leaf_fills = 'darkgrey'),
          color = ~leaf_cols,
          layerId = ~ADMIN_NAME,
          label = ~ADMIN_NAME,
          fillColor = ~leaf_fills,
          fillOpacity = 0.75,
          opacity = 0.5,
          group = 'Boundaries',
          weight = 1
        ) |>
        addPolygons(
          data = shapes() |>
            dplyr::filter(ADMIN_NAME == sel_shape()) |>
            dplyr::mutate(leaf_cols = 'gold',
                          leaf_fills = 'transparent'),
          color = ~leaf_cols,
          layerId = ~ADMIN_NAME,
          label = ~ADMIN_NAME,
          fillColor = ~leaf_fills,
          fillOpacity = 0.75,
          opacity = 0.95,
          group = 'Boundaries',
          weight = 3,
          options = pathOptions(clickable = F)
        ) |>
        flyToBounds(lng1 = hl_bbox[1],
                    lat1 = hl_bbox[2],
                    lng2 = hl_bbox[3],
                    lat2 = hl_bbox[4])
    }

    # If one or more specific species have been chosen,
    # add those to the map via leaflet proxy.
    if(!is.null(input$species_select_input)){
      if(nrow(sar_sp()) > 0){
        l = l |>
          clearGroup('CDC Aquatic SAR highlight') |>
          addPolygons(
            data = sar_sp(),
            fillColor = 'orange',
            color = 'orange',
            fillOpacity = fg_opacity,
            weight = 1.5,
            group = 'CDC Aquatic SAR highlight',
            label = ~paste0(ENG_NAME),
            options = pathOptions(pane = 'CDC-Aquatic-SAR', id = 'cdc_aquatic_sar_layer')
          )
      }
      if(nrow(dfo_sara_sp()) > 0){
        l = l |>
          clearGroup('DFO SARA highlight') |>
          addPolygons(
            data = dfo_sara_sp(),
            fillColor = 'purple',
            color = 'purple',
            fillOpacity = fg_opacity,
            weight = 1.5,
            group = 'DFO SARA highlight',
            label = ~paste0(Common_Name_EN,' (',SARA_Status,')'),
            options = pathOptions(pane = 'DFO-SARA', id = 'dfo_sara_layer')
          )
      }
       if(nrow(crit_hab_bc_sp()) > 0){
         l = l |>
           clearGroup('Critical Habitat highlight') |>
           addPolygons(
             data = crit_hab_bc_sp(),
             fillColor = 'lightblue',
             color = 'lightblue',
             fillOpacity = fg_opacity,
             weight = 1.5,
             group = 'Critical Habitat highlight',
             label = ~paste0(Common_Name_EN,' (',Population_EN,')'),
             options = pathOptions(pane = 'Critical-Habitat', id = 'critical_habitat_layer')
           )
       }
       }
    l
  })
}

