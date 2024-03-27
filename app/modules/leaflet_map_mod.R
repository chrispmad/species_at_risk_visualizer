library(leaflet)

leaflet_map_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns('my_leaf')),
    div(id = ns('loading_stuff'),
        div(
          id = ns('loading_fish'),
          class = 'swimming-fish'
        ),
        div(
          p("Loading...",
            class = 'loading_text')
        )
    )
  )
}

leaflet_map_mod_Server <- function(id, sar, dfo_sara, crit_hab_bc,
                                   sel_type, sel_shape,
                               shape_type_input, shapes, species_select_input,
                               sar_sp, dfo_sara_sp, crit_hab_bc_sp) {
  moduleServer(
    id,
    function(input, output, session) {
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

      # # Fire up the short javascript I wrote to prevent the 3 SAR layers from being clickable,
      # # but still showing their labels.
      # observeEvent(
      #   c(input$my_leaf_cdc_aquatic_sar_layer_click,
      #     input$my_leaf_dfo_sara_layer_click,
      #     input$my_leaf_critical_habitat_layer_click), {
      #       session$sendCustomMessage("disableLayerBClickPropagation")
      #     })

      observe({
        # Fire up the leaflet proxy to add things quickly to the map,

        # Only let this mapping go ahead once all the reactiveVals are updated with
        # current values from the user's input.
        req(sel_type() == shape_type_input())

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
        if(!is.null(species_select_input())){
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

      # Return any click events on the leaflet map to inform other UI elements
      # outside of this module.
      return(reactive(input$my_leaf_shape_click))
    }
  )
}
