library(bslib)

summary_boxes_mod_UI <- function(id) {
  ns <- NS(id)

  number_unique_SAR_species_card = card(
    # p('Unique Aquatic SAR species\n(CDC and DFO SARA)'),
    p('Aquatic SAR (CDC) and SARA-listed (DFO) species'),
    h5(textOutput(ns('sar_unique_species_number'))),
    class = "bg-secondary"
  )

  sq_km_critical_habitat = card(
    card_title('Spatially Defined DFO Critical Habitat'),
    h5(uiOutput(ns('km_2_crit_hab'))),
    class = "skyblue-box"
  )

  summary_cards = tagList(
    layout_column_wrap(
    width = 1/2,
    number_unique_SAR_species_card,
    sq_km_critical_habitat
  ),
    bslib::card(
      p('Species List', style = 'font-size:1.1rem;text-align:center;margin-bottom:-2rem;'),
      p(textOutput(ns('sar_unique_species_list')),
        style = 'font-size:large;'),
      full_screen = TRUE,
      max_height = '18vh'
    )
  )

  summary_cards
}

summary_boxes_mod_Server <- function(id, sel_shape, sar_sp, dfo_sara_sp, crit_hab_bc_sp, shape_type_input, shapes) {
  moduleServer(
    id,
    function(input, output, session) {

      sar_unique_species = reactive({
        if(sel_shape() == 'Province'){
          sar_output = sar_sp()$eng_name
          dfo_sara_output = dfo_sara_sp()$eng_name
        } else {
          if(shape_type_input() == 'nr_r'){

            sar_output = sar_sp() |>
              sf::st_drop_geometry() |>
              dplyr::filter(stringr::str_detect(REGION_NAME,sel_shape())) |>
              dplyr::pull(eng_name)

            dfo_sara_output = dfo_sara_sp() |>
              sf::st_drop_geometry() |>
              dplyr::filter(stringr::str_detect(REGION_NAME,sel_shape())) |>
              dplyr::pull(eng_name)

          } else {

            sar_output = sar_sp() |>
              sf::st_drop_geometry() |>
              dplyr::filter(stringr::str_detect(DISTRICT_NAME,sel_shape())) |>
              dplyr::pull(eng_name)

            dfo_sara_output = dfo_sara_sp() |>
              sf::st_drop_geometry() |>
              dplyr::filter(stringr::str_detect(DISTRICT_NAME,sel_shape())) |>
              dplyr::pull(eng_name)
          }
        }
        unique(c(sar_output, dfo_sara_output))
      })

      output$sar_unique_species_number = renderText({
        length(sar_unique_species())
      })

      output$sar_unique_species_list = renderText({
        paste0(sar_unique_species()[order(sar_unique_species())], collapse = ', ')
      })

      output$km_2_crit_hab = renderUI({
        if(sel_shape() == 'Province'){
          output = round(as.numeric(sum(st_area(crit_hab_bc_sp())))/1000,0)
        } else {
          if(shape_type_input() == 'nr_r'){
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

      # return(list(
      #   sar_unique_species = reactive(sar_unique_species),
      #   km_2_crit_hab = reactive(km_2_crit_hab))
      #   )
    }
  )
}
