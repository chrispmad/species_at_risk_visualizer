walkthrough_boxes = tagList(
  div(id = 'walkthrough-1',
      style = 'display:none;',
      tagList(
        bslib::card(
          layout_columns(
            col_widths = c(3,9),
            shiny::icon('arrow-left', class = 'red-arrow-left'),
            p('Species can be highlighted by selecting their common name')
          ),
          layout_column_wrap(
            width = 1/2,
            actionButton('wt_1_to_2',
                         'Next'),
            actionButton('close_tips',"Close"),
          ),
          class = "walkthrough_box_style",
          style = 'top:5vh;'
        ),
        absolutePanel(
          top = 0, left = 0,
          class = "walkthrough_background_fuzzer_style"
        )
      )
  ),
  div(id = 'walkthrough-2',
      style = 'display:none;',
      tagList(
        bslib::card(
          layout_columns(
            col_widths = c(2,8,2),
            shiny::icon('arrow-left', class = 'red-arrow-left'),
            p('NR Regions or Districts can be selected with the drop-down menu or by clicking them on the map'),
            shiny::icon('arrow-right', class = 'red-arrow-right')
          ),
          layout_column_wrap(
            width = 1/3,
            actionButton('wt_2_to_1',
                         'Back'),
            actionButton('wt_2_to_3',
                         'Next'),
            actionButton('close_tips_2',"Close"),
          ),
          class = "walkthrough_box_style",
          style = 'top:30vh;'
        ),
        absolutePanel(
          top = 0, left = 0,
          class = "walkthrough_background_fuzzer_style"
        )
      )
  ),
  div(id = 'walkthrough-3',
      style = 'display:none;',
      tagList(
        bslib::card(
          layout_columns(
            col_widths = c(1,11),
            shiny::icon('arrow-left', class = 'red-arrow-left'),
            p('Summaries reflect your choice of species or NR Region / District; default is all species and entire province')
          ),
          layout_column_wrap(
            width = 1/3,
            actionButton('wt_3_to_2',
                         'Back'),
            actionButton('wt_3_to_4',
                         'Next'),
            actionButton('close_tips_3',"Close"),
          ),
          class = "walkthrough_box_style",
          style = 'top:50vh;'
        ),
        absolutePanel(
          top = 0, left = 0,
          class = "walkthrough_background_fuzzer_style"
        )
      )
  ),
  div(id = 'walkthrough-4',
      style = 'display:none;',
      tagList(
        bslib::card(
          layout_columns(
            col_widths = c(2,10),
            shiny::icon('arrow-left', class = 'red-arrow-left'),
            p('Click layer groups to show or hide them on the map')
          ),
          layout_column_wrap(
            width = 1/2,
            actionButton('wt_4_to_3',
                         'Back'),
            actionButton('close_tips_4',"Close"),
          ),
          class = "walkthrough_box_style",
          style = 'top:69vh;left:90vh;'
        ),
        absolutePanel(
          top = 0, left = 0,
          class = "walkthrough_background_fuzzer_style"
        )
      )
  )
)
