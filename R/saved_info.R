# Saved information

saved_info_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12
        , div(
          class = "saved-info"
          , h3("Saved table views")
          , reactableOutput(ns("table_ui"))
        )
      )
    )
  )
}

saved_info_server <- function(id){
  moduleServer(
    id
    , function(input, output, session){

      ns <- session$ns

      observeEvent(
        input$refresh_table
        , {
          get_local_storage_multi(1:8, session)
        }
      )

      saved <- eventReactive(
        input$local_storage_multi
        , {
          ls <- input$local_storage_multi %>% map(fromJSON)
          shiny::validate(need(data, "Local storage not available..."))

          empty_row <- function(i){
            data.table(
              index = i
              , used = "no"
              , table_view = ""
              , identifier_count = NA
              , factor_count = NA
              , measure_count = NA
              , factor_filters = NA
              , measure_filter = ""
              , bars = NA
            )
          }

          df <- ls %>% map2(
            seq_along(.)
            , function(x, i){

              is_empty <- length(x) == 1
              if(is_empty) return(empty_row(i))

              data.table(
                index = i
                , used = "yes"
                , table_view = x$table_view
                , identifier_count = length(x$identifier_select)
                , factor_count = length(x$factor_select)
                , measure_count = length(x$measure_select)
                , factor_filters = length(x$factor_filter_select)
                , measure_filter = x$slider_field
                , bars = length(x$bar_option)
              )
            }
          ) %>% rbindlist()

          df
      })

      output$table_ui <- renderReactable({

        df <- saved()

        reactable(
          df
          , compact = TRUE
          , sortable = FALSE
          )

      })

      outputOptions(output, "table_ui", suspendWhenHidden = FALSE)



    }
  )



}
