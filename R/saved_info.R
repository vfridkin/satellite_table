# Saved information

saved_info_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12
        , div(
          class = "saved-info"
          , h3("Saved views")
          , reactableOutput(ns("table_ui"))
          , div(
            class = "saved-info-controls"
            , actionButton(
              ns("clear_button")
              , "Reset selected"
            )
            , checkboxInput(
              ns("splash_check")
              , "Indicate moon and satellite at start"
              , value = TRUE
            )
          )
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
          ls_ids <- c(1:8, 0, "splash_check")
          get_local_storage_multi(ls_ids, session)
        }
      )

      saved <- eventReactive(
        input$local_storage_multi
        , {
          ls <- input$local_storage_multi %>% map(fromJSON)
          shiny::validate(need(data, "Local storage not available..."))

          current_id <- ls[[9]]$setting_circle
          splash_check <- ls[[10]]

          empty_row <- function(i){
            data.table(
              current = FALSE
              , index = i
              , used = "no"
              , table_view = ""
              , identifier_count = NA
              , factor_count = NA
              , measure_count = NA
              , filtered = NA
            )
          }

          df <- ls[1:8] %>% map2(
            seq_along(.)
            , function(x, i){

              is_empty <- length(x) == 1
              if(is_empty) return(empty_row(i))

              is_filtered <- any(
                length(x$factor_filter_select) > 0
                , x$is_slider_filtering
              )

              data.table(
                current = i == current_id
                , index = i
                , used = "yes"
                , table_view = x$table_view
                , identifier_count = length(x$identifier_select)
                , factor_count = length(x$factor_select)
                , measure_count = length(x$measure_select)
                , filtered = is_filtered
              )
            }
          ) %>% rbindlist()

          circle_cell <- function(x){
            circle_icon(1, x)
          }

          used_cell <- function(x){
            if(x == "yes"){
              return(
                div(class = "used-tag", x)
              )
            }
            x
          }

          filter_cell <- function(x){
            show_icon <- !is.na(x) && is.logical(x) && x
            if(show_icon) return(icon("filter"))
            NULL
          }

          col_def <- list(
            current = colDef(
              name = ""
              , align = "center"
              , minWidth = 30
              , cell = function(x) circle_cell(x)
              , html = TRUE
            )
            , index = colDef(name = "Position", align = "center", minWidth = 75)
            , used = colDef(
              name = "Used"
              , align = "center"
              , minWidth = 50
              , cell = function(x) used_cell(x)
            )
            , table_view = colDef(name = "View", align = "center", minWidth = 75)
            , identifier_count = colDef(name = "Identifier", align = "center", minWidth = 75)
            , factor_count = colDef(name = "Factor", align = "center", minWidth = 75)
            , measure_count = colDef(name = "Measure", align = "center", minWidth = 75)
            , filtered = colDef(
              name = "Filter"
              , align = "center"
              , minWidth = 50
              , cell = function(x) filter_cell(x)
            )
          )

          list(
            data = df
            , columns = col_def
          )
        })

      output$table_ui <- renderReactable({

        saved <- saved()

        reactable(
          saved$data
          , columns = saved$columns
          , compact = TRUE
          , sortable = FALSE
          , selection = "multiple"
        )

      })

      outputOptions(output, "table_ui", suspendWhenHidden = FALSE)

    }
  )



}
