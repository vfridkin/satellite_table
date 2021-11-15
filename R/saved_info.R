# Saved information

saved_info_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12
        , div(
          class = "saved-info"
          , style = "padding: 10px;"
          , h3("Saved views", style = "background: transparent; text-align: center;")
          , reactableOutput(ns("table"))
          , div(
            class = "saved-info-controls"
            , actionButton(
              ns("remove_button")
              , "Reset selected"
            )
            , checkboxInput(
              ns("splash_check")
              , "Hide moon/satellite help at start"
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

      # Local functions ---------------------------------------------------------------------------
      get_saved <- function(){
        ls_ids <- c(1:8, 0, "splash_check")
        get_local_storage_multi(ls_ids, session)
      }

      # Refresh table from opener -----------------------------------------------------------------
      observeEvent(
        input$refresh_table
        , {
          get_saved()
        }
      )

      # Data for table ----------------------------------------------------------------------------
      saved <- eventReactive(
        input$local_storage_multi
        , {
          ls <- input$local_storage_multi %>% map(fromJSON)
          shiny::validate(need(data, "Local storage not available..."))

          current_id <- ls[[9]]$setting_circle
          splash_check <- ls[[10]]

          updateCheckboxInput(
            session
            , inputId = "splash_check"
            , value = splash_check
          )

          # Unused row definition
          empty_row <- function(i){
            data.table(
              index = i
              , used = "no"
              , table_view = ""
              , current = FALSE
              , identifier_count = NA
              , factor_count = NA
              , measure_count = NA
              , filtered = NA
            )
          }

          # Convert list from JSON into table data
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
                index = i
                , used = "yes"
                , table_view = x$table_view
                , current = i == current_id
                , identifier_count = length(x$identifier_select)
                , factor_count = length(x$factor_select)
                , measure_count = length(x$measure_select)
                , filtered = is_filtered
              )
            }
          ) %>% rbindlist()

          # Column groups
          col_grp = list(
            colGroup(
              name = "Visible columns"
              , columns = c("identifier_count", "factor_count", "measure_count")
              )
          )

          # Column definition functions
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

          view_cell <- function(x){
            res <- c("summary" = "Summary", "detail" = "Details")[x] %>% unname()
            if(is.na(res)) return("")
            res
          }

          filter_cell <- function(x){
            show_icon <- !is.na(x) && is.logical(x) && x
            if(show_icon) return(icon("filter"))
            NULL
          }

          # Column definitions
          col_def <- list(
            index = colDef(name = "Position", align = "center", minWidth = 75)
            , used = colDef(
              name = "Used"
              , align = "center"
              , minWidth = 50
              , cell = function(x) used_cell(x)
            )
            , table_view = colDef(
              name = "View"
              , align = "center"
              , minWidth = 75
              , cell = function(x) view_cell(x)
              )
            , current = colDef(
              name = ""
              , align = "center"
              , minWidth = 30
              , cell = function(x) circle_cell(x)
              , html = TRUE
            )
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
            , columnGroups = col_grp
            , columns = col_def
            , current_id = current_id
          )
        })

      output$table <- renderReactable({

        saved <- saved()

        reactable(
          saved$data
          , columnGroups = saved$columnGroups
          , columns = saved$columns
          , compact = TRUE
          , sortable = FALSE
          , selection = "multiple"
        )

      })

      outputOptions(output, "table", suspendWhenHidden = FALSE)

      # Remove button -----------------------------------------------------------------------------

      observeEvent(
        input$remove_button
        , {
          selected <- getReactableState("table", "selected") %>% req()
          selected %>% walk(~remove_local_storage(.x, session))

          # If current id is refreshed then reload page
          current_id <- saved()$current_id
          if(current_id %in% selected){
            get_local_storage(current_id, session)
          }
          get_saved()
        }
      )

      # Splash check ------------------------------------------------------------------------------

      # determines if moon/satellite bubbles show at start up
      observeEvent(
        input$splash_check
        , {
          splash_check <- input$splash_check
          set_local_storage("splash_check", splash_check, session)
        }, ignoreInit = TRUE
      )

    # End

    }
  )



}
