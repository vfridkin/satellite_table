# Statistic table
statistic_table_ui <- function(id, field_df){

  ns <- NS(id)

  # Set up choices for selectize inputs -----------------------------------------------------------
  field_df <- field_df[order(display_name),]
  choices <- c("factor", "value", "date") %>%
    {set_names(
      map(.,
          function(var){
            field_df[group_as == var] %>%
              {set_names(.$name, .$display_name)}
          }
      ),.
    )}

  # Allow dates to be selected as values
  choices_value <- c(choices$value, choices$date) %>% sort()

  # Settings init
  settings_init <- list(
    icon = "ellipsis-v"
    , choices_value = choices_value
  )



  # Main UI ---------------------------------------------------------------------------------------
  div(
    class = "statistic_container"
    , div(
      class = "statistic__select-box"
      , fluidRow(
        column(
          width = 6
          , radioGroupButtons(
            inputId = ns("table_view")
            , label = NULL
            , choices = c("summary", "details") %>%
              set_names(c("Summary", "Details"))
            , justified = TRUE
          )
        )
        , column(
          width = 6
          , div(
            style = "display: inline-block; margin-top: 6px;"
            , materialSwitch(
              inputId = ns("view_controls_switch"),
              label = "View controls",
              value = FALSE,
              status = "danger"
            )
          )
          , div(
            class = "table_controls"
            , style = "display: inline-block; position: absolute; right: 0;"
            , table_settings_ui("statistic_table", settings_init)
          )
        )
      )
      , div(
        class = "table_controls"
        , fluidRow(
          column(
            width = 6
            , selectizeInput(
              inputId = ns("factor_select")
              , label = div(icon("columns"), "Factors")
              , choices = choices$factor
              , selected = choices$factor[1]
              , multiple = TRUE
              , width = '100%'
              , options = list(plugins = list('drag_drop'))
            )
          )
          , column(
            width = 6
            , selectizeInput(
              inputId = ns("value_select")
              , label = div(icon("columns"), "Values")
              , choices = choices_value
              , selected = ""
              , multiple = TRUE
              , width = '100%'
              , options = list(plugins = list('drag_drop'))
            )
          )
        )
        , fluidRow(
          column(
            width = 6
            , selectizeInput(
              inputId = ns("factor_filter_select")
              , label = div(icon("filter"), "Factors")
              , choices = ""
              , selected = ""
              , multiple = TRUE
              , width = '100%'
            )
          )
          , column(
            width = 3
            , sliderInput(
              inputId = ns("value_slider")
              , label = div(icon("filter"), "Loading...")
              , min = 1
              , max = 10
              , value = 5
              , ticks = FALSE
              , sep = ","
              , animate = TRUE
              , width = '100%'
            )
          )
          , column(
            width = 3
            , awesomeRadio(
              inputId = ns("value_statistic")
              , label = "Summary statistic"
              , choices = c("min", "mean", "max", "sd") %>% set_names(
                c("Min", "Avg", "Max", "Std dev")
              )
              , selected = "mean"
              , inline = TRUE
              , status = "primary"
            )
          )
        )
      )
    )
    , div(
      class = "statistic__table-box"
      , fluidRow(
        column(
          width = 12
          , reactableOutput(
            outputId = ns("statistic_rt")
          )
        )
      )
    )
  )
}

statistic_table_server <- function(id, init, data){
  moduleServer(
    id
    , function(input, output, session){

      ns <- session$ns

      # Local constants ---------------------------------------------------------------------------
      k <- list(
        column_definitions = get_column_definitions(ac)
        , max_factor_filter_choices = 10
      )

      # Local reactive values ---------------------------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE
        , last_factor_select = NULL
        , slider_field = NULL
        , updated_value_slider_label = NULL
        , slider_is_range = NULL
        , factor_filter = NULL
        , value_filter = NULL
        , factor_filter_choices = NULL
      )

      # Initialize --------------------------------------------------------------------------------
      observe({
        if(m$run_once) return()
        m$last_factor_select <- input$factor_select
        m$slider_field <- init$slider_field$name
        m$slider_is_range <- FALSE
        m$factor_filter <- data.table(
          name = character(0)
          , display = character(0)
          , value = character(0)
          , input_name = character(0)
          , input_display = character(0)
        )
        m$value_filter <- data.table(
          name = character(0)
          , display = character(0)
          , value = numeric(0)
        )
        m$factor_filter_choices <- m$factor_filter %>% copy()

        m$run_once <- TRUE

      })

      # Table settings ----------------------------------------------------------------------------

      rt_settings <- table_settings_server("statistic_table")


      # Reactable data ----------------------------------------------------------------------------
      rt_container <- reactive({

        settings <- rt_settings()
        value_statistic <- input$value_statistic

        selected <- list(
          factor = input$factor_select
          , value = input$value_select
        )
        filtered <- list(
          factor = m$factor_filter
          , value = m$value_filter
        )
        slider <- list(
          is_range = m$slider_is_range
          , field = m$slider_field
        )

        is_selected <- selected %>% map(~!is.null(.x))
        is_filtered <- filtered %>% map(~nrow(.x) > 0)

        # Ensure group selection always has one element
        if(!is_selected$factor){
          updateSelectizeInput(
            session
            , inputId = "factor_select"
            , selected = m$last_factor_select
          )
          return()
        }

        m$last_factor_select <- selected$factor

        # Make a copy of the data so that original is not edited
        df <- data %>% copy()

        # Apply factor filters
        if(is_filtered$factor){
          df <- df %>% apply_factor_filter(filtered)
        }

        # Apply value filters
        if(is_filtered$value){
          df <- df %>% apply_value_filter(slider, filtered, ac)
        }

        # Exit if filtered data has no rows
        validate(
          need(nrow(df) > 0, "No results - try removing filters")
        )

        # Get count
        dfc <- df[, .(count = .N), by = c(selected$factor)]

        if(is_selected$value){
          dfc <- dfc %>%
            add_statistic_cols(df, value_statistic, selected)
        }

        # Sort
        dfc <- dfc %>% setorder(-count)

        apply_column_definitions <- function(df, field_config){

          names(df) %>% walk(
            function(col_name){
              config <- field_config[[col_name]]
              display_decimals <- config$display_decimals

              col <- df[[col_name]]

              if(!is.na(display_decimals)){
                col <- col %>% format(big.mark = ",", digits = display_decimals)
                df[[col_name]] <<- col
              }
            }
          )

          df
        }

        # Apply column definitions
        dfc <- dfc %>%
          apply_column_definitions(ac$field)


        # Add html to cells for column names, bars
        dfc <- dfc %>%
          add_html_to_cells(settings, selected)

        # Column definitions
        col_def <- k$column_definitions[names(dfc)]

        # Return
        list(
          data = dfc
          , columns = col_def
        )

      })

      # View/hide controls ------------------------------------------------------------------------

      observeEvent(
        input$view_controls_switch
        , {
          session$sendCustomMessage("view_controls_switch", input$view_controls_switch)
        }
      )

      # Double click item -------------------------------------------------------------------------
      observeEvent(
        input$double_click_selectize_item
        , {
          item <- input$double_click_selectize_item %T>% req()

          if(item$container %>% str_detect("factor_select")){
            message("selected factor: ", item$value)
          }

          if(item$container %>% str_detect("value_select")){
            message("selected value: ", item$value)
            m$slider_field <- item$value
          }


        }
      )

      # Double click factor cell ------------------------------------------------------------------

      observeEvent(
        input$double_click_cell
        , {
          cell <- input$double_click_cell %>% req()

          has_col_name <- !is.null(cell$col_name)

          if(has_col_name){
            update_factor_filter(
              session
              , cell$col_name
              , cell$value
              , ac
            )
          }
        }
      )

      update_factor_filter <- function(session, col_name, value, ac){

        display_name <- ac$field[[col_name]]$display_name

        new_row <- data.table(
          name = col_name
          , display = display_name
          , value = value
          , input_name = paste(col_name, "=", value)
          , input_display = paste(display_name, "=", value)
        )

        # Get current choices
        choices_df <- m$factor_filter_choices

        # Check if new row is already in choices
        is_new_choice <- !new_row$input_name %in% choices_df$input_name
        if(is_new_choice){
          # Add to choices and reduce choices if over max
          choices_df <- new_row %>% list(choices_df) %>% rbindlist() %>% unique()
          choice_row_count <- min(k$max_factor_filter_choices, nrow(choices_df))
          choices_df <- choices_df[1:choice_row_count]
        }

        # Get currently selected
        selected <- input$factor_filter_select
        selected_df <- choices_df[input_name %in% selected]

        # Check if new row is already in selected
        is_new_select <- !new_row$input_name %in% selected_df$input_name

        # Exit if already in selection (as it would also be in choices)
        if(!is_new_select) return()

        # Add to currently selected
        selected_df <- new_row %>% list(selected_df) %>% rbindlist()

        # Convert dataframes to nameed vectors for updaing selectize input
        choices <- choices_df$input_name %>% set_names(choices_df$input_display)
        selected <- selected_df$input_name %>% set_names(selected_df$input_display)

        updateSelectizeInput(
          session = session
          , inputId = "factor_filter_select"
          , choices = choices
          , selected = selected
        )

        m$factor_filter_choices <- choices_df

      }

      observeEvent(
        input$factor_filter_select
        , {

          m$factor_filter <- input$factor_filter_select %>% map(
            function(x){
              x_split <- x %>% str_split("=", simplify = TRUE) %>% str_trim()
              data.table(name = x_split[1], value = x_split[2])
            }
          ) %>%
            rbindlist()

        }, ignoreNULL = FALSE
      )



      # Change slider definition ------------------------------------------------------------------
      observeEvent(
        m$slider_field
        , {

          # Get range
          slider_step <- ac$field[[m$slider_field]]$slider_step
          slider_range <- data[[m$slider_field]] %>% range(na.rm = TRUE)
          slider_label <- init$field[[m$slider_field]]$display_name %>%
            paste("(range)")

          range_class <- slider_range[1] %>% class()
          # It seems step is in milliseconds for time (hence large number for year step)
          time_format <- if(range_class == "Date") "%Y" else NULL
          decimal_count <- nchar(slider_step)

          # Remove decimal point from decimal count
          if(decimal_count > 1){
            decimal_count <- decimal_count - 1
          }

          # update slider
          updateSliderInput(
            session = session
            , inputId = "value_slider"
            , label = slider_label
            , value = slider_range[2]
            , min = slider_range[1]
            , max = slider_range[2] %>% ceiling_dec(digits = decimal_count)
            , step = slider_step
            , timeFormat = time_format
          )
        }
      )

      # Change slider values ----------------------------------------------------------------------
      observeEvent(
        input$value_slider
        , {

          slider <- input$value_slider
          if(ac$field[[m$slider_field]]$group_as == "date"){
            slider <- slider %>% format("%Y") %>% as.numeric()
          }

          df <- data.table(
            name = m$slider_field
            , display = init$field[[m$slider_field]]$display_name
            , value = slider
          )

          # Update label with filter icon - terrible workaround!
          slider_label <- df$display[1] %>% paste("(range)")

          session$sendCustomMessage(
            'change-slider-label'
            , list(
              id = ns("value_slider")
              , label = slider_label
            )
          )

          m$value_filter <- df

        }, ignoreInit = TRUE
      )


      # Reactable ---------------------------------------------------------------------------------
      output$statistic_rt <- renderReactable({

        rt <- rt_container()
        validate(
          need(rt, "Loading...")
        )

        reactable(
          rt$data
          , columns = rt$columns
          , striped = TRUE
          , highlight = TRUE
          , minRows = 10
          , rowClass = JS("function(rowInfo){return rowInfo}")
        )

      })

    }
  )


}
