# Statistic table
statistic_table_ui <- function(id, field_df){

  ns <- NS(id)

  # Set up choices for selectize inputs -----------------------------------------------------------
  field_df <- field_df[order(display_name),]
  choices <- c("identifier", "factor", "measure", "date") %>%
    {set_names(
      map(.,
          function(var){
            field_df[group_as == var] %>%
              {set_names(.$name, .$display_name)}
          }
      ),.
    )}

  # Allow dates to be selected as measures
  choices_measure <- c(choices$measure, choices$date) %>% sort()

  # Settings init
  settings_init <- list(
    icon = "ellipsis-v"
    , choices_identifier = choices$identifier
    , choices_measure = choices_measure
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
            , choices = c("summary", "detail") %>%
              set_names(c("Summary", "Details"))
            , justified = TRUE
          )
        )
        , column(
          width = 6
          , div(
            style = "display: inline-block; margin-top: 6px; width: 40%;"
            , materialSwitch(
              inputId = ns("view_controls_switch"),
              label = "View controls",
              value = FALSE,
              status = "danger"
            )
          )
          , div(
            style = "display: inline-block; width: 55%"
            , uiOutput(ns("setting_circle_ui"))
          )
          , div(
            class = "table_controls"
            , style = "display: inline-block; width: 5%; position: absolute; right: 1px;"
            , table_settings_ui(ns("statistic_table"), settings_init)
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
              , options = list(
                plugins = list('drag_drop')
              )
            )
          )
          , column(
            width = 6
            , selectizeInput(
              inputId = ns("measure_select")
              , label = div(icon("columns"), "Measures")
              , choices = choices_measure
              , selected = ""
              , multiple = TRUE
              , width = '100%'
              , options = list(
                plugins = list('drag_drop')
                , placeholder = "Click to add measure columns"
              )
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
              , options = list(
                placeholder = "Double click a table cell (on left side of count column)"

              )
            )
          )
          , column(
            width = 3
            , uiOutput(ns("measure_slider_ui"))
          )
          , column(
            width = 3
            , div(
              id = ns("measure_statistic_div")
              , awesomeRadio(
                inputId = ns("measure_statistic")
                , label = "Summary statistic"
                , choices = c("min", "mean", "max", "sd") %>%
                  set_names(c("Min", "Avg", "Max", "SD"))
                , selected = "mean"
                , inline = TRUE
                , status = "primary"
              )
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
      )

      # Local reactive values ---------------------------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE
        , table_view = NULL
        , setting_circle = NULL
        , local_storage = NULL
        , last_factor_select = NULL
        , slider_field = NULL
        , slider_handles = NULL
        , updated_measure_slider_label = NULL
        , slider_is_range = NULL
        , factor_filter = NULL
        , measure_filter = NULL
        , factor_filter_choices = NULL
      )

      # Initialize --------------------------------------------------------------------------------
      observe({
        if(m$run_once) return()

        m$table_view <- "summary"
        m$setting_circle <- 0
        m$last_factor_select <- input$factor_select
        m$slider_field <- init$slider_field$name
        m$slider_handles <- "one"
        m$slider_is_range <- FALSE
        m$factor_filter <- data.table(
          name = character(0)
          , display = character(0)
          , value = character(0)
          , input_name = character(0)
          , input_display = character(0)
        )
        m$measure_filter <- data.table(
          name = character(0)
          , display = character(0)
          , value = numeric(0)
        )
        m$factor_filter_choices <- m$factor_filter %>% copy()

        m$run_once <- TRUE

      })

      # Setting circles ---------------------------------------------------------------------------

      output$setting_circle_ui <- renderUI({
        id <- m$setting_circle %>% as.integer()
        1:5 %>% map(~circle_icon(.x, id == .x))
      })

      # Change to circle loads stored settings
      observeEvent(
        m$setting_circle
        , {
          get_local_storage(m$setting_circle, session)

          # Update local storage for 0
          data <- list(setting_circle = m$setting_circle)
          set_local_storage(0, data, session)
        }
      )

      # From clicking on circle
      observeEvent(
        input$setting_circle
        , {
          m$setting_circle <- input$setting_circle
        }
      )

      # Local storage -----------------------------------------------------------------------------

      # Initialise with defaults - used when no local storage found for an id
      initialise_local_storage <- function(id, session){

        id <- id %>% as.integer()

        # Zero contains non-settings meta data - e.g. circle last clicked
        if(id == 0){
          data <- list(
            setting_circle = 1
          )
        }

        if(id > 0){
          data <- list(
            test = "test"
          )
        }

        set_local_storage(id, data, session)
      }

      # Get local storage settings - triggered by get_local_storage()
      observeEvent(
        input$local_storage
        , {
          id <- m$setting_circle %>% as.integer()
          local_storage <- input$local_storage
          storage_empty <- is.null(local_storage)

          if(storage_empty){
            message("local storage - initialising: ", id)
            initialise_local_storage(id, session)
            get_local_storage(id, session)
            return()
          }

          message("local storage - loading: ", id)
          ls_data <- local_storage %>% fromJSON()

          if(id == 0){
            m$setting_circle <- ls_data$setting_circle
          }

          if(id > 0){

          }

        }, ignoreNULL = FALSE, ignoreInit = TRUE
      )

      # Toggle measure statistics --------------------------------------------------------------
      observeEvent(
        input$measure_select
        , {
          session$sendCustomMessage(
            "control_visibility"
            , list(id = ns("measure_statistic_div"), visible = !is.null(input$measure_select))
          )
        }, ignoreNULL = FALSE
      )

      # Table settings ----------------------------------------------------------------------------

      rt_settings <- table_settings_server("statistic_table")

      # Reactable data ----------------------------------------------------------------------------
      rt_container <- reactive({

        settings <- rt_settings()

        measure_statistic <- input$measure_statistic

        selected <- list(
          factor = input$factor_select
          , measure = input$measure_select
        )
        filtered <- list(
          factor = m$factor_filter
          , measure = m$measure_filter
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

        # Apply measure filters
        if(is_filtered$measure){
          df <- df %>% apply_measure_filter(slider, filtered, ac)
        }

        # Exit if filtered data has no rows
        shiny::validate(
          need({nrow(df) > 0}, "No results try removing filters")
        )

        # Get count
        dfc <- df[, .(count = .N), by = c(selected$factor)]

        if(is_selected$measure){
          dfc <- dfc %>%
            add_statistic_cols(df, measure_statistic, selected)
        }

        # Narrow detail data: df
        id_cols <- settings$identifier_select
        detail_cols <- c(id_cols, selected$factor, selected$measure, m$slider_field) %>% unique()
        df <- df[, ..detail_cols]

        # Sort
        dfc <- dfc %>% setorder(-count)

        apply_column_definitions <- function(df, field_config){

          names(df) %>% walk(
            function(col_name){
              config <- field_config[[col_name]]
              display_decimals <- config$display_decimals

              col <- df[[col_name]]

              if(is.null(display_decimals)) browser()

              if(!is.na(display_decimals)){
                col <- col %>% format(big.mark = ",", digits = display_decimals)
                df[[col_name]] <<- col
              }
            }
          )

          df
        }

        # Apply column definitions
        df <- df %>%
          apply_column_definitions(init$field)

        dfc <- dfc %>%
          apply_column_definitions(init$field)


        # Add html to cells for column names, bars
        df <- df %>%
          add_html_to_cells(settings, selected)

        dfc <- dfc %>%
          add_html_to_cells(settings, selected)

        # Return
        list(
          summary = dfc
          , detail = df
          , columns = list(
            summary = k$column_definitions[names(dfc)]
            , detail = k$column_definitions[names(df)]
          )
        )

      })

      # Change table view -------------------------------------------------------------------------
      observeEvent(
        input$table_view
        , {
          m$table_view <- input$table_view
        }
      )

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

          if(item$container %>% str_detect("measure_select")){
            message("selected measure: ", item$value)
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

          # Exit if cell has no column name
          if(!has_col_name) return()

          # Seperate factor and measure cell dblclicks
          group_as <- init$field[[cell$col_name]]$group_as

          # Send factor cell double clicks to factor filter
          if(group_as == "factor"){
            update_factor_filter(
              session
              , cell$col_name
              , cell$value
              , init$field
            )
          }

          # Send measure cell double clicks to measure slider
          if(group_as %in% c("date", "measure")){
            m$slider_field <- cell$col_name
          }
        }
      )

      update_factor_filter <- function(session, col_name, value, field){

        display_name <- field[[col_name]]$display_name

        settings <- rt_settings()

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
          choice_row_count <- min(settings$max_factor_filter_choices, nrow(choices_df))
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

      # Slider UI ---------------------------------------------------------------------------------

      output$measure_slider_ui <- renderUI({

        # Get range
        slider_step <- init$field[[m$slider_field]]$slider_step
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

        # Settings - one or two handles
        if(m$slider_handles == "one"){
          value <- slider_range[2]
        } else {
          value <- slider_range
        }

        sliderInput(
          inputId = ns("measure_slider")
          , label = div(icon("filter"), slider_label)
          , value = value
          , min = slider_range[1]
          , max = slider_range[2] %>% ceiling_dec(digits = decimal_count)
          , step = slider_step
          , timeFormat = time_format
          , ticks = FALSE
          , sep = ","
          , animate = TRUE
          , width = '100%'
        )

      })

      # Change number of slider handles -----------------------------------------------------------

      observeEvent(
        rt_settings()
        , {
          settings <- rt_settings()
          m$slider_handles <- settings$slider_handles
        }
      )

      # Change slider values ----------------------------------------------------------------------
      observeEvent(
        input$measure_slider
        , {

          slider <- input$measure_slider
          if(init$field[[m$slider_field]]$group_as == "date"){
            slider <- slider %>% format("%Y") %>% as.numeric()
          }

          df <- data.table(
            name = m$slider_field
            , display = init$field[[m$slider_field]]$display_name
            , value = slider
          )

          m$measure_filter <- df

        }, ignoreInit = TRUE
      )


      # Reactable ---------------------------------------------------------------------------------
      output$statistic_rt <- renderReactable({

        rt <- rt_container()
        shiny::validate(
          need(rt, "Loading...")
        )

        view <- m$table_view
        df <- rt[[view]]
        columns <- rt$columns[[view]]

        reactable(
          df
          , columns = columns
          , striped = TRUE
          , highlight = TRUE
          , minRows = 10
          , rowClass = JS("function(rowInfo){return rowInfo}")
        )

      })

    }
  )


}
