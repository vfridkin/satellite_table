# Satellite table ---------------------------------------------------------------------------------

# This is the main table in the app
# It comes with concealable table controls
# Additional controls are in 'more_settings' module
# User changes are saved in local storage
# A summary of these changes is displayed by 'saved_info' module

satellite_table_ui <- function(id, field_df){

  ns <- NS(id)

  # Set up choices for selectize inputs -----------------------------------------------------------
  choices <- get_choices()

  # Settings init
  settings_init <- list(
    icon = "ellipsis-h"
    , choices = choices
  )

  # Main UI ---------------------------------------------------------------------------------------
  div(
    class = "main_container"
    , div(
      class = "top_controls"
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
            id = ns("solar_system")
            , style = "padding-left: 10px; padding-right: 0;"
            , div(
              style = "display: inline-block; margin-top: 6px; width: 55px;"
              , materialSwitch(
                inputId = ns("view_controls_switch")
                , label = NULL
                , value = FALSE
                , status = "danger"
              )
            )
            , div(
              style = "display: inline-block;"
              , uiOutput(ns("setting_circle_ui"))
            )
          )
        )
      )
    )
    , div(
      id = ns("table_controls_div")
      , class = "table_controls"
      , style = "position: relative;"
      , fluidRow(
        class = "table-controls-select"
        , column(
          width = 6
          , div(
            class = "factor-select"
            , selectizeInput(
              inputId = ns("factor_select")
              , label = div(icon("columns"), "Factors")
              , choices = choices$factor %>%
                add_command_choices("Factor", "all", "inverse", "clear")
              , selected = choices$factor[1]
              , multiple = TRUE
              , width = '100%'
              , options = list(
                plugins = list('drag_drop')
              )
            )
          )
        )
        , column(
          width = 6
          , div(
            class = "measure-select"
            , selectizeInput(
              inputId = ns("measure_select")
              , label = div(icon("columns"), "Measures")
              , choices = choices$measure_select %>%
                add_command_choices("Measure", "all", "inverse", "clear")
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
      )
      , fluidRow(
        class = "table-controls-filter"
        , column(
          width = 6
          , div(
            class = "factor-filter-select"
            , selectizeInput(
              inputId = ns("factor_filter_select")
              , label = div(icon("filter"), "Factors")
              , choices = "" %>% add_factor_filter_commands()
              , selected = ""
              , multiple = TRUE
              , width = '100%'
              , options = list(
                placeholder = "Double click a table factor cell to filter"

              )
            )
          )
        )
        , column(
          width = 6
          , div(
            class = "measure-and-more"
            , fluidRow(
              style = "margin: 0;"
              , column(
                width = 6
                , style = "padding: 0"
                , div(
                  class = "measure-filter-select"
                  , uiOutput(ns("measure_slider_ui"))
                )
              )
              , column(
                width = 6
                , div(
                  id = ns("measure_statistic_div")
                  , selectizeInput(
                    inputId = ns("measure_statistic_select")
                    , label = "Measures statistic"
                    , choices = choices$measure_statistics
                    , selected = "mean"
                  )
                )
              )
              , div(
                class = "more_settings"
                , style = "position: absolute; bottom: -5px; right: 25px;"
                , more_settings_ui(ns("more_settings"), settings_init)
              )
            )
          )
        )
      )
    )
    , div(
      class = "main__table-box"
      , style = "padding: 10px; position: relative;"
      , fluidRow(
        column(
          width = 12
          , div(
            style = "position: absolute; left: -5px; top: 1px;"
            , uiOutput(ns("filters_applied_ui"))
          )
          , reactableOutput(
            outputId = ns("main_rt")
          )
          # Some fun
          , space_man("spaceman3.png", "Shiny space man")
        )
      )
    )
    , footer_element()
  )
}

satellite_table_server <- function(id, init, data){
  moduleServer(
    id
    , function(input, output, session){

      ns <- session$ns

      # Local constants ---------------------------------------------------------------------------
      k <- list(
        choices = get_choices()
        , column_definitions = get_column_definitions(ac)
        , setting_circle_count = 8
        , init_factor_filter_df = data.table(
          name = character(0)
          , display = character(0)
          , value = character(0)
          , input_name = character(0)
          , input_display = character(0)
        )
        , init_measure_filter_df = data.table(
          name = character(0)
          , display = character(0)
          , value = numeric(0)
        )
      )

      # Local reactive values ---------------------------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE

        # Summary vs details
        , table_view = NULL

        # Sun and planets
        , view_controls_switch = NULL
        , id = NULL # Active circle
        , local_storage = NULL

        # Selectors
        , factor_select = NULL
        , last_factor_select = NULL
        , measure_select = NULL

        # Factor filter
        , factor_filter_select = NULL
        , factor_filter_choices = NULL
        , factor_filter_select_df = NULL
        , factor_filter_choices_df = NULL

        # Measure filter
        , measure_slider_field = NULL
        , measure_slider_range = NULL
        , measure_slider = NULL
        , is_slider_filtering = NULL # Boolean: set to false if entire range selected
        , force_slider_update = 0  # Force the slider to update
        , measure_filter_df = NULL

        # Measure statistic
        , measure_statistic_select = NULL

        # Overall filter
        , is_table_filtered = NULL

        # Sorting
        , sort_select = NULL
        , sort_order = NULL

        # From settings dropdown
        , settings_init = NULL # input to settings module
      )

      # > Initialize ------------------------------------------------------------------------------
      observe({
        if(m$run_once) return()

        m$id <- 0

        m$last_factor_select <- input$factor_select

        m$factor_filter_select_df <- k$init_factor_filter_df %>% copy()
        m$factor_filter_choices_df <- k$init_factor_filter_df %>% copy()

        m$measure_slider_field <- init$measure_slider_field$name
        m$is_slider_filtering <- FALSE
        m$measure_filter_df <- k$init_measure_filter_df %>% copy()

        m$measure_statistic_select <- init$measure_statistic_select

        m$sort_select = init$sort_select
        m$sort_order = init$sort_order

        m$run_once <- TRUE

      })
      # Table view -------------------------------------------------------------------------
      observeEvent(
        input$table_view
        , {
          m$table_view <- input$table_view
        }
      )

      # View controls switch (Sun) ----------------------------------------------------------------
      observeEvent(
        input$view_controls_switch
        , {
          m$view_controls_switch <- input$view_controls_switch
        }
      )


      # > View/hide controls ------------------------------------------------------------------------
      observeEvent(
        input$view_controls_switch
        , {
          session$sendCustomMessage("view_controls_switch", input$view_controls_switch)
        }
      )

      # Setting circles (planets) -----------------------------------------------------------------

      output$setting_circle_ui <- renderUI({
        1:k$setting_circle_count %>% map(~circle_icon(.x, m$id == .x))
      })

      # Change to circle loads stored settings
      observeEvent(
        m$id
        , {
          get_local_storage(m$id, session)

          # Update local storage for 0
          if(m$id > 0){
            data <- list(setting_circle = m$id)
            set_local_storage(0, data, session)
          }
        }
      )

      # From clicking on circle
      observeEvent(
        input$setting_circle
        , {
          m$id <- input$setting_circle
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
            table_view = "summary"
            , view_controls_switch = TRUE

            , factor_select = init$factor_select
            , measure_select = NULL

            , factor_filter_select = NULL
            , factor_filter_choices = NULL
            , factor_filter_choices_names = NULL

            , measure_slider_field = init$measure_slider_field$name
            , is_slider_filtering = FALSE
            , measure_slider = NULL

            , measure_statistic_select = init$measure_statistic_select

            , sort_select = init$sort_select
            , sort_order = init$sort_order
            , bar_option = init$bar_option
            , identifier_select = init$identifier_select
          )
        }

        set_local_storage(id, data, session)
      }

      # > Get -------------------------------------------------------------------------------------
      # Get local storage settings - triggered by get_local_storage()
      observeEvent(
        input$local_storage
        , {
          id <- m$id
          local_storage <- input$local_storage
          storage_empty <- is.null(local_storage)

          if(storage_empty){
            message("local storage - initialising: ", id)
            initialise_local_storage(id, session)
            get_local_storage(id, session)
            message("initialise_local_storage")
            return()
          }

          stored <- local_storage %>% fromJSON()

          if(id == 0){
            m$id <- stored$setting_circle %>% as.integer()
          }

          if(id > 0){

            # Update table view (summary vs details)
            updateRadioGroupButtons(
              session = session
              , inputId = "table_view"
              , selected = stored$table_view
            )

            # Update controls switch (toggle controls view)
            updateMaterialSwitch(
              session = session
              , inputId = "view_controls_switch"
              , value = stored$view_controls_switch
            )

            # Update selectize inputs
            c("factor_select"
              , "measure_select"
            ) %>% walk(
              ~updateSelectizeInput(
                session = session
                , inputId = .x
                , selected = stored[[.x]]
              )
            )

            # Update factor filter selectize
            m$factor_filter_select <- stored$factor_filter_select
            m$factor_filter_choices <- stored$factor_filter_choices %>%
              set_names(stored$factor_filter_choices_names)

            updateSelectizeInput(
              session = session
              , inputId = "factor_filter_select"
              , selected = m$factor_filter_select
              , choices = m$factor_filter_choices %>% add_factor_filter_commands()
            )

            # Update measure slider
            m$measure_slider_field <- stored$measure_slider_field
            m$is_slider_filtering <- stored$is_slider_filtering
            m$measure_slider <- stored$measure_slider
            m$force_slider_update <- m$force_slider_update + 1

            # Update measure statistic
            updateSelectizeInput(
              session = session
              , inputId = "measure_statistic_select"
              , selected = stored$measure_statistic_select
            )

            m$measure_statistic_select <- stored$measure_statistic_select

            # Update sort
            m$sort_select = stored$sort_select
            m$sort_order = stored$sort_order

            # Update drop down settings
            m$settings_init <- list(
              identifier_select = stored$identifier_select
              , bar_option = stored$bar_option
            )
          }

        }, ignoreNULL = FALSE, ignoreInit = TRUE
      )

      # > Set -------------------------------------------------------------------------------------
      observeEvent(
        list(
          input$table_view
          , m$view_controls_switch
          , m$factor_select
          , m$measure_select
          , m$factor_filter_select
          , m$factor_filter_choices
          , m$measure_slider_field
          , m$is_slider_filtering
          , input$measure_slider
          , m$measure_statistic_select
          , m$sort_select
          , m$sort_order
          , rt_settings()
        )
        , {
          id <- m$id

          if(id == 0){
            return()
          }

          if(id > 0){

            data <- list(
              table_view = input$table_view
              , view_controls_switch = m$view_controls_switch
              , factor_select = m$factor_select
              , measure_select = m$measure_select
              , factor_filter_select = m$factor_filter_select
              , factor_filter_choices = m$factor_filter_choices
              , factor_filter_choices_names = names(m$factor_filter_choices)
              , measure_slider_field = m$measure_slider_field
              , is_slider_filtering = m$is_slider_filtering
              , measure_slider = input$measure_slider
              , measure_statistic_select = m$measure_statistic_select
              , sort_select = m$sort_select
              , sort_order = m$sort_order
            ) %>%
              c(rt_settings())
          }

          set_local_storage(id, data, session)
        }
      )

      # Factor filter -----------------------------------------------------------------------------
      observeEvent(
        m$factor_filter_select
        , {
          m$factor_filter_select_df <- m$factor_filter_select %>%
            convert_factor_filter_to_df(init$field, k$init_factor_filter_df)

        }, ignoreNULL = FALSE
      )

      observeEvent(
        m$factor_filter_choices
        , {
          m$factor_filter_choices_df <- m$factor_filter_choices %>%
            convert_factor_filter_to_df(init$field, k$init_factor_filter_df)

        }, ignoreNULL = FALSE
      )

      # > Double click table cell to filter -------------------------------------------------------

      observeEvent(
        input$double_click_cell
        , {
          cell <- input$double_click_cell %>% req()

          has_col_name <- !is.null(cell$col_name)
          col_name <- cell$col_name

          # Exit if cell has no column name
          if(!has_col_name) return()

          # Remove subtext
          value <- cell$value %>% str_split("\n") %>% pluck(1, 1)

          # Get definition for selected field
          this <- init$field[[col_name]]

          # Send factor cell double clicks to factor filter
          if(this$group_as == "factor"){

            select_df <- m$factor_filter_select_df
            choices_df <- m$factor_filter_choices_df

            new_row <- data.table(
              name = col_name
              , display = this$display_name
              , value = value
              , input_name = paste(col_name, "=", value)
              , input_display = paste(this$display_name, "=", value)
            )

            # Check if new row is already in selected
            is_new_select <- !new_row$input_name %in% select_df$input_name
            if(!is_new_select) return()

            # Check if new row is already in choices
            is_new_choice <- !new_row$input_name %in% choices_df$input_name
            if(is_new_choice){
              # Add to choices and reduce choices if over max
              choices_df <-  list(new_row, choices_df) %>%
              rbindlist() %>%
              .[1:min(init$max_factor_filter_choices, nrow(.))]
            }

            # Add to currently selected
            select_df <- list(select_df, new_row) %>% rbindlist()

            # Convert dataframes to nameed vectors for updaing selectize input
            choices <- choices_df$input_name %>% set_names(choices_df$input_display)
            selected <- select_df$input_name %>% set_names(select_df$input_display)

            updateSelectizeInput(
              session = session
              , inputId = "factor_filter_select"
              , selected = selected
              , choices = choices %>% add_factor_filter_commands()
            )

            m$factor_filter_select <- selected
            m$factor_filter_choices <- choices
          }

          # Send measure cell double clicks to measure slider
          if(this$group_as %in% c("date", "measure")){
            m$measure_slider_field <- col_name
          }
        }
      )

      # Slider UI ---------------------------------------------------------------------------------
      observeEvent(
          m$measure_slider_field
        , {
          m$measure_slider_range <- data[[m$measure_slider_field]] %>% range(na.rm = TRUE)
        }
      )

      output$measure_slider_ui <- renderUI({

        m$force_slider_update

        # Get range
        slider_step <- init$field[[m$measure_slider_field]]$slider_step
        measure_slider_range <- m$measure_slider_range
        slider_label <- init$field[[m$measure_slider_field]]$display_name %>%
          paste("(range)")

        range_class <- measure_slider_range[1] %>% class()
        # It seems step is in milliseconds for time (hence large number for year step)
        time_format <- if(range_class == "Date") "%Y" else NULL
        decimal_count <- nchar(slider_step)

        # Remove decimal point from decimal count
        if(decimal_count > 1){
          decimal_count <- decimal_count - 1
        }

        value <- measure_slider_range[2]

        # Update slider value if exists (i.e. from storage)
        load_slider_value <- m$measure_slider %>% {!is.null(.) && length(.) > 0}
        if(load_slider_value){
          value <- m$measure_slider
          # Convert character to date class - occurs when loading from JSON
          if(class(value) == "character"){
            value <- value %>% as.Date()
          }
        }

        sliderInput(
          inputId = ns("measure_slider")
          , label = div(icon("filter"), slider_label)
          , value = value
          , min = measure_slider_range[1]
          , max = measure_slider_range[2] %>% ceiling_dec(digits = decimal_count)
          , step = slider_step
          , timeFormat = time_format
          , ticks = FALSE
          , sep = ","
          , animate = TRUE
          , width = '100%'
        )

      })

      # > Change number of slider handles -----------------------------------------------------------

      observeEvent(
        rt_settings()
        , {
          settings <- rt_settings()
        }
      )

      # > Change slider values ----------------------------------------------------------------------
      observeEvent(
        input$measure_slider
        , {

          slider <- input$measure_slider

          m$is_slider_filtering <- if(length(slider) == 1){
            slider < m$measure_slider_range[2]
          } else {
            slider != m$measure_slider_range
          }

          if(init$field[[m$measure_slider_field]]$group_as == "date"){
            slider <- slider %>% format("%Y") %>% as.numeric()
          }

          df <- data.table(
            name = m$measure_slider_field
            , display = init$field[[m$measure_slider_field]]$display_name
            , value = slider
          )

          m$measure_filter_df <- df

        }, ignoreInit = TRUE
      )

      # Measure statistics ------------------------------------------------------------------------
      observeEvent(
        input$measure_statistic_select
        , {
          m$measure_statistic_select <- input$measure_statistic_select
        }
      )

      observeEvent(
        list(
          input$measure_select
          , m$table_view
        )
        , {
          session$sendCustomMessage(
            "control_visibility"
            , list(
              id = ns("measure_statistic_div")
              , visible = all(!is.null(input$measure_select), m$table_view == "summary")
            )
          )
        }, ignoreNULL = FALSE
      )

      # More settings ----------------------------------------------------------------------------

      rt_settings <- more_settings_server("more_settings", reactive(m$settings_init))
      # Table filters applied ---------------------------------------------------------------------

      observeEvent(
        list(
          m$factor_filter_select_df
          , m$is_slider_filtering
          , input$measure_slider
        )
        , {
          filter_count <- list(
            factor = nrow(m$factor_filter_select_df)
            , measure = m$is_slider_filtering %>% as.integer()
          )

          session$sendCustomMessage("filters_applied", filter_count)

          filter_sum <- filter_count %>% unlist() %>% sum()
          m$is_table_filtered <- filter_sum > 0
        }
      )

      output$filters_applied_ui <- renderUI({
        if(m$is_table_filtered) icon("filter", class = "active") else ""
      })

      # Double click selectize item ---------------------------------------------------------------
      observeEvent(
        input$double_click_selectize_item
        , {
          item <- input$double_click_selectize_item %T>% req()

          if(item$container %>% str_detect("factor_select")){
            message("selected factor: ", item$value)
          }

          if(item$container %>% str_detect("measure_select")){
            slider_field <- item$value
            slider_range <- data[[slider_field]] %>% range(na.rm = TRUE)

            m$measure_slider <- slider_range[2]
            m$measure_slider_range <- slider_range
            m$measure_slider_field <- slider_field
            m$is_slider_filtering <- FALSE
            m$force_slider_update <- m$force_slider_update + 1
          }
        }
      )

      # Command selections ------------------------------------------------------------------------

      # Process command selections and store result in reactive values
      observeEvent(
        list(
          input$factor_select
          , input$measure_select
          , input$factor_filter_select
        )
        , {
          if(command_select(session, "factor_select", k$choices, m$last_factor_select)) return()
          if(command_select(session, "measure_select", k$choices)) return()

          result <- command_filter(session, "factor_filter_select", m$factor_filter_choices_df)
          if(result$is_filtered){
            m$factor_filter_choices_df <- result$choices_df
            return()
          }

          m$factor_select <- input$factor_select
          m$measure_select <- input$measure_select
          m$factor_filter_select <- input$factor_filter_select
        }
      )

      # Reactable data ----------------------------------------------------------------------------

      rt_container <- reactive({

        settings <- rt_settings()

        measure_statistic <- m$measure_statistic_select

        selected <- list(
          factor = m$factor_select
          , measure = m$measure_select
        )
        filtered <- list(
          factor = m$factor_filter_select_df
          , measure = m$measure_filter_df
        )
        slider <- list(
          field = m$measure_slider_field
        )

        is_selected <- selected %>% map(~!is.null(.x))
        is_filtered <- filtered %>% map(~nrow(.x) > 0)

        # Adjust for measure filter - slider having a value doesn't mean it is filtering
        is_filtered$measure <- all(is_filtered$measure, m$is_slider_filtering)

        # Get selected id and detail columns
        id_cols <- settings$identifier_select
        detail_cols <- c(id_cols
                         , selected$factor
                         , selected$measure
                         , m$measure_slider_field) %>% unique()

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
          df <- df %>% apply_measure_filter(filtered, ac)
        }

        # Exit if filtered data has no rows
        has_data <- nrow(df) > 0
        session$sendCustomMessage("spaceman", has_data)
        shiny::validate(
          need(has_data, "No results. Try removing filters...")
        )

        # Get count
        dfc <- df[, .(count = .N), by = c(selected$factor)]

        # Get total count for later use in count heading
        count_total <- dfc$count %>%
          sum() %>%
          format(big.mark = ",")

        if(is_selected$measure){
          dfc <- dfc %>%
            add_statistic_cols(df, measure_statistic, selected)
        }

        # Narrow detail data: df
        df <- df[, ..detail_cols]

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

        # Column groups
        col_grp_summary <- list(
          colGroup_or_null(name = "Factors", selected$factor)
          , colGroup_or_null(name = "Measures", selected$measure)
        ) %>%
          compact()

        col_grp_detail <- list(
          colGroup_or_null(name = "Identifiers", id_cols)
        ) %>%
          compact() %>%
          c(col_grp_summary)

        # Add count total to count column heading
        col_def_summary <- k$column_definitions[names(dfc)]
        col_def_summary$count <- col_def_summary$count %>% list_modify(
          header = paste0('Count<span style = "position: absolute; right: 15px; bottom: 6px;">'
              , count_total
              ,'</span>'))

        # Add measure statistic to column headers
        if(is_selected$measure){
          measure_statistic_name <- k$choices$measure_statistics %>%
            .[. == measure_statistic] %>% names()

          selected$measure %>% walk(
            function(col_name){

              col_def_summary[[col_name]] <<- col_def_summary[[col_name]] %>%
                list_modify(
                  header = with_tooltip(
                    col_name
                    , ac$field_df
                    , measure_statistic_name)
                )
            }
          )
        }

        # Return
        list(
          summary = dfc
          , detail = df
          , columnGroups = list(
            summary = col_grp_summary
            , detail = col_grp_detail
          )
          , columns = list(
            summary = col_def_summary
            , detail = k$column_definitions[names(df)]
          )
        )

      })



      # Reactable ---------------------------------------------------------------------------------
      output$main_rt <- renderReactable({

        rt <- rt_container()
        shiny::validate(
          need(rt, "Loading...")
        )

        view <- m$table_view
        df <- rt[[view]]
        columnGroups <- rt$columnGroups[[view]]
        columns <- rt$columns[[view]]

        # Set up sort order
        sort_select <- m$sort_select[[view]]
        sort_order <- m$sort_order[[view]]

        # Ensure sort cols are present
        cols_select <- names(df)

        in_sort <- sort_select %in% cols_select
        sort_select <- sort_select[in_sort]
        sort_order <- sort_order[in_sort]

        defaultSorted <- sort_order %>%
          as.list() %>%
          set_names(sort_select)

        reactable(
          df
          , columnGroups = columnGroups
          , columns = columns
          , striped = TRUE
          , highlight = TRUE
          , minRows = 3
          , rowClass = JS("function(rowInfo){return rowInfo}")
          , defaultSorted = defaultSorted
        )
      })

      # Observe sort state changed by mouse click event
      observeEvent(
        input$sort_state_change
        , {
          session$sendCustomMessage("get_sort_order", 1)
        }
      )

      # Get sort order
      observeEvent(
        input$sort_order
        , {
          view <- m$table_view
          cols_sort <- input$sort_order
          cols <- rt_container()[[view]] %>% names()

          is_sorted <- cols_sort != "none"

          m$sort_select[[view]] <- cols[is_sorted]
          m$sort_order[[view]] <- cols_sort[is_sorted] %>%
            str_replace("ascending", "asc") %>%
            str_replace("descending", "desc")

        }
      )






    }
  )
}
