# Statistic table
statistic_table_ui <- function(id, field_df){

  ns <- NS(id)

  # Set up choices for selectize inputs ----------------------------------------
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

  # Main UI --------------------------------------------------------------------
  div(
    class = "statistic_container"
    , div(
      class = "statistic__select-box"
      , fluidRow(
        column(
          width = 6
          , selectizeInput(
            inputId = ns("factor_col_select")
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
          width = 6
          , sliderInput(
            inputId = ns("value_slider")
            , label = div(icon("filter"), "Loading...")
            , min = 1
            , max = 10
            , value = 5
            , ticks = FALSE
            , sep = ""
            , animate = TRUE
            , width = '100%'
          )
        )
      )
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
            style = "display: inline-block; margin-top: 5px;"
            , awesomeRadio(
              inputId = ns("value_statistic")
              , label = NULL
              , choices = c("min", "mean", "max", "sd") %>% set_names(
                c("Minimum", "Average", "Maximum", "Standard deviation")
              )
              , selected = "mean"
              , inline = TRUE
              , status = "primary"
            )
          )
          , div(
            style = "display: inline-block;
              right: 0; position: absolute; padding-right: 20px; top: 5px;"
            , icon("ellipsis-v")
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

      # Local constants --------------------------------------------------------
      k <- list(
        column_definitions = get_column_definitions()
      )

      # Local reactive values --------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE
        , last_factor_col_select = NULL
        , slider_field = NULL
        , updated_value_slider_label = NULL
        , slider_is_range = NULL
        , filter_factors = NULL
        , filter_values = NULL
      )

      # Initiatlise ------------------------------------------------------------
      observe({
        if(m$run_once) return()
        m$last_factor_col_select <- input$factor_col_select
        m$slider_field <- init$slider_field$name
        m$slider_is_range <- FALSE
        m$factor_filter <- list()
        m$value_filter <- list()
        m$run_once <- TRUE

      })

      # Reactable data ---------------------------------------------------------
      rt_container <- reactive({

        factor_col_select <- input$factor_col_select
        value_select <- input$value_select

        factor_filter <- m$factor_filter
        value_filter <- m$value_filter

        statistic_function <- function(x){
          fn <- input$value_statistic
          res <- do.call(input$value_statistic, list(x, na.rm = TRUE))
          if(fn == "sd" && class(x) == "Date"){
            res <- res %>% format(big.mark = ",", digits = 0) %>% paste("days")
          }
          res
        }

        # Ensure group selection always has one element
        if(is.null(factor_col_select)){
          updateSelectizeInput(
            session
            , inputId = "factor_col_select"
            , selected = m$last_factor_col_select
          )
          return()
        }

        m$last_factor_col_select <- input$factor_col_select

        # Make a copy of the data so that original is not edited
        df <- data %>% copy()

        # Apply value filters
        if(length(value_filter) > 0){

          comparison <- if(m$slider_is_range) "%between%" else "<="

          filter_col <- m$slider_field
          group_as <- ac$field[[filter_col]]$group_as

          if(group_as == "date"){
            df$year <- df[[filter_col]] %>% format("%Y") %>% as.numeric()
            filter_col <- "year"
          }

          filter_expression <- parse(
            text = glue("{filter_col} {comparison} {value_filter}")
          )

          df <- df[!is.na(df[[filter_col]])]
          df <- df[eval(filter_expression)]
        }

        # Get count
        df_result <- df[, .(count = .N), by = c(factor_col_select)]

        is_value_selected <- !is.null(value_select)
        if(is_value_selected){
          df_val <- df[, lapply(.SD, statistic_function)
                         , by = c(factor_col_select)
                         , .SDcols = value_select
          ][, ..value_select]

          df_result <- df_result %>% cbind(df_val)
        }

        # Order by decreasing count
        df_result <- df_result %>% setorder(-count)

        # Add bars
        df_result$count <- add_bars(df_result$count)

        # Get column definition
        col_def <- k$column_definitions[names(df_result)]

        list(
          data = df_result
          , columns = col_def
        )

      })

      # Change slider definition -----------------------------------------------
      observeEvent(
        m$slider_field
        , {

          # Get range
          slider_range <- data[[m$slider_field]] %>% range(na.rm = TRUE)
          slider_label <- init$field[[m$slider_field]]$display_name %>%
            paste("(range)")

          range_class <- slider_range[1] %>% class()

          if(range_class == "Date"){
            slider_range <- slider_range %>%
              format("%Y") %>%
              as.numeric()
          }

          # update slider
          updateSliderInput(
            session = session
            , inputId = "value_slider"
            , label = slider_label
            , value = slider_range[2]
            , min = slider_range[1]
            , max = slider_range[2]
          )
        }
      )

      # Change slider values ---------------------------------------------------
      observeEvent(
        input$value_slider
        , {

          slider <- input$value_slider
          m$value_filter <- list(slider) %>%
            set_names(m$slider_field)

          # Update label with filter icon - terrible workaround!
          slider_label <- init$field[[m$slider_field]]$display_name %>%
            paste("(range)")

          session$sendCustomMessage(
            'change-slider-label'
            , list(
              id = ns("value_slider")
              , label = slider_label
            )
          )

        }, ignoreInit = TRUE
      )


      # Reactable --------------------------------------------------------------
      output$statistic_rt <- renderReactable({

        rt <- rt_container()
        validate(
          need(rt, "Loading...")
        )

        col_def <- rt$columns %>% copy()
        # col_def[1][[1]]$footer <- "Total"

        reactable(
          rt$data
          , columns = col_def
          , striped = TRUE
          , highlight = TRUE
          , minRows = 10
          # , defaultColDef = colDef(
          #   footerStyle = list(fontWeight = "bold")
          #   )
        )

      })

    }
  )


}
