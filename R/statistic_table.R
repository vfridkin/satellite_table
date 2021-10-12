# Statistic table
statistic_table_ui <- function(id, field_df){

  ns <- NS(id)

  # Set up choices for selectize inputs ----------------------------------------
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
  choices_value <- c(choices$date, choices$value)

  # Main UI --------------------------------------------------------------------
  div(
    class = "statistic_container"
    , div(
      class = "statistic__select-box"
      , fluidRow(
        column(
          width = 6
          , selectizeInput(
            inputId = ns("factor_select")
            , label = div(icon("table"), "Factors")
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
            , label = div(icon("ruler-vertical"), "Values")
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
          , sliderInput(
            inputId = ns("value_slider")
            , label = "Loading..."
            , min = 1
            , max = 10
            , value = 5
            , ticks = FALSE
            , sep = ""
            , animate = TRUE
            , width = '100%'
          )
        )
        , column(
          width = 6
          , checkboxGroupButtons(
            inputId = ns("value_statistic")
            , label = "Statistic"
            , choices = c("min", "mean", "max", "sd")
            , justified = TRUE
            , checkIcon = list(
              yes = icon("ok", lib = "glyphicon")
            )
          )
        )
      )
    )
    , div(
      class = "statistic__table-box"
      , reactableOutput(
        outputId = ns("statistic_rt")
      )
    )
  )
}

statistic_table_server <- function(id, init, data){
  moduleServer(
    id
    , function(input, output, session){

      # Local constants --------------------------------------------------------
      k <- list(
        column_definitions = get_column_definitions()
      )

      # Local reactive values --------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE
        , last_factor_select = NULL
        , slider_field = NULL
      )

      # Initiatlise ------------------------------------------------------------
      observe({
        if(m$run_once) return()
        m$last_factor_select <- input$factor_select
        m$slider_field <- init$slider_field$name
        m$run_once <- TRUE
      })

      # Reactable data ---------------------------------------------------------
      rt_container <- reactive({

        factor_select <- input$factor_select
        value_select <- input$value_select

        # Ensure group selection always has one element
        if(is.null(factor_select)){
          updateSelectizeInput(
            session
            , inputId = "factor_select"
            , selected = m$last_factor_select
          )
          return()
        }

        m$last_factor_select <- input$factor_select

        df <- data[, .(count = .N), by = c(factor_select)]

        is_value_selected <- !is.null(value_select)
        if(is_value_selected){
          fn <- function(x) mean(x, na.rm = TRUE)
          df_val <- data[, lapply(.SD, fn)
                         , by = c(factor_select)
                         , .SDcols = value_select
          ][, ..value_select]

          df <- df %>% cbind(df_val)
        }

        # Order by decreasing count
        df <- df %>% setorder(-count)

        # Get column definition
        col_def <- k$column_definitions[names(df)]

        list(
          data = df
          , columns = col_def
        )

      })

      # Change slider values ---------------------------------------------------
      observeEvent(
        m$slider_field
        , {

          # Get range
          slider_range <- data[[m$slider_field]] %>% range(na.rm = TRUE)
          slider_label <- init$field[[m$slider_field]]$display_name |>
            paste("(range)")

          range_class <- slider_range[1] %>% class()

          if(range_class == "Date"){
            slider_range <- slider_range %>%
              format("%Y") %>%
              as.integer()
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

      # Reactable --------------------------------------------------------------
      output$statistic_rt <- renderReactable({

        rt <- rt_container()
        validate(
          need(rt, "Loading...")
        )

        reactable(
          rt$data
          , columns = rt$columns
        )

      })

    }
  )


}
