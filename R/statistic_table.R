# Statistic table
statistic_table_ui <- function(id, field_df){

  ns <- NS(id)

  choices <- c("group", "value", "date") %>%
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

  div(
    class = "statistic_countainer"
    , div(
      class = "statistic__select-box"
      , selectizeInput(
        inputId = ns("group_select")
        , label = div(icon("table"), "Rows")
        , choices = choices$group
        , selected = choices$group[1]
        , multiple = TRUE
        , options = list(plugins = list('drag_drop'))
      )
      , selectizeInput(
        inputId = ns("value_select")
        , label = div(icon("ruler-vertical"), "Values")
        , choices = choices_value
        , selected = ""
        , multiple = TRUE
        , options = list(plugins = list('drag_drop'))
      )
      , sliderInput(
        inputId = ns("value_slider")
        , label = "Loading..."
        , min = 1
        , max = 10
        , value = 5
        , ticks = FALSE
        , sep = ""
        , animate = TRUE
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

      m <- reactiveValues(
        run_once = FALSE
        , last_group_select = NULL
        , slider_field = NULL
      )

      observe({
        if(m$run_once) return()
          m$last_group_select <- input$group_select
          m$slider_field <- init$slider_field$name
        m$run_once <- TRUE
      })

      rt_data <- reactive({

        group_select <- input$group_select
        value_select <- input$value_select

        # Ensure group selection always has one element
        if(is.null(group_select)){
          updateSelectizeInput(
            session
            , inputId = "group_select"
            , selected = m$last_group_select
          )
          return()
        }

        m$last_group_select <- input$group_select

        df <- data[, .(count = .N), by = c(group_select)]

        is_value_selected <- !is.null(value_select)
        if(is_value_selected){
          fn <- function(x) mean(x, na.rm = TRUE)
          df_val <- data[, lapply(.SD, fn)
                         , by = c(group_select)
                         , .SDcols = value_select
          ][, ..value_select]

          df <- df %>% cbind(df_val)
        }

        df %>% setorder(-count)
      })

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


      output$statistic_rt <- renderReactable({

        df <- rt_data()
        validate(
          need(df, "Loading...")
        )

        reactable(
          df
        )

      })

    }
  )


}
