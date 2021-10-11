# Statistic table
statistic_table_ui <- function(id, field_df){

  ns <- NS(id)

  choices <- c("group", "value") %>%
    {set_names(
      map(.,
          function(var){
            field_df[group_as == var] %>%
              {set_names(.$name, .$display_name)}
          }
      ),.
    )}

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
        , choices = choices$value
        , selected = ""
        , multiple = TRUE
        , options = list(plugins = list('drag_drop'))
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

statistic_table_server <- function(id, data){
  moduleServer(
    id
    , function(input, output, session){

      m <- reactiveValues(
        run_once = FALSE
        , last_group_select = NULL
      )

      observe({
        if(m$run_once) return()
          m$last_group_select <- input$group_select
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
