# Table settings

table_settings_ui <- function(id, init){

  ns <- NS(id)

  div(
    dropdown(
      inputId = ns("settings_dropdown")
      , fluidRow(
        column(
          width = 12
          , radioGroupButtons(
            inputId = ns("slider_handles")
            , label = "Slider handles"
            , choices = c("one", "two") %>%
              set_names(c("One", "Two"))
            , selected = "one"
            , justified = TRUE
          )
        )
      )
      , fluidRow(
        column(
          width = 12
          , selectizeInput(
            inputId = ns("sort_by")
            , label = "Sort by"
            , choices = init$choices_measure
            , selected = ""
            , multiple = TRUE
            , width = '100%'
          )
        )
      )
      , fluidRow(
        column(
          width = 12
          , radioGroupButtons(
            inputId = ns("bar_option")
            , label = "Measure columns with bars"
            , choices = c("count", "sort", "all") %>%
              set_names(c("Count", "Sorted", "All"))
            , selected = "count"
            , justified = TRUE
          )
        )
      )
      , fluidRow(
        column(
          width = 12
          , selectInput(
            inputId = ns("max_factor_filter_choices")
            , label = "Maxiumum factor filter choices"
            , choices = 1:50
            , selected = 10
          )
        )
      )
      , style = "simple"
      , icon = icon(init$icon)
      , status = "default"
      # , tooltip = tooltipOptions(placement = "bottom", title = init$tooltip, html = FALSE)
      , right = TRUE
      , width = "50vw"
    )
  )


}


table_settings_server <- function(id){
  moduleServer(
    id
    , function(input, output, session){

      # Local reactives ---------------------------------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE
        , out = NULL
      )

      # observeEvent(
      #   list(
      #     input$slider_handles
      #     , input$sort_by
      #     , input$bar_option
      #   )
      #   , {
      #     browser()
      #   }
      # )

      settings <- reactive({

        list(
          slider_handles = input$slider_handles
          , sort_by = input$sort_by
          , bar_option = input$bar_option
          , max_factor_filter_choices = input$max_factor_filter_choices %>% as.integer()
        )
      })




      # Return value ------------------------------------------------------------------------------

      return(
        settings
      )


    }
  )}



