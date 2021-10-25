# Table settings

table_settings_ui <- function(id, init){

  ns <- NS(id)

  div(
    dropdown(
      inputId = ns("settings_dropdown")
      , fluidRow(
        column(
          width = 12
          , selectizeInput(
            inputId = ns("sort_by")
            , label = div(icon("sort"), "Sort by")
            , choices = init$choices$measure_date
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
            , label = div(icon("align-left"), "Measure columns with bars")
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
          , selectizeInput(
            inputId = ns("identifier_select")
            , label = div(icon("satellite"), "Identifiers (Details view only)")
            , choices = init$choices$identifier
            , selected = init$choices$identifier[1]
            , multiple = TRUE
            , width = '100%'
            , options = list(
              plugins = list('drag_drop')
            )
          )
        )
      )
      , fluidRow(
        column(
          width = 6
          , radioGroupButtons(
            inputId = ns("slider_handles")
            , label = div(icon("arrows-alt-h"), "Slider handles")
            , choices = c("one", "two") %>%
              set_names(c("One", "Two"))
            , selected = "one"
            , justified = TRUE
          )
        )
        , column(
          width = 6
          , selectInput(
            inputId = ns("max_factor_filter_choices")
            , label = div(icon("filter"), "Factor history limit")
            , choices = 1:50
            , selected = 10
          )
        )
      )

      , style = "simple"
      , icon = div(style = "font-size: 14px; padding-top: 2px; padding-right: 2px;", "...more")
      , status = "default"
      , right = TRUE
      , width = "50vw"
    )
  )


}


table_settings_server <- function(id, init){
  moduleServer(
    id
    , function(input, output, session){

      # Local reactives ---------------------------------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE
        , last_factor_select = NULL
      )

      # Initialise local reactive values ----------------------------------------------------------
      observe({
        if(m$run_once) return()

          m$last_identifier_select <- input$identifier_select

        m$run_once <- TRUE
      })

      observeEvent(
        init()
        , {
          stored <- init()

          # Update selectize inputs
          c("sort_by"
            , "identifier_select"
            , "max_factor_filter_choices"
          ) %>% walk(
            ~updateSelectInput(
              session = session
              , inputId = .x
              , selected = stored[[.x]]
            )
          )
          # Update radio group buttons
          c("bar_option"
            , "slider_handles"
          ) %>% walk(
            ~updateRadioGroupButtons(
              session = session
              , inputId = .x
              , selected = stored[[.x]]
            )
          )

        }, ignoreNULL = TRUE
      )

      # Settings reactive -------------------------------------------------------------------------
      settings <- reactive({

        identifier_select <- input$identifier_select

        is_selected <- list(
          identifier = !is.null(identifier_select)
        )

        # Ensure group selection always has one element
        if(!is_selected$identifier){
          updateSelectizeInput(
            session
            , inputId = "identifier_select"
            , selected = m$last_identifier_select
          )
          return()
        }

        m$last_identifier_select <- identifier_select

        list(
          slider_handles = input$slider_handles
          , sort_by = input$sort_by
          , bar_option = input$bar_option
          , identifier_select = identifier_select
          , max_factor_filter_choices = input$max_factor_filter_choices %>% as.integer()
        )
      })

      # Return value ------------------------------------------------------------------------------

      return(
        settings
      )


    }
  )}



