# Table settings

more_settings_ui <- function(id, init){

  ns <- NS(id)

  # Main UI ---------------------------------------------------------------------------------------
  div(
    dropdown(
      inputId = ns("settings_dropdown")
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
          width = 12
          , selectizeInput(
            inputId = ns("bar_option")
            , label = div(icon("align-left"), "Measure columns with bars")
            , choices = init$choices$bar_option %>%
              add_command_choices("Measure", "count", "all", "inverse", "clear")
            , selected = "count"
            , multiple = TRUE
            , width = '100%'
          )
        )
      )

      , style = "simple"
      , icon = div(class = "more_settings_button", "...more")
      , status = "default"
      , right = TRUE
      , width = "50vw"
    )
  )
}

more_settings_server <- function(id, init){
  moduleServer(
    id
    , function(input, output, session){

      # Local constants ---------------------------------------------------------------------------
      k <- list(
        choices = get_choices()
      )

      # Local reactives ---------------------------------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE

        , bar_option = NULL
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
          c("identifier_select"
            , "bar_option"
          ) %>% walk(
            ~updateSelectInput(
              session = session
              , inputId = .x
              , selected = stored[[.x]]
            )
          )

        }, ignoreNULL = TRUE
      )

      # command selections ------------------------------------------------------------------------

      # Process command selections and store result in reactive values
      observeEvent(
        list(
          # input$sort_by
          input$bar_option
        )
        , {
          if(command_select(session, "bar_option", k$choices)) return()

          m$bar_option <- input$bar_option
        }
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
          identifier_select = identifier_select
          , bar_option = m$bar_option
        )
      })

      # Return value ------------------------------------------------------------------------------

      return(
        settings
      )


    }
  )}



