# Table settings

more_settings_ui <- function(id, init){

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
            , choices = init$choices$sort_by %>%
              add_command_choices("Measure", "count", "clear")
            , selected = "count"
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
        style = "display: none;" # TODO: Complete multi slider handle option
        , column(
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

        , sort_by = NULL
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
          c("sort_by"
            , "bar_option"
            , "identifier_select"
            # , "max_factor_filter_choices"
          ) %>% walk(
            ~updateSelectInput(
              session = session
              , inputId = .x
              , selected = stored[[.x]]
            )
          )
          c(
            "slider_handles"
          ) %>% walk(
            ~updateRadioGroupButtons(
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
          input$sort_by
          , input$bar_option
        )
        , {
          if(command_select(session, "sort_by", k$choices)) return()
          if(command_select(session, "bar_option", k$choices)) return()

          m$sort_by <- input$sort_by
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
          sort_by = m$sort_by
          , bar_option = m$bar_option
          , identifier_select = identifier_select
          , slider_handles = input$slider_handles
          , max_factor_filter_choices = input$max_factor_filter_choices %>% as.integer()
        )
      })

      # Return value ------------------------------------------------------------------------------

      return(
        settings
      )


    }
  )}



