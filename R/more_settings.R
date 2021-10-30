# Table settings

more_settings_ui <- function(id, init){

  ns <- NS(id)

  sort_choices <- init$choices$sort_by
  # %>% add_command_choices("Measure", "count", "clear")

  icons <- rep("arrow-down", length(sort_choices))

  div(
    dropdown(
      inputId = ns("settings_dropdown")
      , fluidRow(
        column(
          width = 12
          , selectInputWithIcons(
              inputId = ns("sort_by")
              , div(icon("sort"), "Sort by")
              , labels = names(sort_choices)
              , values = sort_choices
              , icons = icons
              , iconStyle = "font-size: 14px; background-color: transparent;"
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

      # Local functions ---------------------------------------------------------------------------
      set_sort_order <- function(sort_message){
        session$sendCustomMessage("set_sort_order", sort_message)
      }

      get_sort_order <- function(){
        session$sendCustomMessage("get_sort_order", 0)
      }

      # Local reactives ---------------------------------------------------------------------------
      m <- reactiveValues(
        run_once = FALSE

        , sort_by = NULL
        , sort_order = NULL
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
            , "max_factor_filter_choices"
          ) %>% walk(
            ~updateSelectInput(
              session = session
              , inputId = .x
              , selected = stored[[.x]]
            )
          )

          sort_message <- list(
            selected = stored$sort_by
            , order = stored$sort_order
          )

          set_sort_order(sort_message)

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

      # Sort order from JS ------------------------------------------------------------------------

      observeEvent(
        input$sort_item_change
        , {
          change <- input$sort_item_change
          message(change$item, " ", change$order)
          get_sort_order()
        }
      )

      observeEvent(
        m$sort_by
        , get_sort_order()
      )

      observeEvent(
        input$sort_order
        , {
          m$sort_order <- input$sort_order %>% map_int(
            function(x){
              if(x == "up") 1L else -1L
            }
          )
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

        # Ensure sort_by and sort_order are same dimensions
        valid_sort <- length(m$sort_by) == length(m$sort_order)
        if(!valid_sort) return()

        list(
          sort_by = m$sort_by
          , sort_order = m$sort_order
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



