# Server main file

#  .d8888b.                                              8888888b.
# d88P  Y88b                                             888   Y88b
# Y88b.                                                  888    888
#  "Y888b.    .d88b.  888d888 888  888  .d88b.  888d888  888   d88P
#     "Y88b. d8P  Y8b 888P"   888  888 d8P  Y8b 888P"    8888888P"
#       "888 88888888 888     Y88  88P 88888888 888      888 T88b
# Y88b  d88P Y8b.     888      Y8bd8P  Y8b.     888  d8b 888  T88b
#  "Y8888P"   "Y8888  888       Y88P    "Y8888  888  Y8P 888   T88b

function(input, output, session){

  # Satellite table -------------------------------------------------------------------------------

  # Set up initialisation for satellite server
  # It is used if stored data does not exist
  init <- tibble::lst(
    factor_select = "class_of_orbit"
    , measure_slider_field = ac$field$date_of_launch
    , measure_statistic_select = "mean"
    , identifier_select = "current_official_name_of_satellite"
    , sort_select = list(
      summary = "count"
      , detail = identifier_select
    )
    , sort_order = list(
      summary = "desc"
      , detail = "asc"
    )
    , bar_option = "count"
    , max_factor_filter_choices = 10
    , field = ac$field
  )

  # Get satellite data and start the main server
  data <- get_data()
  satellite_table_server("main", init, data)

  # Saved info - opened by clicking on the satellite ----------------------------------------------
  saved_info_server("saved")

  # Help splash - for new users to understand to click the moon and satellite ---------------------
  observeEvent(
    input$splash_close
    , {
      splash_check <- input$dont_show_splash_again_check
      set_local_storage("splash_check", splash_check, session)
    }
  )

  # The main tour of the app - activated by the moon ----------------------------------------------
  observeEvent(
    input$start_help
    , {

      #Ensure table controls are visible
      updateMaterialSwitch(
        session = session
        , inputId = "main-view_controls_switch"
        , value = TRUE
      )

      # Start tour
      introjs(
        session
        , options = list(
          steps = help_steps()
          , nextLabel = "Next"
          , skipLabel = "x"
          , showStepNumbers = FALSE
          , showBullets = TRUE
          , disableInteraction = TRUE
        )
      )
    }
  )
}
