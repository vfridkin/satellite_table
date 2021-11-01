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

  # Initial default values
  init <- list(
    factor_select = "class_of_orbit"
    , slider_field = ac$field$date_of_launch
    , identifier_select = "current_official_name_of_satellite"
    , slider_handles = "one"
    , bar_option = "count"
    , max_factor_filter_choices = 10
    , field = ac$field
  )

  data <- get_data()
  satellite_table_server("main", init, data)

  observeEvent(
    input$start_help
    , {
      introjs(
        session
        , options = list(
          steps = help_steps()
          , nextLabel = "next"
          , skipLabel = ""
          , showStepNumbers = FALSE
          , showBullets = TRUE
          , disableInteraction = TRUE
        )
      )
    }
  )


}
