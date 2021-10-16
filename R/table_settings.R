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
            , choices = init$choices_value
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
            , label = "Value columns with bars"
            , choices = c("count", "sort", "all") %>%
              set_names(c("Count", "Sorted", "All"))
            , justified = TRUE
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



      # Return value -------------------------------------------------------------------


    }
)}



