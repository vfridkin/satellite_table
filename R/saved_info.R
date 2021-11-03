# Saved information

saved_info_ui <- function(id){

  ns <- NS(id)

  div(
    class = "saved-info"
    , h3("Saved table views")
    , reactableOutput(ns("saved_rt"))
  )
}

saved_info_server <- function(id, init, data){
  moduleServer(
    id
    , function(input, output, session){

      ns <- session$ns


      output$saved_rt <- renderReactable({

        reactable(iris)

      })




    }
  )



}
