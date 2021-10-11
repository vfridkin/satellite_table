# Run example
run_example <- function(){
  rstudioapi::documentSaveAll()
  source('global.R')
  source("example/example_table.R")
  shiny::shinyApp(ui, server)
}

run_example()
