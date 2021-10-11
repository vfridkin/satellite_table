# Example of table

# Playground
if(FALSE){
  data <- get_data()

  group_fields <- ac$field_df[group_as == "group"]$name
  value_fields <- ac$field_df[group_as == "value"]$name

  # Count single field
  g1 <- group_fields[1]
  rt_g1 <- data[, .(count = .N), by = c(g1)] %>% setorder(-count)

  # Count all fields
  gAll <- group_fields
  rt_gAll <- data[, .(count = .N), by = gAll] %>% setorder(-count)

  # Mean single field
  v1 <- value_fields[1]
  exp1 <- glue("mean({v1}, na.rm = TRUE)")
  new_names <- c(g1, "count", v1)

  rt_g1_v1 <- data[, .(count = .N, eval(parse(text = exp1))), by = c(g1)] %>%
    setorder(-count) %>%
    set_names(new_names)

  reactable(rt_g1_v1)

  # One group field, Mean all value field
  g1 <- group_fields[1]
  vAll <- value_fields
  fn <- function(x) mean(x, na.rm = TRUE)

  rt_g1_Count <- data[, .(count = .N), by = c(g1)]
  rt_g1_vAll <- data[, lapply(.SD, fn), by = c(g1), .SDcols = vAll][, ..vAll]

  rt_cbind <-  rt_g1_Count %>% cbind(rt_g1_vAll) %>% setorder(-count)
}

ui <- fluidPage(

  statistic_table_ui("example", ac$field_df)

)

server <- function(input, output, session){

  init <- list(
    slider_field = ac$field$date_of_launch
    , field = ac$field
  )
  data <- get_data()
  statistic_table_server("example", init, data)

}


