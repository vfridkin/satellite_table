# Common functions

sar <- function(){
  rstudioapi::documentSaveAll()
  runApp()
}

load_config <- function(){

  config <- read_yaml("config/config.yaml")

  field_df <- fread("config/fields.csv")
  field <- field_df %>% {set_names(purrr::transpose(.), .$name)}

  config %>%
    list_modify(
      field = field
      , field_df = field_df
    )
}

get_data <- function(){
  df <- fread("data/UCS-Satellite-Database-1-1-2021.txt", colClasses = "character") %>%
    janitor::remove_empty(which = "cols") %>%
    replace_col_names() %>%
    janitor::clean_names()

  value_fields <- ac$field_df[group_as == "value"]$name
  date_fields <- ac$field_df[group_as == "date"]$name

  value_fields %>%
    walk(
      ~{
        df[[.x]] <<- df[[.x]] %>%
        str_replace_all(",", "") %>%
        as.numeric()
      }
    )
  date_fields %>%
    walk(
      ~{
        df[[.x]] <<- df[[.x]] %>%
          as.Date(format = "%m/%d/%Y")
        }
    )

  df
}


# Col name of form V123 replace with preceding name
replace_col_names <- function(df){
  df_names <- names(df)
  for(i in 2:length(df_names)){
    x <- df_names[i]
    if(str_detect(x, "^V\\d+$")){
      df_names[i] <- df_names[i-1]
    }
  }
  names(df) <- df_names
  df
}
