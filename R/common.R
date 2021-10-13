# Common functions

# Save and rerun app - easier than clicking the run button :)
sar <- function(){
  rstudioapi::documentSaveAll()
  runApp()
}

# App configuration
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

  # # Text version - found to have older version
  # if(FALSE){
  #   df <- fread("data/UCS-Satellite-Database-1-1-2021.txt", colClasses = "character") %>%
  #     janitor::remove_empty(which = "cols") %>%
  #     replace_col_names() %>%
  #     janitor::clean_names()
  # }

  # Read and replace column names
  df <- readxl::read_excel("data/UCS-Satellite-Database-5-1-2021.xls") %>%
    setDT() %>%
    janitor::remove_empty(which = "cols") %>%
    replace_col_names() %>%
    janitor::clean_names()

  field_df <- ac$field_df[name %in% names(df)]

  value_fields <- field_df[group_as == "value"]$name
  date_fields <- field_df[group_as == "date"]$name

  # Set value fields to numeric
  value_fields %>%
    walk(
      ~{
        df[[.x]] <<- df[[.x]] %>%
          str_replace_all(",", "") %>%
          str_extract("\\-*\\d+\\.*\\d*") %>%
          as.numeric()
      }
    )

  # Set date fields to Date
  date_fields %>%
    walk(
      ~{
        df[[.x]] <<- df[[.x]] %>%
          as.Date(format = "%m/%d/%Y")
      }
    )

  df
}

get_column_definitions <- function(){

  df <- ac$field_df

  df[, .(display_name, display_decimals)] %>%
    pmap(
      function(display_name, display_decimals){

        this_format <- if(is.na(display_decimals)){NULL} else {
          colFormat(separators = TRUE, digits = display_decimals)
        }

        this_footer <- if(display_name != "Count"){NULL} else {
          footer = function(values) formatC(sum(values), big.mark = ",", digits = 0)
        }

        colDef(
          name = display_name
          , filterable = FALSE
          , format = this_format
          , footer = this_footer
        )
      }
    ) %>%
    set_names(df$name)
}


# Col name of form ...12 replace with preceding name
replace_col_names <- function(df){
  df_names <- names(df)
  for(i in 2:length(df_names)){
    x <- df_names[i]
    if(str_detect(x, "^\\.\\.\\.\\d+$")){
      df_names[i] <- df_names[i-1]
    }
  }
  names(df) <- df_names
  df
}
