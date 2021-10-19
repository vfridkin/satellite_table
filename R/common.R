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
          , html = TRUE
          # , footer = this_footer
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

# Add column name to cells in column
add_col_name <- function(col, col_name){
  paste0('<div data-col-name="',col_name ,'">',col ,'</div>')
}

# Add bars to column cells
add_bars <- function(col, col_name){

  bar_html <- function(percent, value, pad_width){

    sort_value <- formatC(col, width = pad_width, format = "d", flag = "0")

    paste0('<div class="rt-td rt-align-right"
sort-value="', sort_value,'"
role="cell"
style="
flex: 100 0 auto;
background-image: linear-gradient(to right
, #ff7a14, #8a3c00 '
, percent, '%
, transparent '
, percent, '%);
transition: all 1s;
background-size: 100% 75%;
border-top: transparent;
background-repeat: no-repeat;
background-position: center center;">
<div class="rt-td-inner" style = "background: transparent">',value ,'</div>
</div>'
    ) %>% str_remove_all("\n")
  }

  max_val <- max(col)

  percent <- (100*col/max_val) %>% round()
  pad_width <- nchar(col) %>% max()

  bar_html(percent, col, pad_width)

}

# Combine adding col names and bars to cells
add_html_to_cells <- function(df, settings, selected){

  names(df) %>% walk(
    function(col_name){

      col <- df[[col_name]]

      bars <- "count"

      fn <- if(col_name %in% bars) add_bars else add_col_name
      col <- col %>% fn(col_name)

      df[[col_name]] <<- col
    }
  )

  df
}

# Subset df by value filter from slider definition and value
apply_value_filter <- function(df, slider, filtered, ac){

  comparison <- if(slider$is_range) "%between%" else "<="

  filter_col <- slider$field
  group_as <- ac$field[[filter_col]]$group_as

  if(group_as == "date"){
    df$year <- df[[filter_col]] %>% format("%Y") %>% as.numeric()
    filter_col <- "year"
  }

  filter_expression <- parse(
    text = glue("{filter_col} {comparison} {filtered$value}")
  )

  # Remove NA values
  df <- df[!is.na(df[[filter_col]])]

  df[eval(filter_expression)]
}

# Column bind summary statistics to dataframe summarised by count
add_statistic_cols <- function(dfc, df, value_statistic, selected){
  statistic_function <- function(x){
    res <- do.call(value_statistic, list(x, na.rm = TRUE))
    if(value_statistic == "sd" && class(x) == "Date"){
      res <- res %>%
        format(big.mark = ",", digits = 0) %>%
        paste("days")
    }
    res
  }

  cols <- selected$value

  df_val <- df[, lapply(.SD, statistic_function)
               , by = c(selected$factor)
               , .SDcols = cols
  ][, ..cols]

  dfc %>% cbind(df_val)
}


