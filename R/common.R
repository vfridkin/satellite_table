# Common functions

# RUN APP -----------------------------------------------------------------------------------------

# Save and rerun app - easier than clicking the run button :)
sar <- function(){
  rstudioapi::documentSaveAll()
  runApp()
}

# SOURCE FILES/CONFIG/DATA ------------------------------------------------------------------------------

load_styles <- function(){
  div(
    includeCSS("www/animations.css")
    , includeCSS("www/main.css")
    , includeCSS("www/launch_screen.css")
    , includeCSS("www/table_screen.css")
  )
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

  # # Text version - is an older version so using XL below instead
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

  measure_fields <- field_df[group_as == "measure"]$name
  date_fields <- field_df[group_as == "date"]$name

  # Set measure fields to numeric
  measure_fields %>%
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

  # Replace NA factors with 'unknown'
  factor_cols <- ac$field_df[group_as == "factor"]$name
  for (col in factor_cols) df[is.na(get(col)), (col) := "unknown"]

  df
}

get_column_definitions <- function(ac){

  df <- ac$field_df

  df[, .(display_name)] %>%
    pmap(
      function(display_name){
        colDef(
          name = display_name
          , filterable = FALSE
          , html = TRUE
        )
      }
    ) %>%
    set_names(df$name)
}

get_choices <- function(){

  df <- ac$field_df[order(display_name),]
  choices <- c("identifier", "factor", "measure", "date") %>%
    {set_names(
      map(.,
          function(var){
            df[group_as == var] %>%
              {set_names(.$name, .$display_name)}
          }
      ),.
    )}

  # Combine measures with dates
  choices$measure_date <- c(choices$measure, choices$date) %>% sort()

  choices$factor_select <- choices$factor
  choices$measure_select <- choices$measure_date
  choices$sort_by <- choices$measure_date
  choices$bar_option <- choices$measure_date

  choices
}

add_special_choices <- function(choices, choices_name, ...){

  special <- c(...)

  special_choices <- c(
    "Count" = "count"
    , "All" = "all"
    , "Inverse" = "inverse"
    , "Clear" = "clear"
  ) %>% .[. %in% c(...)]

  special_choices <- special_choices %>%
    map_chr(
      function(x){
        if(x == "count") return(x)
        paste0(x, ac$special_function_suffix)
      }
    )

  list(choices, special_choices) %>%
    set_names(c(choices_name, "Special"))
}

special_select <- function(session, id, choices, last_factor_select = ""){

  select <- session$input[[id]]
  sfx <- ac$special_function_suffix

  nothing_special <- any(
    is.null(select)
    , !any(select %>% str_detect(sfx))
  )

  if(nothing_special) return(FALSE)

  # Get first special function - should only be one anyway
  special <- select %>% keep(~str_detect(.x, sfx)) %>% .[1]
  special <- special %>% str_remove(sfx)

  # Default to no selection
  selected <- ""

  if(special == "all"){
    selected <- choices[[id]]
  }

  if(special == "inverse"){
    selected <- choices[[id]] %>% setdiff(select)
  }

  # Replace null selection with empty string to enable update
  if(length(selected) == 0){
    selected <- ""
  }

  # Leave one remainder if factors
  if(selected == "" && id == "factor_select"){
    selected <- last_factor_select[1]
  }

  updateSelectizeInput(
    session = session
    , inputId = id
    , selected = selected
  )

  return(TRUE)
}

# DATA I/O ----------------------------------------------------------------------------------------

to_local_storage_id <- function(id){
  paste0(ac$local_storage_id_prefix, id)
}

set_local_storage <- function(id, data, session){
  ls_id <- id %>% to_local_storage_id()
  json_string <- data %>% toJSON() %>% toString()
  session$sendCustomMessage("set_local_storage", list(id = ls_id, data = json_string))
}

get_local_storage <- function(id, session){
  ls_id <- id %>% to_local_storage_id()
  session$sendCustomMessage("get_local_storage", ls_id)
}

set_setting_circle <- function(id, session){
  session$sendCustomMessage("set_setting_circle", id)
}

# USER INTERFACE ----------------------------------------------------------------------------------

# Circles for saving settings
circle_icon <- function(id, fill = FALSE){
  class_fill <- if(fill) "fas" else "far"
  margin_right <- if(id %% 4 == 0) "10px" else "0"
  HTML(paste0('
      <i
        data-id="',id ,'"
        class="setting-circle ',class_fill ,' fa-circle"
        style="padding: 5px; margin-right: ', margin_right,'"
      ></i>'
  ))
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

  # Remove commas and make numeric
  values <- col %>%
    str_remove_all(",") %>%
    as.numeric()

  max_val <- max(values)

  percent <- (100*values/max_val) %>% round()
  pad_width <- nchar(col) %>% max()

  bar_html(percent, col, pad_width)

}

# Format column before mixing data values with HTML - the usual column
# definitions are applied afterwards - in the reactable function, which
# may not work if the values have been mixed with HTML.
apply_column_definitions <- function(df, field_config){

  names(df) %>% walk(
    function(col_name){
      config <- field_config[[col_name]]
      display_decimals <- config$display_decimals

      col <- df[[col_name]]

      if(!is.na(display_decimals)){
        col <- col %>% format(big.mark = ",", digits = display_decimals)
        df[[col_name]] <<- col
      }
    }
  )

  df
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

# TABLE FILTERS -----------------------------------------------------------------------------------

# Subset df by factor filter
apply_factor_filter <- function(df, filtered){
  filtered$factor %>% pmap(
    function(name, value){
      df <<- df[get(name) == value]
    }
  )
  df
}

# Subset df by measure filter from slider definition and value
apply_measure_filter <- function(df, slider, filtered, ac){

  comparison <- if(slider$handles == "one") "<=" else "%between%"
  filter <- filtered$measure[1]

  filter_col <- filter$name
  group_as <- ac$field[[filter_col]]$group_as

  if(group_as == "date"){
    df$year <- df[[filter_col]] %>% format("%Y") %>% as.numeric()
    filter_col <- "year"
  }

  filter_expression <- parse(
    text = glue("{filter_col} {comparison} {filter$value}")
  )

  # Remove NAs
  df <- df[!is.na(df[[filter_col]])]

  df[eval(filter_expression)]
}

# DATA WRANGLING ----------------------------------------------------------------------------------

# Column bind summary statistics to dataframe summarised by count
add_statistic_cols <- function(dfc, df, measure_statistic, selected){
  statistic_function <- function(x){
    res <- do.call(measure_statistic, list(x, na.rm = TRUE))
    if(measure_statistic == "sd" && class(x) == "Date"){
      res <- res %>%
        format(big.mark = ",", digits = 0) %>%
        paste("days")
    }
    res
  }

  cols <- selected$measure

  df_val <- df[, lapply(.SD, statistic_function)
               , by = c(selected$factor)
               , .SDcols = cols
  ][, ..cols]

  dfc %>% cbind(df_val)
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

# Round up/down to significant digits
floor_dec <- function(x, digits=1) round(x - 5*10^(-digits-1), digits)
ceiling_dec <- function(x, digits=1) round(x + 5*10^(-digits-1), digits)

# String manipulation -----------------------------------------------------------------------------

add_s <- function(x){
  if(x > 1) "s" else ""
}

