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
    , includeCSS("www/help.css")
    , includeCSS("www/introjs.css")
  )
}


# App configuration
load_config <- function(){

  config <- read_yaml("config/config.yaml")

  field_df <- fread("config/fields.csv")
  field <- field_df %>% {set_names(purrr::transpose(.), .$name)}
  lookup_df <- fread("config/lookup.csv")

  config %>%
    list_modify(
      field_df = field_df
      , field = field
      , lookup_df = lookup_df
    )
}

get_data <- function(){

  # Read and replace column names
  df <- readxl::read_excel("data/UCS-Satellite-Database-1-1-2022.xls") %>%
    setDT() %>%
    janitor::remove_empty(which = "cols") %>%
    replace_col_names() %>%
    janitor::clean_names()

  # Remove entries without class of orbit
  df <- df[!is.na(class_of_orbit),]

  # Add year and month of launch
  df <- df[, c("year_of_launch", "month_of_launch", "month_num") := list(
    date_of_launch %>% format("%Y")
    , date_of_launch %>% format("%B")
    , date_of_launch %>% format("%m")
  )]

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

  df[, .(name, display_name)] %>%
    pmap(
      function(name, display_name){

        colDef(
          name = display_name
          , filterable = FALSE
          , html = TRUE
          , header = with_tooltip(name, df)
        )
      }
    ) %>%
    set_names(df$name)

}

with_tooltip <- function(col_name, field_df, sub_heading = NULL) {
  display_name <- field_df[name == col_name]$display_name
  description <- field_df[name == col_name]$description
  value <- display_name
  if(!is.null(sub_heading)){
    value <- paste0(value, '<div class="measure-statistic-subheader">'
                    , sub_heading
                    ,'</div>')
  }
  HTML(paste0('<div style="cursor: help;" data-tooltip="'
              , description
              ,'">'
              , value
              ,'</div>
    ')
  )
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

  # Combine measures with dates for measures selection
  md <- c(choices$measure, choices$date) %>% sort()

  # Measure statistics
  measure_statistics <- c("min", "mean", "max", "sd") %>%
    set_names(c("Minimum", "Average", "Maximum", "Standard deviation"))

  # Return with target widget ids
  choices %>% list_modify(
    factor_select = choices$factor
    , measure_select = md
    , measure_statistics = measure_statistics
    , bar_option = choices$measure
  )
}

add_command_choices <- function(choices, choices_name, ...){

  has_no_choices <- is.null(choices) || length(choices) == 0 || choices == ""
  if(has_no_choices) return(character(0))

  command <- c(...)

  command_choices <- c(
    "Count" = "count"
    , "All" = "all"
    , "Inverse" = "inverse"
    , "Clear" = "clear"
    , "Delete selected" = "delete_selected"
    , "Delete all" = "delete_all"
  ) %>% .[. %in% c(...)]

  command_choices <- command_choices %>%
    map_chr(
      function(x){
        if(x == "count") return(x)
        paste0(x, ac$command_function_suffix)
      }
    )

  list(choices, command_choices) %>%
    set_names(c(choices_name, "Commands"))
}

add_factor_filter_commands <- function(choices){
  choices %>%
    add_command_choices("Factor filters", "clear", "delete_selected", "delete_all")
}

get_command_from_id <- function(session, id){

  select <- session$input[[id]]
  sfx <- ac$command_function_suffix

  command_found <- all(
    !is.null(select)
    , any(select %>% str_detect(sfx))
  )

  command <- NULL
  if(command_found){
    command <- select %>%
      keep(~str_detect(.x, sfx)) %>%
      .[1] %>%
      str_remove(sfx)
  }

  list(
    name = command
    , select = select
  )
}


# This handles changing the choices
command_filter <- function(session, id, choices_df = ""){

  command <- get_command_from_id(session, id)

  if(is.null(command$name)) return(
    list(is_filtered = FALSE)
  )

  # Default to no selection
  selected <- ""

  if(command$name == "delete_selected"){
    choices_df <- choices_df[!input_name %in% command$select]
  }

  if(command$name == "delete_all"){
    choices_df <- choices_df[0, ]
  }

  # Convert from dataframe to vector
  choices <- choices_df$input_name %>% set_names(choices_df$input_display)

  updateSelectizeInput(
    session = session
    , inputId = id
    , selected = selected
    , choices = choices %>% add_factor_filter_commands()
  )

  return(
    list(is_filtered = TRUE, choices_df = choices_df)
  )

}

# This handles changing the selection, keeping choices constant
command_select <- function(session, id, choices = "", last_factor_select = ""){

  command <- get_command_from_id(session, id)

  if(is.null(command$name)) return(FALSE)

  # Default to no selection
  selected <- ""

  if(command$name == "all"){
    selected <- choices[[id]]
  }

  if(command$name == "inverse"){
    selected <- choices[[id]] %>% setdiff(command$select)
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

get_local_storage_multi <- function(ids, session){
  ls_ids <- ids %>% map(to_local_storage_id)
  session$sendCustomMessage("get_local_storage_multi", ls_ids)
}

remove_local_storage <- function(id, session){
  ls_id <- id %>% to_local_storage_id()
  session$sendCustomMessage("remove_local_storage", ls_id)
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

  lookup <- ac$lookup_df
  lookup_cols <- lookup$name %>% unique()
  if(col_name %in% lookup_cols){
    df <- lookup[name == col_name, .(value, value_description)] %>%
      .[,
        col_with_desc := paste0('<div style = "color: inherit;">'
                                ,value
                                ,'<div style="font-size: 10px; color: inherit;">'
                                ,value_description
                                ,'</div></div>')
      ]

    col <- data.table(value = col)[df, on = "value", desc := i.col_with_desc]$desc

  }

  sort_attribute <- ""
  if(col_name == "month_of_launch"){
    sort_value <- match(col, month.name) %>%
      formatC(width = 2, format = "d", flag = "0")
    sort_attribute <- paste0('sort-value="', sort_value, '"')
  }

  paste0('<div ', sort_attribute, '  data-col-name="',col_name ,'">',col ,'</div>')

}

# Add bars to column cells
add_bars <- function(col, col_name){

  bar_html <- function(percent, value, pad_width){

    sort_value <- formatC(col, width = pad_width, format = "d", flag = "0")

    # Get bar colours
    field_def <- ac$field_df[name == col_name]
    bar_color <- list(
      left = field_def$bar_colour_left
      , right = field_def$bar_colour_right
    )

        paste0('<div class="rt-td rt-align-right"
sort-value="', sort_value,'"
role="cell"
style="
flex: 100 0 auto;
background-image: linear-gradient(to right
, ', bar_color$left, '
, ', bar_color$right, ' '
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

  is_na_value <- is.na(values)
  max_val <- max(values, na.rm = TRUE)

  percent <- (100*values/max_val) %>% round()
  pad_width <- nchar(col) %>% max(na.rm = TRUE)
  if(is.na(pad_width)) pad_width <- 1

  # Replace NA with blanks
  col[is_na_value] <- ""
  bars <- bar_html(percent, col, pad_width)

  bars[is_na_value] <- NA
  bars
}

# Format column before mixing data values with HTML - the usual column
# definitions are applied afterwards - in the reactable function, which
# may not work if the values have been mixed with HTML.
apply_column_definitions <- function(df, field_config){

  names(df) %>% walk(
    function(col_name){
      config <- field_config[[col_name]]

      col_format <- config$format
      display_decimals <- config$display_decimals
      has_decimals <- !is.na(display_decimals)

      col <- df[[col_name]]
      na_values <- is.na(col)

      if(col_format == "percent"){
        col <- 100*col
      }

      if(has_decimals){
        col <- col %>% format(big.mark = ",", digits = display_decimals)
      }

      col[na_values] <- NA
      df[[col_name]] <<- col
    }
  )

  df
}


# Combine adding col names and bars to cells
add_html_to_cells <- function(df, settings, selected){

  bars <- settings$bar_option

  names(df) %>% walk(
    function(col_name){

      col <- df[[col_name]]
      na_values <- is.na(col)

      fn <- if(col_name %in% bars) add_bars else add_col_name
      col <- col %>% fn(col_name)

      col[na_values] <- NA
      df[[col_name]] <<- col
    }
  )

  df
}

# Icon with a link
icon_link <- function(name, link){
  a(href = link, target="_blank", rel="noopener noreferrer", icon(name))
}

# colGroup but return NULL if columns are NULL
colGroup_or_null <- function(name, columns){
  if(is.null(columns)) return(NULL)
  colGroup(name = name, columns = columns)
}

# Show satellite info box with comments and sources
show_info_box <- function(session, is_visible){
  session$sendCustomMessage("show_info_box", is_visible)
}

# Slider parameters for updating slider input
get_slider_params <- function(slider_field, data, config){

  slider_label <- config[[slider_field]]$display_name %>%
    paste("(range)")

  slider_step <- config[[slider_field]]$slider_step
  decimal_count <- nchar(slider_step %% 1) - 1

  # Get range
  slider_range <- data[[slider_field]] %>% range(na.rm = TRUE)
  range_class <- slider_range[1] %>% class()
  time_format <- if(range_class == "Date") "%Y" else NULL

  slider_range[2] <- slider_range[2] %>% ceiling_dec(digits = decimal_count)

  list(
    label = slider_label
    , step = slider_step
    , range = slider_range
    , time_format = time_format
    , range_class = range_class
  )
}

# TABLE FILTERS -----------------------------------------------------------------------------------

# Subset df by factor filter
apply_factor_filter <- function(df, filtered){

  factor_names <- filtered$factor$name %>% unique()

  # Split into groups with same filter name
  # Effect is to have an OR condition on different values with the same filter name
  filter_groups <- factor_names %>% map(
    function(x){
      filtered$factor[name == x]$value
    }
  ) %>% set_names(factor_names)

  filter_groups %>% iwalk(
    function(values, name){
      df <<- df[get(name) %in% values]
    }
  )
  df
}

# Subset df by measure filter from slider definition and value
apply_measure_filter <- function(df, filtered, ac){

  comparison <- "<="
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

# Converts string representation of filter to datatable
convert_factor_filter_to_df <- function(this_filter, init_field, init_df){

  is_not_filtered <- is.null(this_filter) || (length(this_filter) == 0)
  if(is_not_filtered) return(init_df)

  this_filter %>% map(
    function(x){
      x_split <- x %>% str_split("=", simplify = TRUE) %>% str_trim()
      col_name <- x_split[1]
      value <- x_split[2]

      this <- init_field[[col_name]]

      data.table(
        name = col_name
        , display = this$display_name
        , value = value
        , input_name = paste(col_name, "=", value)
        , input_display = paste(this$display_name, "=", value)
      )
    }
  ) %>%
    rbindlist()
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

