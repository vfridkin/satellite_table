# Common functions

sar <- function(){
  rstudioapi::documentSaveAll()
  runApp()
}

load_config <- function(){
  read_yaml("config/config.yaml")
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
