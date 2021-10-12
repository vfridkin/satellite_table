# Global

#  .d8888b.  888          888               888     8888888b.
# d88P  Y88b 888          888               888     888   Y88b
# 888    888 888          888               888     888    888
# 888        888  .d88b.  88888b.   8888b.  888     888   d88P
# 888  88888 888 d88""88b 888 "88b     "88b 888     8888888P"
# 888    888 888 888  888 888  888 .d888888 888     888 T88b
# Y88b  d88P 888 Y88..88P 888 d88P 888  888 888 d8b 888  T88b
#  "Y8888P88 888  "Y88P"  88888P"  "Y888888 888 Y8P 888   T88b

# Load packages
library(shiny)
library(shinyWidgets)
library(readxl)
library(reactable)
library(data.table)
library(glue)
library(yaml)
library(janitor)
library(stringr)
library(purrr)
library(magrittr)
library(sparkline)

# Load files
list.files("R", full.names = TRUE) |> walk(source)

# Load app config
ac <- load_config()

# Ooptions
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))
