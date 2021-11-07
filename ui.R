# User interface main file

# 888     888 8888888     8888888b.
# 888     888   888       888   Y88b
# 888     888   888       888    888
# 888     888   888       888   d88P
# 888     888   888       8888888P"
# 888     888   888       888 T88b
# Y88b. .d88P   888   d8b 888  T88b
#  "Y88888P"  8888888 Y8P 888   T88b

###################################################################################################
#
# -= Welcome to the Satellite Table =-
#
# There are three main modules in the app, all in the R directory
# 1 satellite_table*
# 2 more_settings
# 3 saved_info*
#
# *There are two tables in the app, both using reactable
#
# Summary of design
# ~~~~~~~~~~~~~~~~~
# Configuration is controlled from YAML and CSV files.
# Module more_settings is called from satellite_table.
# Local storage is used to save user changes.
# For speed, HTML is created directly with data.table
# instead of using colDef functions or the recent reactablefmtr package
# Examples include
# - cell bars
# - cell subheadings
# - custom tooltips for column headers
#
# At this time, getReactableState does not provide sort status of the table, so
# custom functions were created to get and save sort state.
#
# Selectize inputs are used extensively to control the columns in the table, including
# - column visibility
# - column order
# - filtering
#
# There are commands embedded in the selectize choices to add and remove in bulk.
# The table can be filtered by double clicking on a factor cell - the filters
# are managed by a selectize input.
# Filtering a measure is done with the slider.  The filtering measure in the slider
# is selected by double clicking on a selectize measure item.
#
# It's probably easier to go through the inbuilt help system to understand factors and
# measures and a lot of the above :)
#
# Thanks for reading and hope you enjoy the app, Vlad Fridkin 2021
#
###################################################################################################

fluidPage(

  # CSS and JS files - for animation, styling and custom interactions
  load_styles()
  , includeScript("www/script.js")

  # Tour dependency
  , introjsUI()

  # Start page render
  , header_element(
    title = "Satellites"
    , subtitle = glue("UCS Satellite Database vers {ac$source_version}")
    , sublink = ac$source_link
  )
  , image_element("satellite2.png", "Satellite")
  , button_element("Launch")

  # These appear after the launch button is clicked
  , help_element("moon_question.png", "Help button")
  , satellite_table_ui("main", ac$field_df)

  # For new users, indicate the moon and satellite are clickable
  , screen_overlay()
  , bubble_moon()
  , bubble_satellite()

  # Clicking the satellite opens up a summary of saved views
  , saved_info_ui("saved")
)
