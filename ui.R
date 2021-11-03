# User interface main file

# 888     888 8888888     8888888b.
# 888     888   888       888   Y88b
# 888     888   888       888    888
# 888     888   888       888   d88P
# 888     888   888       8888888P"
# 888     888   888       888 T88b
# Y88b. .d88P   888   d8b 888  T88b
#  "Y88888P"  8888888 Y8P 888   T88b

fluidPage(
  load_styles()
  , includeScript("www/script.js")
  , introjsUI()
  , header_element(
    title = "Satellites"
    , subtitle = glue("UCS Satellite Database vers {ac$source_version}")
    , sublink = ac$source_link
  )
  , image_element("satellite2.png", "Satellite")
  , button_element("Launch")
  , help_element("moon_question.png", "Help button")
  , satellite_table_ui("main", ac$field_df)
  , screen_overlay()
  , help_splash()
  , saved_info_ui("saved")
)
