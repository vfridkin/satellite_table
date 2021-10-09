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
  includeCSS("www/style.css")
  , header_element(
    title = "Satellites"
    , subtitle = glue("UCS Satellite Database vers {ac$source_version}")
    , sublink = ac$source_link
  )
  , image_element("satellite.png", "Satellite")
)

