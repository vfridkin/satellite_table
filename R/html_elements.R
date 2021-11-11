# UI components

header_element <- function(title, subtitle, sublink){
  div(
    class = "header"
    , div(
      class = "header__text-box"
      , h1(class = "heading-primary"
           , span(
             class="heading-primary--main"
             , title
           )
           , a(
             class="heading-primary--sub"
             , href= sublink
             , target="_blank"
             , rel="noopener noreferrer"
             , glue(subtitle)
           )
      )
    )
  )
}

image_element <- function(file, alt){
  div(
    class = "body__image-box"
    , span(
      class = "filled_circle2 ping"
    )
    , style = "width: 50px; height: 50px; top: 6vh; left: 1vw; background: transparent;"
    , img(
      class = "body__image"
      , src = file
      , alt = alt
    )
  )
}

button_element <- function(label){
  div(
    class = "body__btn-box"
    , a(
      href="#"
      , id = "launch_button"
      , class = "launch-btn btn--animated"
      , label
    )
  )
}

help_element <- function(file, alt){
  div(
    class = "body__help-box"
    , span(
      class = "filled_circle ping"
    )
    , img(
      class = "body__help-image"
      , src = file
      , alt = alt
    )
  )
}

screen_overlay <- function(){
  div(
    class = "screen-overlay"
    , style = "display: none;"
  )
}

bubble_moon <- function(){
  div(
    class = "bubble-moon"
    , h3("Click the moon for a tour"
         , icon("angle-double-right")
         , style = "padding: 10px 0 10px 0; margin: 0; background: transparent;"
    )
    , div(
      style = "display: inline-block; padding: 0 0 15px 0;"
      , class = "moon-button"
      , actionButton(
        inputId = "moon_button"
        , label = "Got it"
      )
    )
    , div(
      style = "display: inline-block; width: 230px; padding: 0px 10px 15px 10px;"
    )
  )
}

bubble_satellite <- function(){
  div(
    class = "bubble-satellite"
    , h3(icon("angle-double-left")
         , "Click the satellite for saved views"
         , style = "padding: 10px 0 10px 0; margin: 0; background: transparent;"
    )
    , div(
      style = "display: inline-block; padding: 0px 10px 15px 15px;"
      , class = "satellite-button"
      , actionButton(
        inputId = "satellite_button"
        , label = "Got it"
      )
    )
    , div(
      style = "display: inline-block; padding: 0px 10px 15px 10px;"
      , checkboxInput(
        inputId = "dont_show_splash_again_check"
        , label = "Don't show us again"
        , value = FALSE
      )
    )
  )
}

space_man <- function(file, alt){
  div(
    class = "space-man"
    , img(
      class = "space-man-image"
      , src = file
      , alt = alt
    )
  )
}

footer_element <- function(){
  div(
    class = "footer"
    , div(
      class = "footer__text-box"
      , div(
        HTML("RStudio Table Contest 2021 &copy; Vlad Fridkin")
        , icon_link("linkedin", "https://www.linkedin.com/in/vfridkin/")
        , icon_link("github", "https://github.com/vfridkin")
        , icon_link("youtube", "https://www.youtube.com/channel/UCl_LeQWtHsopscfKBg0wE1A")
      )
    )
  )
}



