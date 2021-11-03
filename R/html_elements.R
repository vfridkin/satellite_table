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

help_splash <- function(){
  div(
    class = "splash-box"
    , h3("Click the moon for help - anytime "
         , icon("angle-double-right")
         , style = "padding: 10px 0 10px 0; margin: 0;"
    )
    , div(
      style = "display: inline-block"
      , checkboxInput(
        inputId = "dont_show_splash_again_check"
        , label = "Don't show again"
        , value = FALSE
      )
    )
    , div(
      style = "display: inline-block"
      , class = "splash-button"
      , actionButton(
        inputId = "splash_ok_button"
        , label = "Got it"
      )
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



