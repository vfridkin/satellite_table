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

help_splash <- function(file, alt){
  div(
    class = "help__splash-background"
    , style = "display: none;"
    , div(
      class = "help__splash-box"
      , style = "display: none;"
      , align = "center"
      , div(
        class="help__splash-title"
        , "WELCOME"
      )
      , img(
        class = "help__splash-image"
        , src = file
        , alt = alt
      )
      , h3("Click the moon for help anytime "
           , icon(class = "fa-flip-horizontal", "mouse-pointer")
           , style = "padding-bottom: 10px;"
           )
      , prettyCheckbox(
        inputId = "dont_show_again_check"
        , "Don't show next time"
        , value = FALSE
        , icon = icon("check")
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



