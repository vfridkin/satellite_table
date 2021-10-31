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
    , img(
      class = "body__help-image"
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



