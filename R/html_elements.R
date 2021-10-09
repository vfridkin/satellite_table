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


