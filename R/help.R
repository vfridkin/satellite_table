# Help

help_steps <- function(){

  # Override icon to make work in help steps
  icon_strong <- function(name){
    icon(
      style="font-family: \"Font Awesome 5 Free\";
        color: rgba(var(--colour-white-rgb), var(--alpha-high))"
      , name
    )
  }

  icon_circle_empty <- function(){
    HTML("
      <i style=\"font-family: 'Font Awesome 5 Free';
         color: rgba(var(--colour-white-rgb), var(--alpha-high))\"
         class=\"far fa-circle\">
      </i>
    ")
  }

  welcome <- div(
    p(
      "Things you can do"
      , tags$ul(
        tags$li("View aggregated statistics (summary)")
        , tags$li("View individual records (details)")
        , tags$li("Add and reorder columns")
        , tags$li("Filter by double clicking")
        , tags$li("Add cell bars")
        , tags$li("Animate measure range")
        , tags$li("Change the measure statistic")
        , tags$li("Save all the above in up to eight custom views")
      )
    )
    , p(
      "The satellite data used here is compiled by the Union of Concerned Scientists (UCS).
       Click the link below the heading to visit their site and learn more about the data.
      "
    )
    , p(
      "Navigate this guide with the buttons below.
      Exit by clicking the top right 'x' or the background.
      "
    )
  ) %>% as.character()


  table_view <- div(
    p(
      "The table has three types of columns"
      , tags$ul(
        tags$li(strong("Identifiers"), "are unique to each satellite")
        , tags$li(strong("Factors"), "are words common to multiple satellites")
        , tags$li(strong("Measures"), "are numbers and dates")
      )
      , "Columns of the same type are positioned together."
    )
    , p(
      "Click the above buttons to view the table as"
      , tags$ul(
        tags$li(
          strong("Summary")
          , "- factor columns are on the left and aggregated measures
              are on the right side of the table")
        , tags$li(
          strong("Details")
          , "- identifier columns on the left, followed by factors and
              then measures on the right side of the table.")
      )
    )
    , p(
      "The summary view does not contain identifiers.
       The details view does not contain the count column.
      "
    )
  ) %>% as.character()

  solar_system <- div(
    p(
      div(
        id = "switch_for_help_container"
        , style = "display: inline-block; width: 50px; margin-bottom: 0;"
        , materialSwitch(
          inputId = "switch_for_help"
          , label = NULL
          , value = TRUE
          , status = "danger"
        )
      )
      , div(
        style = "display: inline; width: 80%;"
        , "The 'sun' switch hides/shows the concealable controls."
      )
    )
    , p(
      icon_strong("circle")
      , icon_circle_empty()
      , "Eight circles (the 'planets') store eight sets of table
        views including summary/details selection, selected columns,
        filters, measure statistic and cell bar choices."
    )
    , p("
        Saving is automatic, click the little circles to move between configurations.
      "
    )
  ) %>% as.character()



  table_controls_select <- div(
    p(
      "These controls are visible when the 'sun' switch is on."
    )
    , p(
      icon_strong("columns")
      , strong("Factors")
      , "select factor columns to display. Minimum one selection."
    )
    , p(
      icon_strong("columns")
      , strong("Measures")
      , "select measure columns to display. Double click a measure
        to put it in the slider."
    )
  ) %>% as.character()

  table_controls_filter <- div(
    p(
      icon_strong("filter"), strong("Factors"), "shows current factor filters.  To create
      a filter, double click on a table cell in a factor column.  Recent filters are stored
      in the drop down. Different factors use an AND condition. Multiple filters on the same
      factor use an OR condition."
    )
    , p(
      icon_strong("filter")
      , strong("<measure>"), "filters the selected measure using the slider.
        Click ", icon_strong("play"), "to animate the slider."
    )
    , p(
      strong("Measure statistic"), "visible when a measure is selected in the
      summary view. Select a statistic from the drop down."
    )
    , p(
      strong("...more"), "reveals more controls including sort order, columns with
      cell bars and identifier columns"
    )
  ) %>% as.character()

  main_table <- div(
    p(
      "The table contains three types of columns in this order: identifier, factor and measure."
    )
    , p(
      "When filtered, a filter icon appears next to the top left corner of the table."
    )
    , p(
      "Double click a table cell to in a factor column to filter. (Use the slider to filter by
      a measure.)"
    )
  ) %>% as.character()

  satellite <- div(
    p("Click the satellite for info on saved views")
  ) %>% as.character()

  data.frame(
    title = c("Welcome"
              , "Table columns and views"
              , "Show controls and save changes"
              , "Select and reorder columns"
              , "Filter columns, select statistic"
              , "Table interactions"
              , "Satellite"
    )
    , intro = c(
      welcome
      , table_view
      , solar_system
      , table_controls_select
      , table_controls_filter
      , main_table
      , satellite
    )
    , element = c(
      ".header__text-box"
      , "#main-table_view"
      , "#main-solar_system"
      , ".table-controls-select"
      , ".table-controls-filter"
      , ".main__table-box"
      , ".body__image-box"
    )
  )
}
