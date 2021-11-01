# Help

help_steps <- function(){

  table_view <- div(
    p(
      "The table can be viewed in two ways, (1) the summary view
      groups by factors and aggregates measures, (2) the details view
      is the original table."
    )
    , p(
      "Factors are words common to multiple records (i.e. can be grouped) and
      measures are numbers and dates (ie. can be aggregated).  The details view
      also has identifiers, these are unique to each record."
    )
    , p(
      "In the table, identifiers, factors and measures appear in this order from
      left to right."
    )
  ) %>% as.character()

  solar_system <- div(
    p(
      "Click the switch on he left (the 'Sun') to show/hide the concealable controls.
      The 8 little circles (the 'Planets') store 8 independent table
      configurations including column selections, filters, selected measure statistic
      and which columns have bars.  Saving is automatic, click the little circles to
      move between configurations.
      "
    )
  ) %>% as.character()


  table_controls <- div("Test") %>% as.character()
  main_table <- div("Test") %>% as.character()
  satellite <- div("Test") %>% as.character()


  data.frame(
    title = c("Table view", "Show controls/save changes", "Concealable controls", "Main table", "Satellite")
    , intro = c(table_view, solar_system, table_controls, main_table, satellite)
    , element = c("#main-table_view", "#main-solar_system", ".table_controls", ".main__table-box", ".body__image-box")
  )
}
