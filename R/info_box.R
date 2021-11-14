# Satellite info box

info_box_ui <- function(id, init){

  ns <- NS(id)

  # Set up choices for selectize inputs -----------------------------------------------------------
  all_choices <- get_choices()

  exclude_choices <- "date_of_launch"
  factor_choices <- c("year_of_launch")

  choices <- all_choices$measure_select %>% .[!. %in% exclude_choices]
  choices <- choices %>%
    c(all_choices$factor_select %>% .[. %in% factor_choices]) %>%
    sort()

  selected <- "year_of_launch"

  div(
    class = "info-box"
    , fluidRow(
      column(
        width = 6
        , div(
          style = "padding: 10px;"
          , gt_output(ns("table"))
        )
      )
      , column(
        width = 6
        , div(
          class = "measure-select"
          , selectizeInput(
            inputId = ns("measure_select")
            , label = NULL
            , choices = choices
            , selected = selected
            , multiple = FALSE
            , width = '100%'
          )
          , textOutput(ns("measure_text"))
        )
        , echarts4rOutput(ns("chart"))
      )
    )
    # , div(
    #   class = "info-box-controls"
    #   , actionButton(
    #     inputId = ns("close_button")
    #     , label = "Close"
    #   )
    # )
  )
}

info_box_server <- function(id, identifier_select, data, field_df){
  moduleServer(
    id
    , function(input, output, session){

      # Constants ---------------------------------------------------------------------------------

      k <- list(
        text_high = "rgba(255, 255, 255, 0.9)"
        , text_med = "rgba(255, 255, 255, 0.7)"
        , text_low = "rgba(255, 255, 255, 0.5)"
      )

      # Reactives ---------------------------------------------------------------------------------

      m <- reactiveValues(
        row = NULL
        , measure_select = NULL
      )

      # Get data row from identifier selected
      observeEvent(
        identifier_select()
        , {
          select <- identifier_select() %>% req()
          m$row <- data[get(select$col_name) == select$value]
          m$measure_select <- input$measure_select
        }
      )

      observeEvent(
        input$measure_select
        , {
          m$measure_select <- input$measure_select
        }
      )

      # Hold satellite data in satellite reactive
      satellite <- reactive({

        this_row <- m$row
        shiny::validate(need(this_row, "Loading..."))

        # Get row count to identify non-unique identifiers
        row_count <- nrow(this_row)

        # Use the first row - in case there are more than one
        this_row <- this_row[1]

        # Data for chart
        measure_col <- m$measure_select
        marker_value <- this_row[[measure_col]]
        measure_values <- data[[measure_col]] %>% as.numeric()
        marker_title <- this_row$current_official_name_of_satellite
        series_colour <- field_df[name == measure_col]$bar_colour_left

        list(
          row = this_row
          , chart = list(
            data = data.table(value = measure_values)
            , marker_value = marker_value
            , measure_col = measure_col
            , marker_title = marker_title
            , series_colour = series_colour
          )
        )

      })

      # Table -------------------------------------------------------------------------------------

      output$table <- render_gt({

        satellite <- satellite()
        this <- satellite$row

        # table header
        name <- this$current_official_name_of_satellite
        users <- this$users
        launch_date <- this$date_of_launch %>% format("%d %b %Y")
        launch_site <- this$launch_site
        country <- this$country_of_operator_owner
        purpose <- this$purpose
        detailed_purpose <- this$detailed_purpose

        if(detailed_purpose != "unknown"){
          purpose <- "{purpose} > {detailed_purpose}" %>% glue()
        }

        # Identifiers
        id_cols <- field_df[group_as == "identifier", .(name, display_name)]
        df_id <- id_cols %>% pmap(
          function(name, display_name){
            data.table(
              this_name = display_name
              , this_value = this[[name]]
            )
          }
        ) %>% rbindlist()

        # Add source of orbital data
        df_id <- list(
          df_id
          , data.table(
            this_name = "Source Used for Orbital Data"
            , this_value = this$source_used_for_orbital_data
          )
        ) %>% rbindlist()

        # Comments and remaining sources
        comments_sources <- c("comments", "source_\\d") %>%
          map(
            function(type){
              cols <- names(this) %>% purrr::keep(~str_detect(.x, type))
              this[, ..cols] %>%
                unlist() %>%
                .[!is.na(.)]
            }
          ) %>% set_names(c("comments", "sources"))

        comments <- comments_sources$comments %>% paste(collapse = "\n")

        source_no <- length(comments_sources$sources)

        sources <- if(source_no > 0){
          comments_sources$sources %>%
            # paste0('[Link ', (1:source_no),'](', ., '){:target="_blank"}') %>%
            paste0('<a href="', ., '" target="_blank" rel="noopener noreferrer">link ',(1:source_no) ,'</a>') %>%
            paste(collapse = ", ")
        } else ""

        gt_tbl <- gt(df_id) %>%
          tab_style(
            style = list(
              cell_text(color = k$text_low)
            )
            , locations = cells_body(
              columns = this_name
            )
          ) %>%
          tab_style(
            style = list(
              cell_text(color = k$text_med)
            )
            , locations = cells_body(
              columns = this_value
            )
          )


        title_style <- list(
          cell_fill(color = "#212121")
          , cell_text(color = k$text_high)
        )

        gt_tbl <- gt_tbl %>%
          tab_header(
            title = name
            , subtitle = "{country} - {users} - {launch_site}: {launch_date}" %>% glue()
          ) %>%
          tab_style(
            style = title_style
            , locations = cells_title(groups = c("title"))
          ) %>%
          tab_style(
            style = title_style
            , locations = cells_title(groups = c("subtitle"))
          ) %>%
          tab_source_note(
            source_note = "Purpose: {purpose}" %>% glue()
          ) %>%
          tab_source_note(
            source_note = "Comments: {comments}" %>% glue()
          ) %>%
          tab_source_note(
            source_note = "Sources: {sources}" %>% glue() %>% md()
          ) %>%
          tab_style(
            style = cell_borders(
              sides = c("top", "bottom"),
              color = k$text_low,
              weight = px(1.5),
              style = "solid"
            ),
            locations = cells_body(
              columns = everything(),
              rows = everything()
            )
          ) %>%
          tab_options(
            column_labels.hidden = TRUE
            , column_labels.font.size = "14px"
            , table.font.size = "14px"
          )


        gt_tbl

      })

      outputOptions(output, "table", suspendWhenHidden = FALSE)


      # Measure text ------------------------------------------------------------------------------

      output$measure_text <- renderText({

        this <- satellite()$chart

        value <- this$marker_value
        if(is.na(value)) value <- "Unknown"

        "{this$marker_title}: {value}" %>% glue()

      })

      # Chart -------------------------------------------------------------------------------------

      output$chart <- renderEcharts4r({

        this <- satellite()$chart

        e <- this$data %>%
          e_chart() %>%
          e_histogram(
            value
            , name = NULL
            , legend = FALSE
            , bar_width = "90%"
            , x_index = 0
            , y_index = 0
            , color = this$series_colour
          )  %>%
          e_hide_grid_lines() %>%
          e_color(background = "transparent")

        # Mark chart if satellite has a value
        has_value <- !is.na(this$marker_value)

        if(has_value){
          e <- e %>% e_mark_p(
            type = "line"
            , serie_index = 1
            , data = list(xAxis = this$marker_value, label = list(formatter = this$marker_title))
            , lineStyle = list(type = "dashed", color = "white")
            , symbol = "none"
            , label = list(color = k$text_high)
            , animation = FALSE
          )
        }

        e

      })

      # Close button ------------------------------------------------------------------------------

      observeEvent(
        input$close_button
        , {
          show_info_box(session, FALSE)
        }
      )



    }
  )
}

