# Satellite info box

info_box_ui <- function(id){

  ns <- NS(id)

  div(
    class = "info-box"
    , fluidRow(
      column(
        width = 12
        , div(
          style = "padding: 10px;"
          , gt_output(ns("table"))
          , div(
            class = "info-box-controls"
            , actionButton(
              inputId = ns("close_button")
              , label = "Close"
            )
          )
        )
      )
    )
  )
}

info_box_server <- function(id, identifier_select, data, field_df){
  moduleServer(
    id
    , function(input, output, session){

      m <- reactiveValues(
        row = NULL
      )

      observeEvent(
        identifier_select()
        , {
          select <- identifier_select() %>% req()
          m$row <- data[get(select$col_name) == select$value]
        }
      )


      satellite <- reactive({

        this_row <- m$row
        shiny::validate(need(this_row, "Loading..."))

        # Get row count to identify non-unique identifiers
        row_count <- nrow(this_row)

        # Use the first row - in case there are more than one
        this_row <- this_row[1]


        list(
          row = this_row
        )

      })

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

        text_high <- "rgba(255, 255, 255, 0.9)"
        text_med <- "rgba(255, 255, 255, 0.7)"
        text_low <- "rgba(255, 255, 255, 0.5)"

        gt_tbl <- gt(df_id) %>%
          tab_style(
            style = list(
              cell_text(color = text_low)
            )
            , locations = cells_body(
              columns = this_name
            )
          ) %>%
          tab_style(
            style = list(
              cell_text(color = text_med)
            )
            , locations = cells_body(
              columns = this_value
            )
          )


        title_style <- list(
          cell_fill(color = "#212121")
          , cell_text(color = text_high)
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
              color = text_low,
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
            # , column_labels.font.size = "smaller"
            # , table.font.size = "smaller"
          )


        gt_tbl

      })

      outputOptions(output, "table", suspendWhenHidden = FALSE)

      observeEvent(
        input$close_button
        , {
          show_info_box(session, FALSE)
        }
      )



    }
  )
}

