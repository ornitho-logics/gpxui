#' GPX Shiny Server
#'
#' @param .cnf Path to a MariaDB option file passed to [db_con()].
#' @param group Option group in `.cnf` passed to [db_con()].
#'
#' @return A Shiny server function.
#' @export
gpx_server <- function(.cnf = NULL, group = NULL) {
  force(.cnf)
  force(group)

  function(input, output, session) {
    session$allowReconnect(TRUE)

    #+ Read gpx, update db, update UI

    run_update <- reactive({
      req(input$upload_GPX)

      # read gpx
      pts <- read_all_waypoints(input$upload_GPX)
      trk <- read_all_tracks(input$upload_GPX)

      # update new data to db
      updated_pts <- gpx_to_database(
        x = pts,
        tab = "GPS_POINTS",
        .cnf = .cnf,
        group = group
      )
      updated_trk <- gpx_to_database(
        x = trk,
        tab = "GPS_TRACKS",
        .cnf = .cnf,
        group = group
      )

      rbindlist(list(updated_pts, updated_trk))
    })

    observeEvent(input$upload_GPX, {
      x <- run_update()

      updateTextInput(
        session,
        "last_pts_dt",
        value = x[tab == "GPS_POINTS", as.character(last_entry_before_update)]
      )
      updateTextInput(
        session,
        "last_trk_dt",
        value = x[tab == "GPS_TRACKS", as.character(last_entry_before_update)]
      )

      selected_gps <- na.omit(x$gps_id)[1]
      if (!is.na(selected_gps)) {
        updateSelectInput(session, "gps_id", selected = as.character(selected_gps))
      }
    })

    #* MAP: base map
    output$MAP <- renderLeaflet({
      basemap()
    })

    #* MAP: update map
    # on upload
    observeEvent(input$upload_GPX, {
      pts <- read_GPX_table(
        tab = "GPS_POINTS",
        dt = input$last_pts_dt,
        .cnf = .cnf,
        group = group,
        sf = TRUE
      )
      trk <- read_GPX_table(
        tab = "GPS_TRACKS",
        dt = input$last_trk_dt,
        .cnf = .cnf,
        group = group,
        sf = TRUE
      )
      bbox <- st_bbox_all(list(pts, trk))

      leafletProxy("MAP") |>
        gpxmap(bbox, pts, trk, zoom = TRUE)
    })

    # on explore
    observeEvent(input$go_explore, {
      pts <- read_GPX_table(
        tab = "GPS_POINTS",
        dt = input$show_after,
        gps_id = input$gps_id,
        .cnf = .cnf,
        group = group,
        sf = TRUE
      )
      trk <- read_GPX_table(
        tab = "GPS_TRACKS",
        dt = input$show_after,
        gps_id = input$gps_id,
        .cnf = .cnf,
        group = group,
        sf = TRUE
      )
      bbox <- st_bbox_all(list(pts, trk))

      leafletProxy("MAP") |>
        gpxmap(bbox, pts, trk, zoom = TRUE)

      if (!.has_leaflet_bbox(bbox)) {
        toast(
          title = "Warning",
          body = h4("Nothing to show!"),
          options = list(
            position = "topLeft",
            autohide = TRUE,
            close = FALSE,
            delay = 2000
          )
        )
      }
    })

    #* Feedback

    get_feedback <- reactive({
      e1 <- includeMarkdown(system.file(package = "gpxui", "www", "help.md"))
      if (!is.null(input$upload_GPX)) {
        e1 <- gpx_file_upload_check(input$upload_GPX)
      }

      e2 <- ""

      if (nchar(input$last_trk_dt) > 0 || nchar(input$last_pts_dt) > 0) {
        e2 <- gpx_summary(
          read_GPX_table(
            tab = "GPS_POINTS",
            dt = input$last_pts_dt,
            gps_id = input$gps_id,
            .cnf = .cnf,
            group = group,
            sf = TRUE
          ),
          read_GPX_table(
            tab = "GPS_TRACKS",
            dt = input$last_trk_dt,
            gps_id = input$gps_id,
            .cnf = .cnf,
            group = group,
            sf = TRUE
          )
        )
      }

      div(e1, e2)
    })

    output$feedback <- renderUI({
      get_feedback()
    })

    #* EXPORT

    output$download_points <- downloadHandler(
      filename = function() {
        glue("{input$export_object}.{input$export_class}")
      },
      content = function(file) {
        gpx_export(
          tab = input$export_object,
          file = file,
          .cnf = .cnf,
          group = group
        )
      }
    )
  }
}
