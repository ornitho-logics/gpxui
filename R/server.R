#' GPX Shiny Server
#'
#' @param server Deprecated alias for the MariaDB option-file `group`.
#' @param db Database/schema selected after connecting.
#'
#' @return A Shiny server function.
#' @export
gpx_server <- function(server = "localhost", db = "tests") {
  force(server)
  force(db)

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
        server = server,
        db = db,
        pts,
        tab = "GPS_POINTS"
      )
      updated_trk <- gpx_to_database(
        server = server,
        db = db,
        trk,
        tab = "GPS_TRACKS"
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
        server,
        db,
        "GPS_POINTS",
        input$last_pts_dt,
        sf = TRUE
      )
      trk <- read_GPX_table(
        server,
        db,
        "GPS_TRACKS",
        input$last_trk_dt,
        sf = TRUE
      )
      bbox <- st_bbox_all(list(pts, trk))

      leafletProxy("MAP") |>
        gpxmap(bbox, pts, trk, zoom = TRUE)
    })

    # on explore
    observeEvent(input$go_explore, {
      pts <- read_GPX_table(
        server,
        db,
        "GPS_POINTS",
        input$show_after,
        input$gps_id,
        sf = TRUE
      )
      trk <- read_GPX_table(
        server,
        db,
        "GPS_TRACKS",
        input$show_after,
        input$gps_id,
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
            server,
            db,
            "GPS_POINTS",
            input$last_pts_dt,
            input$gps_id,
            sf = TRUE
          ),
          read_GPX_table(
            server,
            db,
            "GPS_TRACKS",
            input$last_trk_dt,
            input$gps_id,
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
        gpx_export(server = server, db = db, input$export_object, file)
      }
    )
  }
}
