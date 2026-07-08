#' Directory Input
#'
#' Create a browser directory upload control that returns the selected files as
#' a Shiny file-input data frame.
#'
#' @param inputId Input id.
#' @param label Optional label shown above the control.
#' @param placeholder Placeholder text for the read-only text field.
#'
#' @return A Shiny tag object.
#' @export
#' @examples
#' if (interactive()) {
#'   options(shiny.maxRequestSize = 10 * 1024^3)
#'
#'   ui <- shiny::fluidPage(
#'     shiny::sidebarLayout(
#'       shiny::sidebarPanel(
#'         dirInput("fileIn")
#'       ),
#'       shiny::mainPanel(
#'         shiny::tableOutput("contents")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$contents <- shiny::renderTable({
#'       if (is.null(input$fileIn)) {
#'         return(NULL)
#'       }
#'       data.table::data.table(input$fileIn)[, file.size(datapath), by = name]
#'     })
#'   }
#'
#'   shiny::shinyApp(ui, server)
#' }

dirInput <- function(inputId, label = NULL, placeholder = "No dir selected.") {
  div(
    class = "form-group shiny-input-container",
    if (!is.null(label)) {
      tags$label(class = "control-label", `for` = inputId, label)
    },
    div(
      class = "input-group",
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(
          class = paste("btn btn-default", "btn-secondary"),
          a(icon("folder-open"), "Browse"),
          tags$input(
            id = inputId,
            name = inputId,
            type = "file",
            webkitdirectory = TRUE,
            onchange = "pressed()",
            style = "display: none;"
          )
        )
      ),
      tags$input(
        type = "text",
        class = "form-control",
        placeholder = placeholder,
        readonly = "readonly"
      )
    ),
    tags$div(
      id = paste(inputId, "_progress", sep = ""),
      class = "progress active shiny-file-input-progress",
      tags$div(class = "progress-bar bg-success")
    )
  )
}


#' gpx_file_upload_check
#' @param  x a data.frame returned by UI after dir upload. See [gpxui::dirInput()]
#'
#' @return A Shiny tag object describing the selected directory.
#' @export
gpx_file_upload_check <- function(x) {
  did <- deviceID(x)

  d <- data.table(x)

  if (is.na(did)) {
    o1 <- "<span class='badge rounded-pill bg-warning'> GPS ID not found! </span>" |>
      HTML() |>
      h3()
  } else {
    o1 <- glue(
      "<span class='badge rounded-pill bg-success'>GPS {did}</span> detected."
    ) |>
      HTML() |>
      h3()
  }

  ngpx <- nrow(d[str_detect(name, "\\.gpx$")])

  if (ngpx == 0) {
    o2 <- glue(
      "Files uploaded OK but the selected folder contains no {tags$code('gpx')} files. Did you select the correct folder?"
    ) |>
      HTML()
  } else {
    o2 <- glue("Selected directory has {ngpx} {code('gpx')} file(s).") |>
      HTML() |>
      tags$b()
  }

  div(
    p(o1),
    p(o2),
    hr()
  )
}


#' Summarise Tracks
#'
#' @param x An `sf` track object returned by [read_GPX_table()] with
#'   `sf = TRUE`.
#'
#' @return A `data.table` with display-ready summary rows.
#' @export
track_summary <- function(x) {
  if (nrow(x) == 0) {
    o <- data.table(Info = "No GPX track files found")
  }

  if (nrow(x) > 0) {
    o <- sf::st_drop_geometry(x) |> setDT()
    o[, dist := sf::st_length(x) |> units::set_units("km")]
    o[, deltat := difftime(max_dt, min_dt, units = "hours")]

    o <- o[, .(
      `N track points` = nrow(o) |> as.character(),
      `mean elevation` = weighted.mean(mean_ele, w = n) |>
        round(2) |>
        as.character(),
      `max elevation` = max(max_ele) |> round(2) |> as.character(),
      `min elevation` = min(min_ele) |> round(2) |> as.character(),
      `start hour` = min(min_dt) |> format("%H:%M"),
      `stop hour` = max(max_dt) |> format("%H:%M"),
      `avg speed (km/hour)` = weighted.mean(
        as.numeric(dist) / as.numeric(deltat),
        w = n
      ) |>
        round(2) |>
        as.character()
    )]

    o[, i := 1]
    o <- melt(o, id.vars = "i")[, i := NULL]

    if (nrow(x) == 0) {
      o <- o[1, ]
    }
  }

  o
}

#' Summarise Waypoints
#'
#' @param x An `sf` waypoint object returned by [read_GPX_table()] with
#'   `sf = TRUE`.
#'
#' @return A `data.table` with display-ready summary rows.
#' @export
points_summary <- function(x) {
  if (nrow(x) == 0) {
    o <- data.table(Info = "No GPX waypoints files found")
  }

  if (nrow(x) > 0) {
    h <-
      x |>
      sf::st_union() |>
      sf::st_convex_hull() |>
      sf::st_area() |>
      units::set_units("km^2")

    o <- data.table(
      `N waypoints` = nrow(x) |> as.character(),
      `first point` = x$gps_point[1] |> as.character(),
      `last point` = x$gps_point[nrow(x)] |> as.character(),
      `Area covered (sqkm)` = round(h, 2) |> as.character()
    )

    o[, i := 1]
    o <- melt(o, id.vars = "i")[, i := NULL]

    if (nrow(x) == 0) {
      o <- o[1, ]
    }
  }

  o
}

#' Build a GPX Summary Panel
#'
#' @param pts An `sf` waypoint object.
#' @param trk An `sf` track object.
#'
#' @return A Shiny tag object containing summary tables.
#' @export
gpx_summary <- function(pts, trk) {
  trk_smr <- track_summary(trk)
  pst_smr <- points_summary(pts)

  trk_tab <-
    knitr::kable(
      trk_smr,
      caption = "Track",
      digits = 2,
      table.attr = "class = 'table table-striped table-sm'",
      format = "html"
    ) |>
    HTML()

  trk_pts <-
    knitr::kable(
      pst_smr,
      caption = "Points",
      digits = 2,
      table.attr = "class = 'table table-striped table-sm'",
      format = "html"
    ) |>
    HTML()

  div(
    trk_tab,
    trk_pts
  )
}


#' Sidebar Section Title
#'
#' @param txt Title text.
#' @param icon Font Awesome icon name passed to [shiny::icon()].
#' @param class CSS class applied to the title.
#'
#' @return A Shiny tag object.
#' @export
ctrl_title <- function(txt, icon = "circle", class = "text-danger") {
  span(icon(icon), txt, class = class) |> strong()
}
