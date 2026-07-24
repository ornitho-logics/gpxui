.gpxui_dependency <- function() {
  htmltools::htmlDependency(
    name = "gpxui-styles",
    version = utils::packageVersion("gpxui"),
    src = "www",
    stylesheet = "style.css",
    package = "gpxui",
    all_files = FALSE
  )
}

#' GPX Shiny UI
#'
#' @param gps_ids GPS ids shown in the explorer filter.
#' @param export_tables Database tables/views available for export.
#'
#' @return A Shiny UI object.
#' @export
gpx_ui <- function(
  gps_ids = 1:10,
  export_tables = c("mid_points", "mid_tracks")
) {
  page <- bs4Dash::dashboardPage(
    help = NULL,
    dark = TRUE,
    title = "GPS manager",
    header = dashboardHeader(
      title = "GPS manager",
      border = FALSE,
      controlbarIcon = icon("circle-info"),
      sidebarIcon = icon("bars"),
      rightUi = tagList()
    ),
    sidebar = dashboardSidebar(
      collapsed = FALSE,
      sidebarMenu(
        tags$li(
          class = "gpx-sidebar-section",
          ctrl_title("1. Upload", "cloud-upload"),

          dirInput(
            "upload_GPX",
            label = NULL,
            placeholder = "./GARMIN/Garmin/GPX"
          )
        ),

        tags$li(
          class = "gpx-sidebar-section",
          ctrl_title("2. Export", "cloud-download"),

          selectInput(
            inputId = "export_object",
            label = NULL,
            choices = export_tables,
            multiple = FALSE,
            width = "100%"
          ),
          selectInput(
            inputId = "export_class",
            label = "Export as:",
            choices = c("gpx", "csv"),
            multiple = FALSE,
            width = "100%"
          ),
          downloadButton(
            outputId = "download_points",
            label = "Download",
            class = "gpx-sidebar-button"
          )
        ),

        tags$li(
          class = "gpx-sidebar-section",
          ctrl_title("3. Explore", "map"),
          selectInput(
            inputId = "gps_id",
            label = "Select GPS ID:",
            choices = gps_ids,
            multiple = TRUE,
            width = "100%"
          ),
          dateInput(
            inputId = "show_after",
            label = "Pick a start date:",
            width = "100%"
          ),
          actionButton(
            "go_explore",
            "Show on map",
            class = "gpx-sidebar-button gpx-explore-button"
          )
        ),

        # Hidden inputs
        tags$li(
          class = "invisible",
          style = "display: none;",
          textInput("last_pts_dt", "Last points"),
          textInput("last_trk_dt", "Last tracks")
        )
      )
    ),
    body = dashboardBody(
      div(
        class = "gpx-map-shell",
        leafletOutput(
          outputId = "MAP",
          width = "100%",
          height = "100%"
        )
      )
    ),

    controlbar = dashboardControlbar(
      collapsed = TRUE,
      pinned = FALSE,
      overlay = TRUE,

      shinycssloaders::withSpinner(
        id = "spinner",

        uiOutput("feedback"),

        type = 1,
        color = "#e7debd",
        size = 1
      )
    )
  )

  page_query <- htmltools::tagQuery(page)
  page_query <- page_query$
    find("meta")$
    filter(function(tag, ...) {
      identical(tag$attribs$name, "viewport")
    })$
    removeAttrs("content")$
    addAttrs(
      content = "width=device-width, initial-scale=1, viewport-fit=cover"
    )
  page <- page_query$allTags()

  htmltools::attachDependencies(page, .gpxui_dependency())
}
