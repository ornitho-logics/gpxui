# UI for fetching, visualising and exporting GPS data
#     /home/mihai/github/mpio-be/gpxui/inst/Garmin65s
#' shiny::devmode(TRUE); shiny::runApp('./inst/UI/', launch.browser = TRUE)

# ! Packages, functions
require(gpxui)

cleandb <- function() {
  con <- db_con()
  DBI::dbExecute(con, "TRUNCATE GPS_POINTS")
  DBI::dbExecute(con, "TRUNCATE GPS_TRACKS")
  DBI::dbDisconnect(con)
  message("GPS_POINTS & GPS_TRACKS tables are empty now.")
}

#! Options
options(shiny.maxRequestSize = 10 * 1024^3)

GPS_IDS <- 1:20

#* Variables
cnf_path <- Sys.getenv("GPXUI_CNF")
# group <- Sys.getenv("GPXUI_GROUP")
group <- "nz_fieldworker"
if (!nzchar(group)) {
  group <- "gpxui"
}

# cleandb()
