# ==========================================================================
# UI for fetching, visualising and exporting GPS data
#     /home/mihai/github/mpio-be/gpxui/inst/Garmin65s
#' shiny::runApp('./inst/UI/', launch.browser =  TRUE)
# ==========================================================================

#! Packages, functions
sapply(
  c(
    "gpxui",
    "leaflet",
    "bs4Dash",
    "sf"
  ),
  require,
  character.only = TRUE,
  quietly = TRUE
)

cleandb <- function(db = DB) {
  if (db == "tests") {
    con <- db_con(db = db)
    DBI::dbExecute(con, "TRUNCATE GPS_POINTS")
    DBI::dbExecute(con, "TRUNCATE GPS_TRACKS")
    DBI::dbDisconnect(con)
    message("GPS_POINTS & GPS_TRACKS tables are empty now.")
  }
}

#! Options
options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 10 * 1024^3)

#* Variables
cnf_path <- Sys.getenv("GPXUI_CNF")
group <- "localhost"
DB <- "tests"


cleandb()
