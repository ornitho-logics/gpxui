gpxfile <- system.file(
  package = "gpxui",
  "Garmin65s",
  "GPX",
  "Waypoints_20-APR-23.gpx"
)

gpxtrackfile <- system.file(
  package = "gpxui",
  "Garmin65s",
  "GPX",
  "Current",
  "Current.gpx"
)

gpxfile_empty <- system.file(
  package = "gpxui",
  "Garmin65s",
  "GPX",
  "Waypoints_empty.gpx"
)

gpxdir <- system.file(package = "gpxui", "Garmin65s", "GPX")
dirout <- as_dirInput_output(gpxdir)
dirout_valid <- dirout[dirout$name != "Waypoints_empty.gpx", ]


gpxdir_noid <- system.file(package = "gpxui", "Garmin65s", "GPX", "Current")
dirout_noid <- as_dirInput_output(gpxdir_noid)

cnf_path <- Sys.getenv("GPXUI_CNF")
if (!nzchar(cnf_path)) {
  cnf_path <- "~/.config/dbo/.my.cnf"
}
group <- Sys.getenv("GPXUI_GROUP")
if (!nzchar(group)) {
  group <- "localhost"
}
assign("cnf_path", cnf_path, envir = .GlobalEnv)
assign("group", group, envir = .GlobalEnv)


cleandb <- function() {
  con <- db_con(db = "tests")
  DBI::dbExecute(con, "TRUNCATE GPS_POINTS")
  DBI::dbExecute(con, "TRUNCATE GPS_TRACKS")
  DBI::dbDisconnect(con)
}
