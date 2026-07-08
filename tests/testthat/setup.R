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

.test_cnf_with_database <- function(cnf_path, group, database) {
  cnf_path <- path.expand(cnf_path)
  lines <- readLines(cnf_path, warn = FALSE)
  headers <- grep("^\\s*\\[[^]]+\\]", lines)
  groups <- sub("^\\s*\\[([^]]+)\\].*$", "\\1", lines[headers])
  group_start <- headers[match(group, groups)]

  if (is.na(group_start)) {
    stop("Option group `", group, "` not found in ", cnf_path, call. = FALSE)
  }

  next_header <- headers[headers > group_start][1]
  group_end <- if (is.na(next_header)) {
    length(lines)
  } else {
    next_header - 1L
  }

  group_lines <- lines[seq.int(group_start + 1L, group_end)]
  group_lines <- group_lines[
    !grepl("^\\s*database\\s*=", group_lines, ignore.case = TRUE)
  ]

  test_group <- paste0(group, "_tests")
  test_cnf <- tempfile(fileext = ".cnf")
  file.create(test_cnf)
  Sys.chmod(test_cnf, mode = "0600")
  writeLines(
    c(
      lines,
      "",
      paste0("[", test_group, "]"),
      group_lines,
      paste0("database=", database)
    ),
    test_cnf,
    useBytes = TRUE
  )

  list(cnf_path = test_cnf, group = test_group)
}

test_group <- Sys.getenv("GPXUI_TEST_GROUP")
if (nzchar(test_group)) {
  group <- test_group
} else {
  test_database <- Sys.getenv("GPXUI_TEST_DATABASE")
  if (!nzchar(test_database)) {
    test_database <- "tests"
  }
  test_cnf <- .test_cnf_with_database(cnf_path, group, test_database)
  cnf_path <- test_cnf$cnf_path
  group <- test_cnf$group
}

assign("cnf_path", cnf_path, envir = .GlobalEnv)
assign("group", group, envir = .GlobalEnv)


cleandb <- function() {
  con <- db_con()
  DBI::dbExecute(con, "TRUNCATE GPS_POINTS")
  DBI::dbExecute(con, "TRUNCATE GPS_TRACKS")
  DBI::dbDisconnect(con)
}
