#' Create a MariaDB Connection
#'
#' Creates a MariaDB connection from an option file.
#'
#' If `.cnf` is `NULL`, `db_con()` first looks for `cnf_path` in the global
#' environment, then for the `GPXUI_CNF` environment variable.
#'
#' If `group` is `NULL`, `db_con()` first looks for `group` in the global
#' environment, then uses `"gpxui"`.
#'
#' The database/schema should be configured in the selected option group.
#'
#' @param .cnf Path to a MariaDB option file. If `NULL`, resolved from
#'   `cnf_path` or `GPXUI_CNF`.
#' @param group Option group in `.cnf`. If `NULL`, resolved from global
#'   `group` or `"gpxui"`.
#'
#' @return A MariaDB connection.
#' @export
db_con <- function(.cnf = NULL, group = NULL) {
  if (is.null(.cnf)) {
    if (exists("cnf_path", envir = .GlobalEnv, inherits = FALSE)) {
      .cnf <- get("cnf_path", envir = .GlobalEnv)
    } else {
      .cnf <- Sys.getenv("GPXUI_CNF")
    }
  }

  if (!nzchar(.cnf)) {
    stop(
      "Set `cnf_path` in the global environment or GPXUI_CNF.",
      call. = FALSE
    )
  }

  if (is.null(group)) {
    if (exists("group", envir = .GlobalEnv, inherits = FALSE)) {
      group <- get("group", envir = .GlobalEnv)
    } else {
      group <- "gpxui"
    }
  }

  if (!nzchar(group)) {
    stop("`group` must be a non-empty string.", call. = FALSE)
  }

  DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    default.file = path.expand(.cnf),
    group = group
  )
}

#' Run a MariaDB Query and Return a Data Table
#'
#' @param query SQL query passed to [DBI::dbGetQuery()].
#' @param ... Additional arguments passed to [db_con()].
#' @param params Optional query parameters passed to [DBI::dbGetQuery()].
#'
#' @return A `data.table`.
#' @export
db_get <- function(query, ..., params = NULL) {
  con <- db_con(...)
  on.exit(DBI::dbDisconnect(con))

  if (is.null(params)) {
    x <- DBI::dbGetQuery(con, query)
  } else {
    x <- DBI::dbGetQuery(con, query, params = params)
  }

  x |> data.table::as.data.table()
}

#' gpx_to_database
#' saves new entries to database
#' @param x output of [gpxui::read_all_waypoints()] or [gpxui::read_all_tracks()].
#' @param tab database table (GPS_TRACKS, GPS_POINTS)
#' @param .cnf Path to a MariaDB option file passed to [db_con()].
#' @param group Option group in `.cnf` passed to [db_con()].
#' @return a data.frame containing last entry in db prior to database update and the number of updated entries.
#' @export
gpx_to_database <- function(
  x,
  tab,
  .cnf = NULL,
  group = NULL
) {
  if (!is.null(x) && nrow(x) > 0) {
    con <- db_con(.cnf = .cnf, group = group)
    on.exit(DBI::dbDisconnect(con))

    gid <- x$gps_id[1]

    if (!is.na(gid)) {
      lastdt <- DBI::dbGetQuery(
        con,
        glue(
          "SELECT max(datetime_) dt from {tab}
                          WHERE gps_id = {gid}"
        )
      )$dt

      if (is.na(lastdt)) {
        # revert to the date of the first GPS ever produced
        lastdt <- as.POSIXct("1989-10-01", format = "%Y-%m-%d")
      }

      x1 <- x[datetime_ > lastdt]

      rows_in_db_after_update <- DBI::dbAppendTable(con, tab, x1)

      o <- data.frame(
        last_entry_before_update = lastdt,
        rows_in_db_after_update,
        tab = tab,
        gps_id = gid
      )
    } else {
      o <- data.frame(
        last_entry_before_update = as.POSIXct(NA),
        rows_in_db_after_update = 0,
        tab = tab,
        gps_id = as.numeric(NA)
      )
    }
  } else {
    o <- data.frame(
      last_entry_before_update = as.POSIXct(NA),
      rows_in_db_after_update = 0,
      tab = tab,
      gps_id = as.numeric(NA)
    )
  }

  o
}

#' read_GPX_table
#' Fetch database tables
#' @param tab database table   (GPS_TRACKS, GPS_POINTS)
#' @param dt  database valid datetime. Only entries after this are returned. Default to "1900-01-01"
#' @param gps_id  gps id (one or more). Disregarded when missing, NA, NULL or non-numeric
#' @param sf when TRUE returns a sf df. default to FALSE
#' @param .cnf Path to a MariaDB option file passed to [db_con()].
#' @param group Option group in `.cnf` passed to [db_con()].
#' @export
read_GPX_table <- function(
  tab,
  dt = "1900-01-01",
  gps_id,
  sf = FALSE,
  .cnf = NULL,
  group = NULL
) {
  if (!missing(gps_id)) {
    gps_id <- as.numeric(gps_id) |> unique() |> na.omit()
  }

  sql <- glue("SELECT * FROM {tab} where datetime_ > {shQuote(dt)}")

  if (!missing(gps_id) && length(gps_id) > 0) {
    sql <- glue("{sql} AND gps_id IN ( {paste(gps_id, collapse = ',')} )")
  }

  o <- db_get(sql, .cnf = .cnf, group = group)

  if (sf & nrow(o) > 0) {
    o <- st_as_sf(o, coords = c("lon", "lat"), crs = 4326)

    if (tab == "GPS_TRACKS") {
      o <- dt2lines(o, "seg_id")
    }
  }

  o
}


#' Export a GPX Database Table
#'
#' @param tab Database table or view to export.
#' @param file Output file path ending in `.gpx` or `.csv`.
#' @param .cnf Path to a MariaDB option file passed to [db_con()].
#' @param group Option group in `.cnf` passed to [db_con()].
#'
#' @return For GPX output, the result of [DT2gpx()]. For CSV output,
#'   invisibly returns `NULL`.
#' @export
gpx_export <- function(
  tab,
  file,
  .cnf = NULL,
  group = NULL
) {
  con <- db_con(.cnf = .cnf, group = group)
  on.exit(DBI::dbDisconnect(con))

  x <- DBI::dbReadTable(con, tab) |> setDT()

  if (ncol(x) != 3 & !all(c('lat', 'lon') %in% names(x))) {
    stop("x has to have exactly 3 columns and col 2 & 3 are lat, lon")
  }

  fnam <- basename(file)
  ext <- str_extract(fnam, "gpx$|csv$")

  if (ext == 'gpx') {
    DT2gpx(x, nam = names(x)[1], dest = file)
  }

  if (ext == 'csv') {
    fwrite(x, file)
  }
}
