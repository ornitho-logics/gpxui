#' Read Waypoints and Tracks
#'
#' These functions are wrappers around `sf::st_read`, reading waypoints and tracks from GPX files.
#' If the file is corrupt or empty, the functions return an empty `data.table` with a warning.
#'
#' @name read_gpx
#' @aliases read_waypoints read_tracks
#'
#' @param x A path to a GPX file.
#'
#' @return A `data.table` containing:
#' \describe{
#'   \item{gps_point}{For waypoints: waypoint name. For tracks: track segment identifier.}
#'   \item{datetime_}{Timestamp of the recorded point (if available).}
#'   \item{ele}{Elevation in meters.}
#'   \item{lon}{Longitude.}
#'   \item{lat}{Latitude.}
#' }
#'
#' @examples
#' f1 = system.file(package = "gpxui", "Garmin65s", "GPX", "Waypoints_20-APR-23.gpx")
#' f2 = system.file(package = "gpxui", "Garmin65s", "GPX", "Waypoints_empty.gpx")
#' f3 = system.file(package = "gpxui", "Garmin65s", "GPX", "Current", "Current.gpx")
#' read_waypoints(f1)
#' read_waypoints(f2)
#' read_tracks(f1)
#' read_tracks(f2)
#' read_tracks(f3)
#'
#' @rdname read_gpx
#' @export

read_waypoints <- function(x) {
  w <- try(st_read(x, layer = "waypoints", quiet = TRUE), silent = TRUE)
  if (inherits(w, "sf")) {
    xy <- st_coordinates(w) |> data.table()
    setnames(xy, c("lon", "lat"))
    d <- st_drop_geometry(w) |> setDT()
    d <- d[, .(gps_point = name, datetime_ = time, ele)]
    o <- cbind(d, xy)
    if (nrow(o) == 0) {
      message(basename(x) |> dQuote(), " does not contain any points!")
    }
  } else {
    warning(basename(x) |> dQuote(), " is empty or corrupt!")
    o <- data.table(
      gps_point = character(),
      datetime_ = as.POSIXct(NULL),
      ele = numeric(),
      lon = numeric(),
      lat = numeric()
    )
  }

  o
}

#' @rdname read_gpx
#' @export
read_tracks <- function(x) {
  w <- try(st_read(x, layer = "track_points", quiet = TRUE), silent = TRUE)
  if (inherits(w, "sf")) {
    xy <- st_coordinates(w) |> data.table()
    setnames(xy, c("lon", "lat"))
    d <- st_drop_geometry(w) |> setDT()
    d <- d[, .(
      seg_id = track_seg_id,
      seg_point_id = track_seg_point_id,
      datetime_ = time,
      ele
    )]
    o <- cbind(d, xy)
    if (nrow(o) == 0) {
      message(basename(x) |> dQuote(), " does not contain any tracks!")
    }
  } else {
    warning(basename(x) |> dQuote(), " is empty or corrupt!")
    o <- data.table(
      seg_id = integer(),
      seg_point_id = integer(),
      datetime_ = as.POSIXct(NULL),
      ele = numeric(),
      lon = numeric(),
      lat = numeric()
    )
  }
  o
}


#' deviceID
#' @param x a data.frame  uploaded to the server by dirInput
#' @export
deviceID <- function(x) {
  z <- data.table(x)
  path_to_id <- z[name == "DEVICE_ID.txt", datapath]

  o <- try(
    readLines(path_to_id)[1] |>
      as.numeric(),
    silent = TRUE
  )

  if (inherits(o, "try-error")) {
    o <- NA
  }

  o
}


#' read_all_waypoints
#' @description  read all waypoints from the GPX directory and the gps id from DEVICE_ID.txt when it exists
#' @param  ff  a data.frame  uploaded to the server by dirInput or
#'             a directory path containing gpx files.
#' @param int_names_only keep only numeric names
#' @export
#' @examples
#' g = system.file("Garmin65s", package = "gpxui") |> read_all_waypoints()
read_all_waypoints <- function(ff, int_names_only = TRUE) {
  if (!inherits(ff, 'data.frame') && fs::dir_exists(ff)) {
    ff <- data.frame(
      datapath = list.files(ff, full.names = TRUE, recursive = TRUE)
    )
    ff$name <- basename(ff$datapath)
  }
  gid <- deviceID(ff)

  ff <- ff$datapath
  ff <- ff[basename(ff) |> str_detect("gpx$")]

  if (length(ff) > 0) {
    o <- lapply(ff, read_waypoints) |>
      rbindlist()
    o[, gps_id := gid]

    if (int_names_only) {
      o[, gps_point := as.integer(gps_point)]
      o <- o[!is.na(gps_point)]
    }

    setcolorder(o, "gps_id")
  } else {
    o <- NULL
  }

  o
}

#' read_all_tracks
#' @description  read all tracks from the GPX directory and the gps id from DEVICE_ID.txt when it exists
#' @param  ff  a data.frame  uploaded to the server by dirInput or
#'             a directory path containing gpx files.
#' @export
#' @examples
#' g = system.file("Garmin65s", package = "gpxui") |> read_all_tracks()
read_all_tracks <- function(ff) {
  if (!inherits(ff, 'data.frame') && fs::dir_exists(ff)) {
    ff <- data.frame(
      datapath = list.files(ff, full.names = TRUE, recursive = TRUE)
    )
    ff$name <- basename(ff$datapath)
  }
  gid <- deviceID(ff)

  ff <- ff$datapath
  ff <- ff[basename(ff) |> str_detect("gpx$")]

  if (length(ff) > 0) {
    o <- lapply(ff, read_tracks) |>
      rbindlist()
    o[, gps_id := gid]

    setcolorder(o, "gps_id")
  } else {
    o <- NULL
  }

  o
}
