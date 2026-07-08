#' Convert a Directory to Shiny Upload-Like Rows
#'
#' `dirInput()` returns a data frame with file names and temporary paths. This
#' helper creates the same shape from a local directory, which is useful in tests
#' and non-interactive scripts.
#'
#' @param dr Directory path to scan recursively.
#'
#' @return A `data.frame` with `name` and `datapath` columns.
#' @export
#' @examples
#' system.file(package = "gpxui", "Garmin65s") |>
#'   as_dirInput_output()
as_dirInput_output <- function(dr) {
  ff <- list.files(dr, full.names = TRUE, recursive = TRUE)

  data.frame(name = basename(ff), datapath = ff)
}

#' Convert Track Points to Lines
#'
#' Summarise point-level track data into one line geometry per group.
#'
#' @param x An `sf` object with point geometries and `ele`/`datetime_` columns.
#' @param grp Column name used to group points into lines.
#'
#' @return An `sf` object with `MULTILINESTRING` geometries.
#' @export
dt2lines <- function(x, grp) {
  x |>
    dplyr::group_by(.data[[grp]]) |>
    dplyr::summarise(
      do_union = FALSE,
      .groups = "keep",
      mean_ele = mean(.data$ele),
      max_ele = max(.data$ele),
      min_ele = min(.data$ele),
      max_dt = max(.data$datetime_),
      min_dt = min(.data$datetime_),
      n = dplyr::n()
    ) |>
    st_cast("MULTILINESTRING")
}

.empty_bbox <- function() {
  st_sfc(NULL, crs = 4326) |>
    st_as_sf() |>
    st_bbox()
}

.leaflet_bbox <- function(bbox, pad = 1e-6) {
  if (is.null(bbox)) {
    return(NULL)
  }

  bbox <- suppressWarnings(as.numeric(bbox))
  if (length(bbox) != 4 || anyNA(bbox) || any(!is.finite(bbox))) {
    return(NULL)
  }

  xmin <- min(bbox[1], bbox[3])
  xmax <- max(bbox[1], bbox[3])
  ymin <- min(bbox[2], bbox[4])
  ymax <- max(bbox[2], bbox[4])

  if (xmin < -180 || xmax > 180 || ymin < -90 || ymax > 90) {
    return(NULL)
  }

  if (xmin == xmax) {
    xmin <- max(-180, xmin - pad)
    xmax <- min(180, xmax + pad)
  }
  if (ymin == ymax) {
    ymin <- max(-90, ymin - pad)
    ymax <- min(90, ymax + pad)
  }

  if (xmin >= xmax || ymin >= ymax) {
    return(NULL)
  }

  c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
}

.has_leaflet_bbox <- function(bbox) {
  !is.null(.leaflet_bbox(bbox))
}

#' Combine Bounding Boxes
#'
#' Return one bounding box for all non-empty `sf` objects in a list. Empty or
#' non-`sf` inputs are ignored.
#'
#' @param x A list that may contain `sf` objects.
#'
#' @return An `sf` `bbox`.
#' @export
st_bbox_all <- function(x) {
  if (length(x) == 0) {
    return(.empty_bbox())
  }

  x <- x[sapply(x, inherits, what = "sf")]
  if (length(x) == 0) {
    return(.empty_bbox())
  }

  bbox_list <- lapply(x, function(obj) {
    if (nrow(obj) == 0) {
      return(NULL)
    }

    empty <- try(st_is_empty(obj), silent = TRUE)
    if (!inherits(empty, "try-error") && all(empty)) {
      return(NULL)
    }

    bbox <- try(st_bbox(obj), silent = TRUE)
    if (inherits(bbox, "try-error")) {
      return(NULL)
    }

    .leaflet_bbox(bbox)
  })
  bbox_list <- Filter(Negate(is.null), bbox_list)

  if (length(bbox_list) == 0) {
    return(.empty_bbox())
  }

  bbox_matrix <- do.call(rbind, bbox_list)
  st_bbox(
    c(
      xmin = min(bbox_matrix[, "xmin"]),
      ymin = min(bbox_matrix[, "ymin"]),
      xmax = max(bbox_matrix[, "xmax"]),
      ymax = max(bbox_matrix[, "ymax"])
    ),
    crs = 4326
  )
}


#' Write Waypoints to GPX
#'
#' @param x A data frame or `data.table` containing waypoint data.
#' @param nam Column containing waypoint names.
#' @param longit Column containing longitudes.
#' @param latit Column containing latitudes.
#' @param symbol GPX symbol name to write for every waypoint.
#' @param dest Output GPX file path.
#'
#' @return `TRUE` when `dest` exists after writing, otherwise `FALSE`.
#' @export
#' @examples
#' points <- data.table::data.table(id = "A&B", lon = 11.57, lat = 48.14)
#' DT2gpx(points, nam = "id")
DT2gpx <- function(
  x,
  nam,
  longit = "lon",
  latit = "lat",
  symbol = "Flag, Red",
  dest = tempfile(fileext = ".gpx")
) {
  required <- c(nam, longit, latit)
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  o <- data.table::as.data.table(x)[, required, with = FALSE]
  setnames(o, c("name", "lon", "lat"))

  gpx <- xml_new_root("gpx")
  xml_set_attr(gpx, "version", "1.1")
  xml_set_attr(gpx, "creator", "gpxui")
  xml_set_attr(gpx, "xmlns", "http://www.topografix.com/GPX/1/1")

  for (row in seq_len(nrow(o))) {
    wpt <- xml_add_child(gpx, "wpt")
    xml_set_attr(wpt, "lat", format(o$lat[row], scientific = FALSE, trim = TRUE))
    xml_set_attr(wpt, "lon", format(o$lon[row], scientific = FALSE, trim = TRUE))
    xml_add_child(wpt, "name", as.character(o$name[row]))
    xml_add_child(wpt, "sym", symbol)
  }

  write_xml(gpx, file = dest, options = "format")

  file.exists(dest)
}
