
cleandb()

gpx_to_database(server = "localhost", db = "tests", read_all_waypoints(dirout_valid), tab = "GPS_POINTS")
gpx_to_database(server = "localhost", db = "tests", read_all_tracks(dirout_valid), tab = "GPS_TRACKS")

PTS = read_GPX_table(server = "localhost", db = "tests", "GPS_POINTS", sf = TRUE)
TRK = read_GPX_table(server = "localhost", db = "tests", "GPS_TRACKS", sf = TRUE)
BBO  = st_bbox_all(list(PTS, TRK)) 


test_that("gpx_file_upload_check() works for both full and empty input", {
  o = gpx_file_upload_check(dirout)
  expect_s3_class(o, "shiny.tag")

  o = gpx_file_upload_check(head(dirout, 0))
  expect_s3_class(o, "shiny.tag")
})

test_that("dirInput is shiny", {
  dirInput("id") |> expect_s3_class("shiny.tag")
})

test_that("basemap is leaflet", {
  basemap() |> expect_s3_class("leaflet")
})

test_that("basemap() is updated by gpxmap() with bbox, pts and trks", {
  basemap() |>
    gpxmap(BBO, PTS, TRK) |>
    expect_s3_class("leaflet")

  fit_bounds <- (basemap() |>
    gpxmap(c(NA, 2, 3, 4), PTS, TRK))$x$fitBounds
  expect_null(fit_bounds)

  fit_bounds <- (basemap() |>
    gpxmap(c(1, 91, 2, 92), PTS, TRK))$x$fitBounds
  expect_null(fit_bounds)

  fit_bounds <- (basemap() |>
    gpxmap(c(11, 48, 11, 48), PTS, TRK))$x$fitBounds
  expect_type(fit_bounds, "list")

  fit_bounds <- (basemap() |>
    gpxmap(BBO, PTS, TRK, zoom = FALSE))$x$fitBounds
  expect_null(fit_bounds)
})

test_that("track_summary() works for both full and empty input", {

  trk_summary = track_summary(TRK)
  trk_summary |>
    expect_s3_class("data.table")

  expected_min_ele = sf::st_drop_geometry(TRK)$min_ele |> min() |> round(2)
  trk_summary[variable == "min elevation", as.numeric(value)] |>
    expect_equal(expected_min_ele)
  
  track_summary(head(TRK, 0)) |>
    suppressWarnings() |>
    expect_s3_class("data.table")

  points_summary(PTS) |>
    expect_s3_class("data.table")
  
  points_summary(head(PTS, 0)) |>
    suppressWarnings() |>
    expect_s3_class("data.table")


})

test_that("gpx_summary() works ", {
  gpx_summary(PTS, TRK) |>
    expect_s3_class("shiny.tag")
})

test_that("gpx_summary() works on empty tables", {
  cleandb()

  PTS <- read_GPX_table(server = "localhost", db = "tests", "GPS_POINTS", sf = TRUE)
  TRK <- read_GPX_table(server = "localhost", db = "tests", "GPS_TRACKS", sf = TRUE)

  gpx_summary(PTS, TRK) |>
    expect_s3_class("shiny.tag")
})

test_that("ui elements are shiny", {
  ctrl_title("test") |>
    expect_s3_class("shiny.tag")

  gpx_ui(gps_ids = 1:2, export_tables = "mid_points") |>
    expect_s3_class("shiny.tag.list")

  gpx_server(server = "localhost", db = "tests") |>
    expect_type("closure")
})

test_that("gpx_summary() works on empty tables", {

  cleandb()

  PTS <- read_GPX_table(server = "localhost", db = "tests", "GPS_POINTS", sf = TRUE)
  TRK <- read_GPX_table(server = "localhost", db = "tests", "GPS_TRACKS", sf = TRUE)

  gpx_summary(PTS, TRK) |>
    expect_s3_class("shiny.tag")
  

})
