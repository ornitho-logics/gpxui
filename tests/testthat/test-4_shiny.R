
cleandb()

gpx_to_database(read_all_waypoints(dirout_valid), tab = "GPS_POINTS")
gpx_to_database(read_all_tracks(dirout_valid), tab = "GPS_TRACKS")

PTS = read_GPX_table("GPS_POINTS", sf = TRUE)
TRK = read_GPX_table("GPS_TRACKS", sf = TRUE)
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
  map <- basemap()
  map |> expect_s3_class("leaflet")

  provider_calls <- Filter(
    function(x) x$method == "addProviderTiles",
    map$x$calls
  )
  expect_length(provider_calls, 3)
  expect_equal(
    vapply(provider_calls, function(x) x$args[[3]], character(1)),
    c("Print Map", "Street Map", "Satellite")
  )

  control_call <- Filter(
    function(x) x$method == "addLayersControl",
    map$x$calls
  )[[1]]
  expect_equal(
    control_call$args[[1]],
    c("Print Map", "Street Map", "Satellite")
  )
  expect_true(control_call$args[[3]]$collapsed)
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
  expect_true(all(vapply(fit_bounds[1:4], is.numeric, logical(1))))
  expect_true(all(vapply(
    fit_bounds[1:4],
    function(x) is.null(names(x)),
    logical(1)
  )))

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

  PTS <- read_GPX_table("GPS_POINTS", sf = TRUE)
  TRK <- read_GPX_table("GPS_TRACKS", sf = TRUE)

  gpx_summary(PTS, TRK) |>
    expect_s3_class("shiny.tag")
})

test_that("ui elements are shiny", {
  ctrl_title("test") |>
    expect_s3_class("shiny.tag")

  ui <- gpx_ui(gps_ids = 1:2, export_tables = "mid_points")
  ui |> expect_s3_class("shiny.tag.list")

  dependencies <- htmltools::htmlDependencies(ui)
  expect_equal(dependencies[[1]]$name, "gpxui-styles")
  expect_equal(dependencies[[1]]$stylesheet, "style.css")

  rendered_ui <- htmltools::renderTags(ui)
  expect_match(rendered_ui$html, 'class="gpx-map-shell"', fixed = TRUE)
  expect_match(
    rendered_ui$head,
    'content="width=device-width, initial-scale=1, viewport-fit=cover"',
    fixed = TRUE
  )
  expect_match(rendered_ui$html, 'data-collapsed="true"', fixed = TRUE)
  expect_match(rendered_ui$html, 'data-overlay="true"', fixed = TRUE)

  gpx_server() |>
    expect_type("closure")
})

test_that("gpx_summary() works on empty tables", {

  cleandb()

  PTS <- read_GPX_table("GPS_POINTS", sf = TRUE)
  TRK <- read_GPX_table("GPS_TRACKS", sf = TRUE)

  gpx_summary(PTS, TRK) |>
    expect_s3_class("shiny.tag")
  

})
