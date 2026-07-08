


test_that("read_waypoints() returns a dt", {
  read_waypoints(gpxfile) |> expect_s3_class("data.table")
})

test_that("read_tracks() returns a dt", {

  read_tracks(gpxtrackfile) |> expect_s3_class("data.table")


})

test_that("read_gpx() warns on empty GPX files", {
  expect_warning(read_waypoints(gpxfile_empty), "empty or corrupt")
  expect_warning(read_tracks(gpxfile_empty), "empty or corrupt")
})

test_that("deviceID() works on an as_dirInput_output() return", {
  deviceID(dirout) |> expect_identical(1)
})

test_that("read_all_waypoints() returns a proper dt", {
  z = read_all_waypoints(dirout_valid)

  expect_s3_class(z, "data.table")

  nams = c("gps_id", "gps_point", "datetime_", "ele", "lon", "lat")

  expect_equal(names(z), nams)
})

test_that("read_all_waypoints() returns NULL on zero rows input", {
  head(dirout, 0) |>
    read_all_waypoints() |>
    expect_null()
})

test_that("read_all_tracks() returns a proper dt", {
  z = read_all_tracks(dirout_valid)

  expect_s3_class(z, "data.table")

  nams = c("gps_id", "seg_id", "seg_point_id", "datetime_", "ele", "lon","lat")

  expect_equal(names(z), nams)
})

test_that("read_all_tracks() returns NULL on zero rows input", {
  head(dirout, 0) |>
    read_all_tracks() |>
    expect_null()
})

test_that("DT2gpx writes a gpx file", {
  outf = tempfile(fileext = ".gpx")
  z = read_all_waypoints(dirout_valid)


  DT2gpx(z, nam = "gps_point", dest = outf) |> expect_true()

  o = suppressWarnings({
    o = sf::st_read(outf, layer = "waypoints", quiet = TRUE)
  })
  
  expect_s3_class(o, "sf")

  special = data.table::data.table(gps_point = "A&B", lon = 11.57, lat = 48.14)
  DT2gpx(special, nam = "gps_point", dest = outf) |> expect_true()

  o = suppressWarnings({
    sf::st_read(outf, layer = "waypoints", quiet = TRUE)
  })
  sf::st_drop_geometry(o)$name |> expect_equal("A&B")
  
})
