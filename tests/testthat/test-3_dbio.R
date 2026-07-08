
cleandb()

test_that("gpx_to_database() works as expected on valid inputs", {
  pp = read_all_waypoints(dirout_valid)
  tt = read_all_tracks(dirout_valid)

  # POINTS
  op = gpx_to_database(server = "localhost", db = "tests", pp, tab = "GPS_POINTS")
  expect_s3_class(op, "data.frame")

  # subsequent update
  o = gpx_to_database(server = "localhost", db = "tests", pp, tab = "GPS_POINTS")
  expect_s3_class(o, "data.frame")
  expect_true(o$rows_in_db_after_update == 0)

  # TRACKS
  ot = gpx_to_database(server = "localhost", db = "tests", tt, tab = "GPS_TRACKS")
  expect_s3_class(ot, "data.frame")

  # subsequent update
  o = gpx_to_database(server = "localhost", db = "tests", tt, tab = "GPS_TRACKS")
  expect_s3_class(o, "data.frame")
  expect_true(o$rows_in_db_after_update == 0)

  # both outputs have similar outputs
  rbindlist(list(op, ot)) |> expect_s3_class("data.table")

})

test_that("gpx_to_database() does not error on invalid inputs", {

  # NULL
  o = gpx_to_database(server = "localhost", db = "tests", NULL, tab = "GPS_POINTS")
  expect_s3_class(o, "data.frame")
  expect_true(o$rows_in_db_after_update == 0)
  
  # NA gps_id
  pp = read_all_waypoints(dirout_valid)[, gps_id := NA]

  o = gpx_to_database(server = "localhost", db = "tests", pp, tab = "GPS_POINTS")
  expect_s3_class(o, "data.frame")
  expect_true(o$rows_in_db_after_update == 0)


  

})

test_that("gpx_to_database() works as when there is no device_id file", {


  pp = read_all_waypoints(dirout_noid)

  o = gpx_to_database(server = "localhost", db = "tests", pp, tab = "GPS_POINTS")
  expect_s3_class(o, "data.frame")
  expect_true(o$rows_in_db_after_update == 0)


  

})

test_that("read_GPX_table() works as expected w. dt and sf output", {

  read_GPX_table(server = "localhost", db = "tests", "GPS_POINTS") |>
    expect_s3_class("data.frame")
  o = read_GPX_table(server = "localhost", db = "tests", "GPS_POINTS", dt = "2023-04-20 10:10")
  expect_true( all(o$datetime_ > as.POSIXct("2023-04-20 10:10")) )
  read_GPX_table(server = "localhost", db = "tests", "GPS_POINTS", sf = TRUE) |>
      expect_s3_class("sf")


  read_GPX_table(server = "localhost", db = "tests", "GPS_TRACKS") |>
    expect_s3_class("data.frame")
  x = read_GPX_table(server = "localhost", db = "tests", "GPS_TRACKS", dt = "2023-04-20 10:10")
  expect_true( all(o$datetime_ > as.POSIXct("2023-04-20 10:10")) )
  read_GPX_table(server = "localhost", db = "tests", "GPS_TRACKS", sf = TRUE) |>
    expect_s3_class("sf")
  

})

test_that("read_GPX_table() works as expected w. gps_id", {

  read_GPX_table(server = "localhost", db = "tests", tab = "GPS_POINTS", gps_id = 1) |>
    expect_s3_class("data.frame")
  
  read_GPX_table(server = "localhost", db = "tests", tab = "GPS_POINTS", gps_id = "01") |>
    expect_s3_class("data.frame")
  
  read_GPX_table(server = "localhost", db = "tests", tab = "GPS_POINTS", gps_id = c(1, 2, NA, "x")) |>
    suppressWarnings() |>
    expect_s3_class("data.frame")

  read_GPX_table(server = "localhost", db = "tests", tab = "GPS_POINTS", gps_id = 'x') |>
    suppressWarnings() |>
    expect_s3_class("data.frame")


})

test_that("gpx_export() works as expected ", {

  f1 = tempfile(fileext = ".gpx")
  gpx_export(server = "localhost", db = "tests", tab = "mid_points", file = f1)

  f2 = tempfile(fileext = ".csv")
  gpx_export(server = "localhost", db = "tests", tab = "mid_tracks", file = f2)

  expect_true(file.exists(f1))
  expect_true(file.exists(f2))





})
