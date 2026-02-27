test_that("known coordinates return correct zone", {
  expect_equal(get_climate_zone( 23.0,  45.0, verbose = FALSE), "hyperarid")
  expect_equal(get_climate_zone(-25.0, 134.0, verbose = FALSE), "australian")
  expect_equal(get_climate_zone( 13.5,   2.1, verbose = FALSE), "summer_monsoon")
  expect_equal(get_climate_zone( 34.0,  -5.0, verbose = FALSE), "winter_rain_north")
  expect_equal(get_climate_zone(-33.0, -71.0, verbose = FALSE), "winter_rain_south")
  expect_equal(get_climate_zone( 48.0,  62.0, verbose = FALSE), "continental")
})

test_that("invalid coordinates throw errors", {
  expect_error(get_climate_zone( 91,   0))
  expect_error(get_climate_zone(  0, 181))
  expect_error(get_climate_zone("a",   0))
  expect_error(get_climate_zone(-91,   0))
  expect_error(get_climate_zone(  0, -181))
  expect_error(get_climate_zone(  0,   "b"))
  expect_error(get_climate_zone(c(10, 20), 0))
  expect_error(get_climate_zone(0, c(10, 20)))
})
