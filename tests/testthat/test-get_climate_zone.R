test_that("known coordinates return correct zone", {
  expect_equal(get_climate_zone(23.0, 45.0, verbose=FALSE), "hyperarid")
  expect_equal(get_climate_zone(-25.0, 134.0, verbose=FALSE), "australian")
  expect_equal(get_climate_zone(13.5, 2.1, verbose=FALSE), "summer_monsoon")
})

test_that("invalid coordinates throw errors", {
  expect_error(get_climate_zone(91, 0))
  expect_error(get_climate_zone(0, 181))
  expect_error(get_climate_zone("a", 0))
})
