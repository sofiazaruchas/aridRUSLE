test_that("list_climate_zones returns a data.frame", {
  result <- list_climate_zones()
  expect_s3_class(result, "data.frame")
})

test_that("list_climate_zones returns exactly five zones", {
  result <- list_climate_zones()
  expect_equal(nrow(result), 5)
})

test_that("all expected zone IDs are present", {
  result <- list_climate_zones()
  expect_equal(result$zone, c("winter_rain", "summer_monsoon", "hyperarid",
                              "continental", "australian"))
})

test_that("input_layers is either 1 or 12", {
  result <- list_climate_zones()
  expect_true(all(result$input_layers %in% c(1L, 12L)))
})
