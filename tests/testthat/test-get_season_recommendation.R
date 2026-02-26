test_that("correct months are returned", {
  expect_equal(get_season_recommendation("winter_rain")$months, 1:8)
  expect_equal(get_season_recommendation("australian")$months, c(10,11,12,1,2,3))
  expect_equal(get_season_recommendation("hyperarid")$season_days, 365L)
})

test_that("invalid zone throws error", {
  expect_error(get_season_recommendation("tropisch"))
  expect_error(get_season_recommendation(123))
})
