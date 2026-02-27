test_that("known coordinates return correct zone", {
  expect_equal(get_season_recommendation("hyperarid")$season_days,    365L)
  expect_equal(get_season_recommendation("australian")$season_days,   182L)
  expect_equal(get_season_recommendation("summer_monsoon")$season_days, 122L)
})

test_that("invalid coordinates throw errors", {
  expect_error(get_season_recommendation(1))
  expect_error(get_season_recommendation("tropics"))
  expect_error(get_season_recommendation("a"))
  # additions
  expect_error(get_season_recommendation("winter_rain"))   # alter Name ohne Suffix
  expect_error(get_season_recommendation(NA_character_))
  expect_error(get_season_recommendation(c("hyperarid", "continental")))
})

#  New Tests

test_that("error messages are informative", {
  expect_error(get_season_recommendation(1),         "'zone' must be a single character string")
  expect_error(get_season_recommendation("tropics"),  "must be one of")
})

test_that("every valid zone returns a list with all required fields", {
  valid_zones     <- c("winter_rain_north", "winter_rain_south", "summer_monsoon",
                       "hyperarid", "continental", "australian")
  required_fields <- c("zone", "label", "months", "months_label", "season_days", "note")
  for (z in valid_zones) {
    result <- get_season_recommendation(z)
    expect_type(result, "list")
    expect_true(all(required_fields %in% names(result)))
  }
})

test_that("zone field echoes the input", {
  for (z in c("hyperarid", "continental", "australian")) {
    expect_equal(get_season_recommendation(z)$zone, z)
  }
})

test_that("exact months and season_days match Tabelle 2 of the documentation", {
  r <- get_season_recommendation("winter_rain_north")
  expect_equal(r$months, c(10, 11, 12, 1, 2, 3))
  expect_equal(r$season_days, 182L)

  r <- get_season_recommendation("winter_rain_south")
  expect_equal(r$months, 1:8)
  expect_equal(r$season_days, 243L)

  r <- get_season_recommendation("summer_monsoon")
  expect_equal(r$months, 6:9)
  expect_equal(r$season_days, 122L)

  r <- get_season_recommendation("hyperarid")
  expect_equal(r$months, 1:12)
  expect_equal(r$season_days, 365L)

  r <- get_season_recommendation("continental")
  expect_equal(r$months, 4:9)
  expect_equal(r$season_days, 183L)

  r <- get_season_recommendation("australian")
  expect_equal(r$months, c(10, 11, 12, 1, 2, 3))
  expect_equal(r$season_days, 182L)
})

test_that("winter_rain_north and australian share identical months and season_days", {
  north <- get_season_recommendation("winter_rain_north")
  aus   <- get_season_recommendation("australian")
  expect_equal(north$months,      aus$months)
  expect_equal(north$season_days, aus$season_days)
})

test_that("hyperarid covers all 12 months (no seasonal pattern)", {
  r <- get_season_recommendation("hyperarid")
  expect_equal(sort(r$months), 1:12)
})

test_that("no zone has duplicate months", {
  valid_zones <- c("winter_rain_north", "winter_rain_south", "summer_monsoon",
                   "hyperarid", "continental", "australian")
  for (z in valid_zones) {
    m <- get_season_recommendation(z)$months
    expect_equal(length(m), length(unique(m)))
  }
})

test_that("months_label entry count matches length of months vector", {
  valid_zones <- c("winter_rain_north", "winter_rain_south", "summer_monsoon",
                   "hyperarid", "continental", "australian")
  for (z in valid_zones) {
    r     <- get_season_recommendation(z)
    parts <- strsplit(r$months_label, ",")[[1]]
    expect_equal(length(parts), length(r$months))
  }
})
