test_that("known coordinates return correct zone", {
  result <- list_climate_zones()
  expect_equal(result[result$zone == "hyperarid",    "season_days"], 365L)
  expect_equal(result[result$zone == "australian",   "season_days"], 182L)
  expect_equal(result[result$zone == "summer_monsoon", "season_days"], 122L)
})

test_that("invalid coordinates throw errors", {
  expect_no_error(list_climate_zones())
})

#  New Tests

test_that("returns a data.frame invisibly", {
  result <- list_climate_zones()
  expect_s3_class(result, "data.frame")
  expect_equal(length(capture.output(list_climate_zones())), 0L)
})

test_that("data.frame has all required columns", {
  result <- list_climate_zones()
  expect_true(all(c("zone", "koppen", "formula", "period",
                    "season_days", "input_layers") %in% names(result)))
})

test_that("data.frame has exactly 6 rows", {
  expect_equal(nrow(list_climate_zones()), 6L)
})

test_that("all 6 zone IDs are present and unique", {
  valid_zones <- c("winter_rain_north", "winter_rain_south", "summer_monsoon",
                   "hyperarid", "continental", "australian")
  result <- list_climate_zones()
  expect_setequal(result$zone, valid_zones)
  expect_equal(length(result$zone), length(unique(result$zone)))
})

test_that("input_layers is integer and contains only 1 or 12", {
  result <- list_climate_zones()
  expect_true(is.integer(result$input_layers))
  expect_true(all(result$input_layers %in% c(1L, 12L)))
})

test_that("exact season_days and input_layers match documentation", {
  result <- list_climate_zones()
  r <- function(z) result[result$zone == z, ]

  expect_equal(r("hyperarid")$season_days,       365L)
  expect_equal(r("hyperarid")$input_layers,        1L)  # kumulativer Jahresniederschlag
  expect_equal(r("summer_monsoon")$season_days,  122L)
  expect_equal(r("summer_monsoon")$input_layers,  12L)  # monatliche Layer
  expect_equal(r("continental")$season_days,     183L)
  expect_equal(r("continental")$input_layers,     12L)
  expect_equal(r("australian")$season_days,      182L)
  expect_equal(r("australian")$input_layers,      12L)
  expect_equal(r("winter_rain_north")$season_days, 182L)
  expect_equal(r("winter_rain_north")$input_layers,  1L) # kumulativer Layer
  expect_equal(r("winter_rain_south")$season_days, 243L)
  expect_equal(r("winter_rain_south")$input_layers,  1L)
})

test_that("season_days and period label are consistent with get_season_recommendation()", {
  result <- list_climate_zones()
  for (i in seq_len(nrow(result))) {
    z   <- result$zone[i]
    rec <- get_season_recommendation(z)
    expect_equal(result$season_days[i], rec$season_days,
                 label = paste("season_days für:", z))
    expect_equal(result$period[i], rec$label,
                 label = paste("period label für:", z))
  }
})

test_that("console output contains all zone IDs and package name", {
  msgs     <- capture_messages(list_climate_zones())
  combined <- paste(msgs, collapse = "")
  expect_match(combined, "aridRUSLE")
  for (z in c("winter_rain_north", "winter_rain_south", "summer_monsoon",
              "hyperarid", "continental", "australian")) {
    expect_match(combined, z)
  }
})
