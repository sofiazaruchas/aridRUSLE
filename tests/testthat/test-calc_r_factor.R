make_raster <- function(value, n_layers = 1) {
  r <- terra::rast(
    nrows = 3, ncols = 3,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  if (n_layers == 1) {
    terra::values(r) <- value
  } else {
    r <- terra::rast(replicate(n_layers, {
      layer <- terra::rast(
        nrows = 3, ncols = 3,
        xmin = 0, xmax = 1, ymin = 0, ymax = 1,
        crs = "EPSG:4326"
      )
      terra::values(layer) <- value
      layer
    }))
  }
  r
}

# Input validation

test_that("throws error if precip_raster is not a SpatRaster", {
  expect_error(
    calc_r_factor(matrix(1:9, 3, 3), climate_zone = "hyperarid"),
    "'precip_raster' must be a terra::SpatRaster object"
  )
})

test_that("throws error if layer count is not 1 or 12", {
  bad <- make_raster(50, n_layers = 6)
  expect_error(
    calc_r_factor(bad, climate_zone = "hyperarid"),
    "must have either 1 layer.*or 12 layers"
  )
})

test_that("throws error if climate_zone is invalid", {
  r <- make_raster(100)
  expect_error(
    calc_r_factor(r, climate_zone = "tropical"),
    "Unknown climate zone"
  )
})

test_that("throws error if neither climate_zone nor lat/lon are provided", {
  r <- make_raster(100)
  expect_error(
    calc_r_factor(r),
    "Either 'climate_zone' or both 'lat' and 'lon' must be provided"
  )
})

test_that("throws error if only lat is provided without lon", {
  r <- make_raster(100)
  expect_error(
    calc_r_factor(r, lat = -33.0),
    "Either 'climate_zone' or both 'lat' and 'lon' must be provided"
  )
})

test_that("throws error if 12-layer zone receives 1-layer raster", {
  r <- make_raster(50, n_layers = 1)
  expect_error(
    calc_r_factor(r, climate_zone = "summer_monsoon"),
    "requires 12 monthly layers"
  )
})

test_that("throws error if 1-layer zone receives 12-layer raster", {
  r <- make_raster(50, n_layers = 12)
  expect_error(
    calc_r_factor(r, climate_zone = "hyperarid"),
    "requires 1 cumulative layer"
  )
})

# Return type and structure

test_that("returns a SpatRaster", {
  r <- make_raster(100)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expect_s4_class(result, "SpatRaster")
})

test_that("output has exactly 1 layer", {
  r <- make_raster(100)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expect_equal(terra::nlyr(result), 1)
})

test_that("output layer is named 'R_factor'", {
  r <- make_raster(100)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expect_equal(names(result), "R_factor")
})

test_that("output has same extent and resolution as input", {
  r <- make_raster(100)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expect_equal(as.vector(terra::ext(result)), as.vector(terra::ext(r)))
  expect_equal(terra::res(result), terra::res(r))
})

test_that("r_factor_meta attribute is attached to output", {
  r <- make_raster(100)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  meta <- attr(result, "r_factor_meta")
  expect_type(meta, "list")
  expect_named(meta, c("zone", "formula_name", "season_recommendation", "params"),
               ignore.order = TRUE)
})

test_that("r_factor_meta contains correct zone", {
  r <- make_raster(100)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expect_equal(attr(result, "r_factor_meta")$zone, "hyperarid")
})

test_that("r_factor_meta contains correct formula_name", {
  r <- make_raster(100)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expect_equal(attr(result, "r_factor_meta")$formula_name,
               "Simplified MFI with aridity correction")
})

# plot parameter

test_that("plot = FALSE produces no plot", {
  r <- make_raster(100)
  expect_no_error(
    calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  )
})

test_that("plot = TRUE runs without error", {
  r <- make_raster(100)
  expect_no_error(
    calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = TRUE)
  )
})

# Formula correctness: hyperarid
# R = 0.085 * P^1.350

test_that("hyperarid: R-factor matches expected value for P = 100 mm", {
  r        <- make_raster(100)
  result   <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expected <- 0.085 * (100 ^ 1.350)
  expect_equal(
    terra::global(result, "mean", na.rm = TRUE)[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("hyperarid: R-factor matches expected value for P = 50 mm", {
  r        <- make_raster(50)
  result   <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expected <- 0.085 * (50 ^ 1.350)
  expect_equal(
    terra::global(result, "mean", na.rm = TRUE)[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("hyperarid: all output values are non-negative", {
  r      <- make_raster(10)
  result <- calc_r_factor(r, climate_zone = "hyperarid", verbose = FALSE, plot = FALSE)
  expect_true(terra::global(result, "min", na.rm = TRUE)[[1]] >= 0)
})

# -- Formula correctness: winter_rain_south -----------------------------------
# R = a * P^b * (season_days / 365)  |  defaults: a=0.171, b=1.212, days=243

test_that("winter_rain_south: R-factor matches expected value with defaults", {
  P        <- 300
  r        <- make_raster(P)
  result   <- calc_r_factor(r, climate_zone = "winter_rain_south", verbose = FALSE, plot = FALSE)
  expected <- 0.171 * (P ^ 1.212) * (243 / 365)
  expect_equal(
    terra::global(result, "mean", na.rm = TRUE)[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("winter_rain_south: custom a, b, season_days are applied correctly", {
  P        <- 200
  r        <- make_raster(P)
  result   <- calc_r_factor(r, climate_zone = "winter_rain_south",
                            a = 0.200, b = 1.300, season_days = 182,
                            verbose = FALSE, plot = FALSE)
  expected <- 0.200 * (P ^ 1.300) * (182 / 365)
  expect_equal(
    terra::global(result, "mean", na.rm = TRUE)[[1]],
    expected,
    tolerance = 1e-6
  )
})
