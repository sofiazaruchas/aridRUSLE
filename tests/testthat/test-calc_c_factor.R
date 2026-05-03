# tests/testthat/test-calc_c_factor.R

#  Helpers

make_ndvi <- function(value, nrows = 5, ncols = 5) {
  r <- terra::rast(
    nrows = nrows, ncols = ncols,
    xmin  = 0, xmax = 1, ymin = 0, ymax = 1,
    crs   = "EPSG:4326"
  )
  terra::values(r) <- value
  r
}

# Input validation

test_that("throws error if ndvi_raster is not a SpatRaster", {
  expect_error(
    calc_c_factor(matrix(1:25, 5, 5)),
    "'ndvi_raster' must be a terra::SpatRaster object"
  )
})

test_that("throws error if ndvi_raster has more than 1 layer", {
  r  <- make_ndvi(0.5)
  r2 <- c(r, r)
  expect_error(
    calc_c_factor(r2),
    "'ndvi_raster' must have exactly 1 layer"
  )
})

test_that("produces warning if NDVI values are outside -1 to 1", {
  r <- make_ndvi(2.0)
  expect_warning(
    calc_c_factor(r, verbose = FALSE, plot = FALSE),
    "outside the valid NDVI range"
  )
})

# Return type and structure

test_that("returns a SpatRaster", {
  r      <- make_ndvi(0.3)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_s4_class(result, "SpatRaster")
})

test_that("output has exactly 1 layer", {
  r      <- make_ndvi(0.3)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(terra::nlyr(result), 1)
})

test_that("output layer is named 'C_factor'", {
  r      <- make_ndvi(0.3)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(names(result), "C_factor")
})

test_that("output has same extent as input", {
  r      <- make_ndvi(0.3)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(as.vector(terra::ext(result)), as.vector(terra::ext(r)))
})

test_that("output has same resolution as input", {
  r      <- make_ndvi(0.3)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(terra::res(result), terra::res(r))
})

test_that("output has same CRS as input", {
  r      <- make_ndvi(0.3)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(terra::crs(result), terra::crs(r))
})

# -- Formula correctness
# C = 0.353 * exp(1.669 * NDVI), clamped to [0, 1]

test_that("C-factor matches expected value for NDVI = 0", {
  r        <- make_ndvi(0)
  result   <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expected <- min(max(0.353 * exp(1.669 * 0), 0), 1)
  expect_equal(
    terra::global(result, "mean", na.rm = TRUE)[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("C-factor matches expected value for NDVI = 0.5", {
  r        <- make_ndvi(0.5)
  result   <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expected <- min(max(0.353 * exp(1.669 * 0.5), 0), 1)
  expect_equal(
    terra::global(result, "mean", na.rm = TRUE)[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("C-factor matches expected value for NDVI = -0.5", {
  r        <- make_ndvi(-0.5)
  result   <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expected <- min(max(0.353 * exp(1.669 * -0.5), 0), 1)
  expect_equal(
    terra::global(result, "mean", na.rm = TRUE)[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("C-factor increases as NDVI increases (before clamping)", {
  r_low  <- make_ndvi(0.1)
  r_high <- make_ndvi(0.5)

  c_low  <- calc_c_factor(r_low,  verbose = FALSE, plot = FALSE)
  c_high <- calc_c_factor(r_high, verbose = FALSE, plot = FALSE)

  mean_low  <- terra::global(c_low,  "mean", na.rm = TRUE)[[1]]
  mean_high <- terra::global(c_high, "mean", na.rm = TRUE)[[1]]

  expect_true(mean_low < mean_high)
})

test_that("C-factor is clamped to 1 for very high NDVI values", {
  # NDVI = 1 -> 0.353 * exp(1.669) ~ 1.85 -> clamped to 1
  r      <- make_ndvi(1.0)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(
    terra::global(result, "max", na.rm = TRUE)[[1]],
    1.0,
    tolerance = 1e-6
  )
})

test_that("C-factor is clamped to 0 for very low NDVI values", {
  r      <- make_ndvi(-1.0)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_true(terra::global(result, "min", na.rm = TRUE)[[1]] >= 0)
})

test_that("all output values are within 0 to 1", {
  r       <- make_ndvi(0.4)
  result  <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  min_val <- terra::global(result, "min", na.rm = TRUE)[[1]]
  max_val <- terra::global(result, "max", na.rm = TRUE)[[1]]
  expect_true(min_val >= 0 && max_val <= 1)
})

# NA handling

test_that("NA cells in input are preserved as NA in output", {
  r <- make_ndvi(0.3)
  terra::values(r)[1] <- NA
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_true(is.na(terra::values(result)[1]))
})

test_that("non-NA cells are unaffected by NA neighbours", {
  r <- make_ndvi(0.3)
  terra::values(r)[1] <- NA
  result   <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expected <- min(max(0.353 * exp(1.669 * 0.3), 0), 1)
  expect_equal(terra::values(result)[2], expected, tolerance = 1e-6)
})

test_that("all-NA input returns all-NA output", {
  r      <- make_ndvi(NA)
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_true(all(is.na(terra::values(result))))
})

# Water mask propagation

test_that("NA pixels from water mask are preserved as NA in C-factor", {
  r <- make_ndvi(0.4)
  terra::values(r)[c(1, 5, 10)] <- NA
  result <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expect_true(all(is.na(terra::values(result)[c(1, 5, 10)])))
})

test_that("non-water pixels are correctly computed after masking", {
  r <- make_ndvi(0.4)
  terra::values(r)[c(1, 5, 10)] <- NA
  result   <- calc_c_factor(r, verbose = FALSE, plot = FALSE)
  expected <- min(max(0.353 * exp(1.669 * 0.4), 0), 1)
  non_na   <- terra::values(result)[!is.na(terra::values(result))]
  expect_true(all(abs(non_na - expected) < 1e-6))
})

# verbose output

test_that("verbose = TRUE produces console messages", {
  r <- make_ndvi(0.3)
  expect_message(
    calc_c_factor(r, verbose = TRUE, plot = FALSE),
    "C-factor computed"
  )
})

test_that("verbose = TRUE prints formula message", {
  r <- make_ndvi(0.3)
  expect_message(
    calc_c_factor(r, verbose = TRUE, plot = FALSE),
    "Mahgoub"
  )
})

test_that("verbose = FALSE produces no messages", {
  r <- make_ndvi(0.3)
  expect_no_message(
    calc_c_factor(r, verbose = FALSE, plot = FALSE)
  )
})

# plot parameter

test_that("plot = FALSE runs without error", {
  r <- make_ndvi(0.3)
  expect_no_error(
    calc_c_factor(r, verbose = FALSE, plot = FALSE)
  )
})

test_that("plot = TRUE runs without error", {
  r <- make_ndvi(0.3)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(
    calc_c_factor(r, verbose = FALSE, plot = TRUE)
  )
})
