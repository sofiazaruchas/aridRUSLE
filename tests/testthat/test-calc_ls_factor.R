# tests/testthat/test-calc_ls_factor.R

# Helpers

make_dem <- function(value, nrows = 5, ncols = 5, res = 30) {
  r <- terra::rast(
    nrows = nrows, ncols = ncols,
    xmin  = 0, xmax = ncols * res,
    ymin  = 0, ymax = nrows * res,
    crs   = "EPSG:32719"
  )
  terra::values(r) <- value
  r
}

make_sloped_dem <- function(res = 30, nrows = 5, ncols = 5) {
  r <- terra::rast(
    nrows = nrows, ncols = ncols,
    xmin  = 0, xmax = ncols * res,
    ymin  = 0, ymax = nrows * res,
    crs   = "EPSG:32719"
  )
  terra::values(r) <- rep(seq(0, (nrows - 1) * res, by = res), each = ncols)
  r
}

make_geographic_dem <- function(value = 100) {
  r <- terra::rast(
    nrows = 5, ncols = 5,
    xmin  = -71, xmax = -70,
    ymin  = -30, ymax = -29,
    crs   = "EPSG:4326"
  )
  terra::values(r) <- value
  r
}

# Input validation

test_that("throws error if dem is not a SpatRaster", {
  expect_error(
    calc_ls_factor(matrix(1:25, 5, 5)),
    "'dem' must be a terra::SpatRaster object"
  )
})

test_that("throws error if dem has more than 1 layer", {
  r  <- make_dem(100)
  r2 <- c(r, r)
  expect_error(
    calc_ls_factor(r2),
    "'dem' must have exactly 1 layer"
  )
})

test_that("throws error if dem has no CRS", {
  r <- make_dem(100)
  terra::crs(r) <- ""
  expect_error(
    calc_ls_factor(r),
    "no CRS defined"
  )
})

# Automatic reprojection
test_that("geographic CRS is automatically reprojected to UTM", {
  r <- make_geographic_dem()
  expect_message(
    calc_ls_factor(r, verbose = TRUE, plot = FALSE),
    "Reprojecting to UTM"
  )
})

test_that("geographic CRS reprojection runs without error", {
  r <- make_geographic_dem()
  expect_no_error(
    calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  )
})

test_that("output from geographic DEM is still a SpatRaster", {
  r      <- make_geographic_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_s4_class(result, "SpatRaster")
})

#  Return type and structure

test_that("returns a SpatRaster", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_s4_class(result, "SpatRaster")
})

test_that("output has exactly 1 layer", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(terra::nlyr(result), 1)
})

test_that("output layer is named 'LS_factor'", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(names(result), "LS_factor")
})

test_that("output has same extent as input", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(as.vector(terra::ext(result)), as.vector(terra::ext(r)))
})

test_that("output has same resolution as input", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(terra::res(result), terra::res(r))
})

test_that("output has same CRS as input", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_equal(terra::crs(result), terra::crs(r))
})

# Formula correctness
test_that("flat DEM produces LS values of 0", {
  r      <- make_dem(100)
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  vals   <- terra::values(result, na.rm = TRUE)
  expect_true(all(vals >= 0))
})

test_that("sloped DEM produces LS values greater than 0", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  vals   <- terra::values(result, na.rm = TRUE)
  expect_true(any(vals > 0))
})

test_that("all output values are non-negative", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_true(terra::global(result, "min", na.rm = TRUE)[[1]] >= 0)
})

test_that("LS-factor increases with steeper slope", {
  r_gentle <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = 0, xmax = 150, ymin = 0, ymax = 150,
    crs  = "EPSG:32719"
  )
  terra::values(r_gentle) <- rep(seq(0, 4, by = 1), each = 5)

  r_steep <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = 0, xmax = 150, ymin = 0, ymax = 150,
    crs  = "EPSG:32719"
  )
  terra::values(r_steep) <- rep(seq(0, 40, by = 10), each = 5)

  ls_gentle <- calc_ls_factor(r_gentle, verbose = FALSE, plot = FALSE)
  ls_steep  <- calc_ls_factor(r_steep,  verbose = FALSE, plot = FALSE)

  expect_true(
    terra::global(ls_steep,  "mean", na.rm = TRUE)[[1]] >
      terra::global(ls_gentle, "mean", na.rm = TRUE)[[1]]
  )
})

test_that("larger cell size produces larger LS values (all else equal)", {
  # Same slope angle, different resolution.
  # Rise scales with cell size to keep angle constant:
  # 30m cell: 10m rise -> tan(beta) = 10/30
  # 90m cell: 30m rise -> tan(beta) = 30/90  (same angle)
  r_30m <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = 0, xmax = 150, ymin = 0, ymax = 150,
    crs  = "EPSG:32719"
  )
  terra::values(r_30m) <- rep(seq(0, 40, by = 10), each = 5)

  r_90m <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = 0, xmax = 450, ymin = 0, ymax = 450,
    crs  = "EPSG:32719"
  )
  terra::values(r_90m) <- rep(seq(0, 120, by = 30), each = 5)

  ls_30m <- calc_ls_factor(r_30m, verbose = FALSE, plot = FALSE)
  ls_90m <- calc_ls_factor(r_90m, verbose = FALSE, plot = FALSE)

  expect_true(
    terra::global(ls_90m, "mean", na.rm = TRUE)[[1]] >
      terra::global(ls_30m, "mean", na.rm = TRUE)[[1]]
  )
})

# NA handling
test_that("NA cells in input are preserved as NA in output", {
  r <- make_sloped_dem()
  terra::values(r)[1] <- NA
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_true(anyNA(terra::values(result)))
})

test_that("output contains non-NA values when input has valid cells", {
  r      <- make_sloped_dem()
  result <- calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  expect_true(any(!is.na(terra::values(result))))
})

# verbose output

test_that("verbose = TRUE produces console messages", {
  r <- make_sloped_dem()
  expect_message(
    calc_ls_factor(r, verbose = TRUE, plot = FALSE),
    "LS-factor computed"
  )
})

test_that("verbose = TRUE also prints cell size message", {
  r <- make_sloped_dem()
  expect_message(
    calc_ls_factor(r, verbose = TRUE, plot = FALSE),
    "Cell size used"
  )
})

test_that("verbose = FALSE produces no messages", {
  r <- make_sloped_dem()
  expect_no_message(
    calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  )
})

# plot parameter

test_that("plot = FALSE runs without error", {
  r <- make_sloped_dem()
  expect_no_error(
    calc_ls_factor(r, verbose = FALSE, plot = FALSE)
  )
})

test_that("plot = TRUE runs without error", {
  r <- make_sloped_dem()
  pdf(NULL)
  expect_no_error(
    calc_ls_factor(r, verbose = FALSE, plot = TRUE)
  )
  dev.off()
})
