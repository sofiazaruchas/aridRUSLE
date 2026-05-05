make_raster <- function(values, nrow = 3, ncol = 3) {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- values
  return(r)
}



# Input validation

test_that("non-SpatRaster r_factor throws error", {
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(matrix(1:9, 3, 3), ls, c),
               "'r_factor' must be a SpatRaster")
})

test_that("non-SpatRaster ls_factor throws error", {
  r <- make_raster(rep(1, 9))
  c <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(r, matrix(1:9, 3, 3), c),
               "'ls_factor' must be a SpatRaster")
})

test_that("non-SpatRaster c_factor throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  expect_error(calc_erosion_risk(r, ls, matrix(1:9, 3, 3)),
               "'c_factor' must be a SpatRaster")
})

test_that("non-logical normalize throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(r, ls, c, normalize = "yes"),
               "'normalize' must be a single logical value")
})

test_that("non-logical plot throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(r, ls, c, plot = "yes"),
               "'plot' must be a single logical value")
})

test_that("geometry mismatch throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- terra::rast(nrows = 4, ncols = 4,
                    xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ls) <- rep(1, 16)
  c  <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(r, ls, c),
               "must share the same")
})



# Return type and structure

test_that("result is a SpatRaster", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("result has one layer named ERI", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c)
  expect_equal(terra::nlyr(result), 1L)
  expect_equal(names(result), "ERI")
})

test_that("result has same dimensions as input rasters", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c)
  expect_equal(terra::nrow(result), terra::nrow(r))
  expect_equal(terra::ncol(result), terra::ncol(r))
})

test_that("result is returned invisibly", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  out <- withVisible(calc_erosion_risk(r, ls, c))
  expect_false(out$visible)
})

test_that("result is returned invisibly also when plot = TRUE", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  out <- withVisible(calc_erosion_risk(r, ls, c, plot = TRUE))
  expect_false(out$visible)
})



# ERI values

test_that("ERI values are in range 0-1 with normalize = TRUE", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE)
  vals <- as.vector(terra::values(result))
  expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE))
})

test_that("ERI is zero when any normalised factor is zero", {
  # r has constant value -> normalised to 0 -> ERI = 0 everywhere
  r  <- make_raster(rep(5, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE)
  vals <- as.vector(terra::values(result))
  expect_true(all(vals == 0, na.rm = TRUE))
})

test_that("normalize = FALSE: ERI equals direct product of inputs", {
  r_vals  <- seq(0.1, 0.9, length.out = 9)
  ls_vals <- seq(0.2, 1.0, length.out = 9)
  c_vals  <- seq(0.1, 0.5, length.out = 9)
  r  <- make_raster(r_vals)
  ls <- make_raster(ls_vals)
  c  <- make_raster(c_vals)
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_equal(vals, r_vals * ls_vals * c_vals, tolerance = 1e-6)
})

test_that("NA pixels in any factor propagate to ERI", {
  r_vals        <- rep(0.5, 9)
  r_vals[1]     <- NA
  ls_vals       <- rep(0.5, 9)
  ls_vals[5]    <- NA
  r  <- make_raster(r_vals)
  ls <- make_raster(ls_vals)
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE)
  vals <- as.vector(terra::values(result))
  expect_true(is.na(vals[1]))
  expect_true(is.na(vals[5]))
})



# normalize behaviour

test_that("normalize = TRUE: max ERI value is 1", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(1, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE)
  max_val <- terra::global(result, "max", na.rm = TRUE)[[1]]
  expect_equal(max_val, 1, tolerance = 1e-6)
})

test_that("normalize = TRUE: min ERI value is 0", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(1, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE)
  min_val <- terra::global(result, "min", na.rm = TRUE)[[1]]
  expect_equal(min_val, 0, tolerance = 1e-6)
})



# message

test_that("calc_erosion_risk emits a message about ERI computation", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_message(calc_erosion_risk(r, ls, c, plot = FALSE),
                 "ERI computed")
})



# plot parameter

test_that("plot = TRUE runs without error or warning", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(calc_erosion_risk(r, ls, c, plot = TRUE))
})

test_that("plot = TRUE still returns correct ERI values", {
  r_vals  <- seq(0.1, 0.9, length.out = 9)
  ls_vals <- seq(0.2, 1.0, length.out = 9)
  c_vals  <- seq(0.1, 0.5, length.out = 9)
  r  <- make_raster(r_vals)
  ls <- make_raster(ls_vals)
  c  <- make_raster(c_vals)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = TRUE)
  vals   <- as.vector(terra::values(result))
  expect_equal(vals, r_vals * ls_vals * c_vals, tolerance = 1e-6)
})

test_that("plot = FALSE still emits message", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_message(calc_erosion_risk(r, ls, c, plot = FALSE),
                 "ERI computed")
})
