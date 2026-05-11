make_eri <- function(values, nrow = 3, ncol = 3,
                     xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                     crs = "EPSG:4326") {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = xmin, xmax = xmax,
                   ymin = ymin, ymax = ymax,
                   crs  = crs)
  terra::values(r) <- values
  names(r)         <- "ERI"
  return(r)
}



# Input validation

test_that("non-SpatRaster eri throws error", {
  expect_error(classify_erosion_risk(matrix(1:9, 3, 3)),
               "'eri' must be a SpatRaster")
})

test_that("multi-layer SpatRaster throws error", {
  r <- c(make_eri(rep(0.5, 9)), make_eri(rep(0.5, 9)))
  expect_error(classify_erosion_risk(r),
               "'eri' must be a single-layer SpatRaster")
})

test_that("non-logical plot throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(classify_erosion_risk(eri, plot = "yes"),
               "'plot' must be a single logical value")
})

test_that("ERI values outside 0-1 emit a warning", {
  eri <- make_eri(seq(-0.1, 0.9, length.out = 9))
  expect_warning(classify_erosion_risk(eri, plot = FALSE),
                 "ERI values outside \\[0, 1\\]")
})

test_that("ERI values inside 0-1 do not emit a warning", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_no_warning(classify_erosion_risk(eri, plot = FALSE))
})



# Return type and structure

test_that("result is a SpatRaster", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("result has one layer named ERI_class", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(terra::nlyr(result), 1L)
  expect_equal(names(result), "ERI_class")
})

test_that("result has same dimensions as input", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(terra::nrow(result), terra::nrow(eri))
  expect_equal(terra::ncol(result), terra::ncol(eri))
})

test_that("result is returned invisibly", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  out <- withVisible(classify_erosion_risk(eri, plot = FALSE))
  expect_false(out$visible)
})

test_that("result is returned invisibly also when plot = TRUE", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  out <- withVisible(classify_erosion_risk(eri, plot = TRUE))
  expect_false(out$visible)
})



# Classification values

test_that("values in 0-0.33 are classified as 1 (Low)", {
  eri    <- make_eri(c(0.0, 0.1, 0.2, 0.33), nrow = 2, ncol = 2)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals == 1, na.rm = TRUE))
})

test_that("values in 0.33-0.66 are classified as 2 (Medium)", {
  eri    <- make_eri(c(0.34, 0.45, 0.55, 0.66), nrow = 2, ncol = 2)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals == 2, na.rm = TRUE))
})

test_that("values in 0.66-1 are classified as 3 (High)", {
  eri    <- make_eri(c(0.67, 0.75, 0.9, 1.0), nrow = 2, ncol = 2)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals == 3, na.rm = TRUE))
})

test_that("result contains only class values 1, 2 and 3", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals %in% c(1, 2, 3), na.rm = TRUE))
})

test_that("NA pixels in ERI are preserved as NA in classification", {
  eri_vals    <- seq(0, 1, length.out = 9)
  eri_vals[1] <- NA
  eri_vals[5] <- NA
  eri    <- make_eri(eri_vals)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(is.na(vals[1]))
  expect_true(is.na(vals[5]))
})

test_that("boundary value 0.33 is classified as 1 (Low, include.lowest)", {
  eri    <- make_eri(rep(0.33, 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals == 1, na.rm = TRUE))
})

test_that("boundary value 0.66 is classified as 2 (Medium)", {
  eri    <- make_eri(rep(0.66, 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals == 2, na.rm = TRUE))
})

test_that("boundary value 1.0 is classified as 3 (High)", {
  eri    <- make_eri(rep(1.0, 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals == 3, na.rm = TRUE))
})

test_that("all-NA ERI returns all-NA classification without error", {
  eri_vals <- rep(NA_real_, 9)
  eri      <- make_eri(eri_vals)
  expect_no_error(classify_erosion_risk(eri, plot = FALSE))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(is.na(vals)))
})



# message

test_that("classify_erosion_risk emits a classification message", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_message(classify_erosion_risk(eri, plot = FALSE), "ERI classified")
})



# plot parameter

test_that("plot = TRUE runs without error or warning", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(classify_erosion_risk(eri, plot = TRUE))
})

test_that("plot = TRUE still returns correct classification values", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- classify_erosion_risk(eri, plot = TRUE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(vals %in% c(1, 2, 3), na.rm = TRUE))
})

test_that("plot = FALSE still emits classification message", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_message(classify_erosion_risk(eri, plot = FALSE), "ERI classified")
})

test_that("plot = TRUE with NA pixels runs without error or warning", {
  eri_vals    <- seq(0, 1, length.out = 9)
  eri_vals[1] <- NA
  eri <- make_eri(eri_vals)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(classify_erosion_risk(eri, plot = TRUE))
})
