make_raster <- function(values, nrow = 3, ncol = 3) {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- values
  return(r)
}



# Input validation
test_that("non-SpatRaster target_raster throws error", {
  ndwi <- make_raster(rep(0.1, 9))
  expect_error(apply_water_mask(matrix(1:9, 3, 3), ndwi),
               "'target_raster' must be a SpatRaster")
})

test_that("non-SpatRaster ndwi_raster throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  expect_error(apply_water_mask(ndvi, matrix(1:9, 3, 3)),
               "'ndwi_raster' must be a SpatRaster")
})

test_that("non-numeric threshold throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  ndwi <- make_raster(rep(0.1, 9))
  expect_error(apply_water_mask(ndvi, ndwi, threshold = "high"),
               "'threshold' must be a single numeric value")
})

test_that("vector threshold throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  ndwi <- make_raster(rep(0.1, 9))
  expect_error(apply_water_mask(ndvi, ndwi, threshold = c(0.2, 0.4)),
               "'threshold' must be a single numeric value")
})

test_that("threshold below -1 throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  ndwi <- make_raster(rep(0.1, 9))
  expect_error(apply_water_mask(ndvi, ndwi, threshold = -1.1),
               "'threshold' must be between -1 and 1")
})

test_that("threshold above 1 throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  ndwi <- make_raster(rep(0.1, 9))
  expect_error(apply_water_mask(ndvi, ndwi, threshold = 1.1),
               "'threshold' must be between -1 and 1")
})

test_that("multi-layer ndwi_raster throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  ndwi <- c(make_raster(rep(0.1, 9)), make_raster(rep(0.2, 9)))
  expect_error(apply_water_mask(ndvi, ndwi),
               "'ndwi_raster' must be a single-layer SpatRaster")
})



# 2  Return type and structure
test_that("result is a SpatRaster", {
  ndvi   <- make_raster(rep(0.5, 9))
  ndwi   <- make_raster(rep(0.1, 9))
  result <- apply_water_mask(ndvi, ndwi)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("result has same dimensions as target_raster", {
  ndvi   <- make_raster(rep(0.5, 9))
  ndwi   <- make_raster(rep(0.1, 9))
  result <- apply_water_mask(ndvi, ndwi)
  expect_equal(terra::nrow(result),  terra::nrow(ndvi))
  expect_equal(terra::ncol(result),  terra::ncol(ndvi))
  expect_equal(terra::nlyr(result),  terra::nlyr(ndvi))
})

test_that("result preserves layer names from target_raster", {
  ndvi <- make_raster(rep(0.5, 9))
  names(ndvi) <- "NDVI"
  ndwi   <- make_raster(rep(0.1, 9))
  result <- apply_water_mask(ndvi, ndwi)
  expect_equal(names(result), "NDVI")
})

test_that("multi-layer target_raster: all layers masked and names preserved", {
  ndvi1 <- make_raster(rep(0.5, 9))
  ndvi2 <- make_raster(rep(0.3, 9))
  ndvi  <- c(ndvi1, ndvi2)
  names(ndvi) <- c("NDVI_Jan", "NDVI_Feb")
  ndwi   <- make_raster(rep(0.1, 9))
  result <- apply_water_mask(ndvi, ndwi)
  expect_equal(terra::nlyr(result), 2L)
  expect_equal(names(result), c("NDVI_Jan", "NDVI_Feb"))
})



# 3  Masking logic
test_that("pixels with NDWI >= threshold are set to NA", {
  # 5 water pixels (>= 0.3), 4 land pixels (< 0.3)
  ndwi_vals <- c(0.5, 0.3, 0.1,
                 0.4, 0.2, 0.0,
                 0.6, 0.1, 0.3)
  ndvi_vals <- rep(0.6, 9)

  ndvi   <- make_raster(ndvi_vals)
  ndwi   <- make_raster(ndwi_vals)
  result <- apply_water_mask(ndvi, ndwi, threshold = 0.3)
  vals   <- as.vector(terra::values(result))

  # pixels 1, 2, 4, 7, 9 have NDWI >= 0.3 -> NA
  expect_true(is.na(vals[1]))
  expect_true(is.na(vals[2]))
  expect_true(is.na(vals[4]))
  expect_true(is.na(vals[7]))
  expect_true(is.na(vals[9]))

  # pixels 3, 5, 6, 8 have NDWI < 0.3 -> unchanged
  expect_equal(vals[3], 0.6)
  expect_equal(vals[5], 0.6)
  expect_equal(vals[6], 0.6)
  expect_equal(vals[8], 0.6)
})

test_that("all land pixels (NDWI < threshold) are unchanged", {
  ndvi   <- make_raster(rep(0.7, 9))
  ndwi   <- make_raster(rep(0.1, 9))   # all below 0.3
  result <- apply_water_mask(ndvi, ndwi, threshold = 0.3)
  expect_equal(as.vector(terra::values(result)), rep(0.7, 9))
})

test_that("all water pixels (NDWI >= threshold) are NA", {
  ndvi   <- make_raster(rep(0.7, 9))
  ndwi   <- make_raster(rep(0.5, 9))   # all above 0.3
  result <- apply_water_mask(ndvi, ndwi, threshold = 0.3)
  expect_true(all(is.na(as.vector(terra::values(result)))))
})

test_that("existing NA pixels in target_raster remain NA after masking", {
  ndvi_vals <- c(0.5, NA,  0.4,
                 0.3, 0.6, NA,
                 0.2, 0.5, 0.4)
  ndvi   <- make_raster(ndvi_vals)
  ndwi   <- make_raster(rep(0.1, 9))   # no water
  result <- apply_water_mask(ndvi, ndwi, threshold = 0.3)
  vals   <- as.vector(terra::values(result))
  expect_true(is.na(vals[2]))
  expect_true(is.na(vals[6]))
})

test_that("custom threshold is respected", {
  # threshold = 0.0: all pixels with NDWI >= 0 are water
  ndwi_vals <- c(-0.1,  0.0,  0.1,
                 -0.2,  0.2,  0.3,
                 -0.3,  0.0,  0.4)
  ndvi   <- make_raster(rep(0.5, 9))
  ndwi   <- make_raster(ndwi_vals)
  result <- apply_water_mask(ndvi, ndwi, threshold = 0.0)
  vals   <- as.vector(terra::values(result))

  # NDWI >= 0.0: pixels 2, 3, 5, 6, 8, 9 -> NA
  expect_true(is.na(vals[2]))
  expect_true(is.na(vals[3]))
  expect_true(is.na(vals[5]))
  expect_true(is.na(vals[6]))
  expect_true(is.na(vals[8]))
  expect_true(is.na(vals[9]))

  # NDWI < 0.0: pixels 1, 4, 7 -> unchanged
  expect_equal(vals[1], 0.5)
  expect_equal(vals[4], 0.5)
  expect_equal(vals[7], 0.5)
})



# 4  Geometry alignment (resample)
test_that("ndwi_raster with different resolution is resampled without error", {
  ndvi <- terra::rast(nrows = 4, ncols = 4,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndvi) <- rep(0.5, 16)

  ndwi <- terra::rast(nrows = 2, ncols = 2,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndwi) <- rep(0.1, 4)

  expect_no_error(apply_water_mask(ndvi, ndwi, threshold = 0.3))
})

test_that("resampled result has same dimensions as target_raster", {
  ndvi <- terra::rast(nrows = 4, ncols = 4,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndvi) <- rep(0.5, 16)

  ndwi <- terra::rast(nrows = 2, ncols = 2,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndwi) <- rep(0.1, 4)

  result <- apply_water_mask(ndvi, ndwi, threshold = 0.3)
  expect_equal(terra::nrow(result), 4L)
  expect_equal(terra::ncol(result), 4L)
})

test_that("geometry mismatch triggers a message about resampling", {
  ndvi <- terra::rast(nrows = 4, ncols = 4,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndvi) <- rep(0.5, 16)

  ndwi <- terra::rast(nrows = 2, ncols = 2,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndwi) <- rep(0.1, 4)

  expect_message(apply_water_mask(ndvi, ndwi), "resampling")
})
