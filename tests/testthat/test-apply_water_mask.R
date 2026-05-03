make_raster <- function(values, nrow = 3, ncol = 3) {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- values
  return(r)
}

# Builds a minimal 2-band satellite raster (band 1 = green, band 2 = NIR)
# by default, so green_band = 1, nir_band = 2 must be passed in tests.
make_satellite <- function(green_vals, nir_vals, nrow = 3, ncol = 3) {
  green <- make_raster(green_vals, nrow, ncol)
  nir   <- make_raster(nir_vals,   nrow, ncol)
  s2    <- c(green, nir)
  names(s2) <- c("B1_green", "B2_nir")
  return(s2)
}



# Input validation

test_that("non-SpatRaster target_raster throws error", {
  s2 <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(matrix(1:9, 3, 3), s2,
                                green_band = 1, nir_band = 2),
               "'target_raster' must be a SpatRaster")
})

test_that("non-SpatRaster satellite_raster throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  expect_error(apply_water_mask(ndvi, matrix(1:9, 3, 3),
                                green_band = 1, nir_band = 2),
               "'satellite_raster' must be a SpatRaster")
})

test_that("green_band out of range throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 5, nir_band = 2),
               "'green_band' must be a single integer between 1 and")
})

test_that("nir_band out of range throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 99),
               "'nir_band' must be a single integer between 1 and")
})

test_that("green_band equal to nir_band throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 1),
               "'green_band' and 'nir_band' must refer to different bands")
})

test_that("non-numeric threshold throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                threshold = "high"),
               "'threshold' must be a single numeric value")
})

test_that("vector threshold throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                threshold = c(0.2, 0.4)),
               "'threshold' must be a single numeric value")
})

test_that("threshold below -1 throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                threshold = -1.1),
               "'threshold' must be between -1 and 1")
})

test_that("threshold above 1 throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                threshold = 1.1),
               "'threshold' must be between -1 and 1")
})

test_that("non-logical return_ndwi throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                return_ndwi = "yes"),
               "'return_ndwi' must be a single logical value")
})

test_that("non-logical plot argument throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                plot = "yes"),
               "'plot' must be a single logical value")
})

test_that("vector plot argument throws error", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                plot = c(TRUE, FALSE)),
               "'plot' must be a single logical value")
})



# Return type and structure

test_that("result is a SpatRaster", {
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("result has same dimensions as target_raster", {
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2)
  expect_equal(terra::nrow(result), terra::nrow(ndvi))
  expect_equal(terra::ncol(result), terra::ncol(ndvi))
  expect_equal(terra::nlyr(result), terra::nlyr(ndvi))
})

test_that("result preserves layer names from target_raster", {
  ndvi <- make_raster(rep(0.5, 9))
  names(ndvi) <- "NDVI"
  s2     <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2)
  expect_equal(names(result), "NDVI")
})

test_that("multi-layer target_raster: all layers masked and names preserved", {
  ndvi1 <- make_raster(rep(0.5, 9))
  ndvi2 <- make_raster(rep(0.3, 9))
  ndvi  <- c(ndvi1, ndvi2)
  names(ndvi) <- c("NDVI_Jan", "NDVI_Feb")
  s2     <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2)
  expect_equal(terra::nlyr(result), 2L)
  expect_equal(names(result), c("NDVI_Jan", "NDVI_Feb"))
})

test_that("result is returned invisibly", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  out  <- withVisible(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2))
  expect_false(out$visible)
})

test_that("result is returned invisibly also when plot = TRUE", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  out  <- withVisible(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                       plot = TRUE))
  expect_false(out$visible)
})



# NDWI computation

test_that("NDWI is computed correctly from green and NIR bands", {
  # green = 0.2, nir = 0.1 -> NDWI = (0.2 - 0.1) / (0.2 + 0.1) = 0.333...
  # above threshold 0.3 -> all pixels masked
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(rep(0.2, 9), rep(0.1, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             threshold = 0.3)
  expect_true(all(is.na(as.vector(terra::values(result)))))
})

test_that("NDWI below threshold leaves pixels unmasked", {
  # green = 0.1, nir = 0.2 -> NDWI = (0.1 - 0.2) / (0.1 + 0.2) = -0.333...
  # below threshold 0.3 -> no pixels masked
  ndvi   <- make_raster(rep(0.7, 9))
  s2     <- make_satellite(rep(0.1, 9), rep(0.2, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             threshold = 0.3)
  expect_equal(as.vector(terra::values(result)), rep(0.7, 9))
})

test_that("NDWI masking logic is correct for mixed pixels", {
  # Pixel-wise green and NIR values -> known NDWI per pixel
  # pixel 1: green=0.4, nir=0.1 -> NDWI = 0.60  (>= 0.3: water)
  # pixel 2: green=0.1, nir=0.4 -> NDWI = -0.60 (<  0.3: land)
  # pixel 3: green=0.3, nir=0.3 -> NDWI = 0.00  (<  0.3: land)
  # pixel 4: green=0.2, nir=0.1 -> NDWI = 0.33  (>= 0.3: water)
  green_vals <- c(0.4, 0.1, 0.3, 0.2, rep(0.1, 5))
  nir_vals   <- c(0.1, 0.4, 0.3, 0.1, rep(0.4, 5))
  ndvi       <- make_raster(rep(0.6, 9))
  s2         <- make_satellite(green_vals, nir_vals)
  result     <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                 threshold = 0.3)
  vals <- as.vector(terra::values(result))
  expect_true(is.na(vals[1]))   # water
  expect_equal(vals[2], 0.6)    # land
  expect_equal(vals[3], 0.6)    # land
  expect_true(is.na(vals[4]))   # water
})



# Masking logic

test_that("all land pixels are unchanged", {
  # All NDWI < 0.3 (nir > green)
  ndvi   <- make_raster(rep(0.7, 9))
  s2     <- make_satellite(rep(0.1, 9), rep(0.5, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             threshold = 0.3)
  expect_equal(as.vector(terra::values(result)), rep(0.7, 9))
})

test_that("all water pixels are NA", {
  # All NDWI > 0.3 (green >> nir)
  ndvi   <- make_raster(rep(0.7, 9))
  s2     <- make_satellite(rep(0.9, 9), rep(0.1, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             threshold = 0.3)
  expect_true(all(is.na(as.vector(terra::values(result)))))
})

test_that("existing NA pixels in target_raster remain NA after masking", {
  ndvi_vals <- c(0.5, NA,  0.4,
                 0.3, 0.6, NA,
                 0.2, 0.5, 0.4)
  ndvi   <- make_raster(ndvi_vals)
  s2     <- make_satellite(rep(0.1, 9), rep(0.5, 9))  # all land
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             threshold = 0.3)
  vals <- as.vector(terra::values(result))
  expect_true(is.na(vals[2]))
  expect_true(is.na(vals[6]))
})

test_that("custom threshold is respected", {
  # NDWI = (green - nir) / (green + nir)
  # Use green=0.0, nir=1.0 -> NDWI = -1.0 (land even at threshold = 0.0)
  # Use green=1.0, nir=0.0 -> NDWI =  1.0 (water at any threshold <= 1.0)
  green_vals <- c(0.0, 1.0, 0.0,
                  1.0, 0.0, 1.0,
                  0.0, 1.0, 0.0)
  nir_vals   <- c(1.0, 0.0, 1.0,
                  0.0, 1.0, 0.0,
                  1.0, 0.0, 1.0)
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(green_vals, nir_vals)
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             threshold = 0.0)
  vals <- as.vector(terra::values(result))
  # Even pixels (green=1, nir=0): NDWI=1 >= 0.0 -> water (NA)
  expect_true(is.na(vals[2]))
  expect_true(is.na(vals[4]))
  expect_true(is.na(vals[6]))
  expect_true(is.na(vals[8]))
  # Odd pixels (green=0, nir=1): NDWI=-1 < 0.0 -> land
  expect_equal(vals[1], 0.5)
  expect_equal(vals[3], 0.5)
  expect_equal(vals[5], 0.5)
  expect_equal(vals[7], 0.5)
  expect_equal(vals[9], 0.5)
})



# return_ndwi attribute

test_that("return_ndwi = FALSE: no ndwi attribute on result", {
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             return_ndwi = FALSE)
  expect_null(attr(result, "ndwi"))
})

test_that("return_ndwi = TRUE: ndwi attribute is a SpatRaster", {
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             return_ndwi = TRUE)
  expect_true(inherits(attr(result, "ndwi"), "SpatRaster"))
})

test_that("return_ndwi = TRUE: ndwi attribute has same dimensions as target_raster", {
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             return_ndwi = TRUE)
  ndwi_out <- attr(result, "ndwi")
  expect_equal(terra::nrow(ndwi_out), terra::nrow(ndvi))
  expect_equal(terra::ncol(ndwi_out), terra::ncol(ndvi))
  expect_equal(terra::nlyr(ndwi_out), 1L)
})

test_that("return_ndwi = TRUE: stored NDWI values are correct", {
  # green = 0.4, nir = 0.2 -> NDWI = (0.4-0.2)/(0.4+0.2) = 0.333...
  ndvi   <- make_raster(rep(0.5, 9))
  s2     <- make_satellite(rep(0.4, 9), rep(0.2, 9))
  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             return_ndwi = TRUE)
  ndwi_vals <- as.vector(terra::values(attr(result, "ndwi")))
  expect_equal(ndwi_vals, rep((0.4 - 0.2) / (0.4 + 0.2), 9),
               tolerance = 1e-6)
})



# Geometry alignment (resample)

test_that("satellite_raster with different resolution is resampled without error", {
  ndvi <- terra::rast(nrows = 4, ncols = 4,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndvi) <- rep(0.5, 16)

  green <- terra::rast(nrows = 2, ncols = 2,
                       xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  nir   <- terra::rast(nrows = 2, ncols = 2,
                       xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(green) <- rep(0.1, 4)
  terra::values(nir)   <- rep(0.5, 4)
  s2 <- c(green, nir)

  expect_no_error(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                   threshold = 0.3))
})

test_that("resampled result has same dimensions as target_raster", {
  ndvi <- terra::rast(nrows = 4, ncols = 4,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndvi) <- rep(0.5, 16)

  green <- terra::rast(nrows = 2, ncols = 2,
                       xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  nir   <- terra::rast(nrows = 2, ncols = 2,
                       xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(green) <- rep(0.1, 4)
  terra::values(nir)   <- rep(0.5, 4)
  s2 <- c(green, nir)

  result <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                             threshold = 0.3)
  expect_equal(terra::nrow(result), 4L)
  expect_equal(terra::ncol(result), 4L)
})

test_that("geometry mismatch triggers a message about resampling", {
  ndvi <- terra::rast(nrows = 4, ncols = 4,
                      xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(ndvi) <- rep(0.5, 16)

  green <- terra::rast(nrows = 2, ncols = 2,
                       xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  nir   <- terra::rast(nrows = 2, ncols = 2,
                       xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(green) <- rep(0.1, 4)
  terra::values(nir)   <- rep(0.5, 4)
  s2 <- c(green, nir)

  expect_message(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2),
                 "resampling")
})



# plot parameter

test_that("plot = FALSE still emits a message about band indices", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  expect_message(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                  plot = FALSE),
                 "NDWI computed from bands")
})

test_that("plot = TRUE runs without error or warning", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                     plot = TRUE))
})

test_that("plot = TRUE emits a message about band indices", {
  ndvi <- make_raster(rep(0.5, 9))
  s2   <- make_satellite(rep(0.1, 9), rep(0.05, 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_message(apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                  plot = TRUE),
                 "NDWI computed from bands")
})

test_that("plot = TRUE still returns correct masked values", {
  # pixel 1: green=0.4, nir=0.1 -> NDWI=0.60 >= 0.3 -> water (NA)
  # pixel 2: green=0.1, nir=0.4 -> NDWI=-0.60 < 0.3 -> land (0.6)
  green_vals <- c(0.4, 0.1, rep(0.1, 7))
  nir_vals   <- c(0.1, 0.4, rep(0.4, 7))
  ndvi       <- make_raster(rep(0.6, 9))
  s2         <- make_satellite(green_vals, nir_vals)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result     <- apply_water_mask(ndvi, s2, green_band = 1, nir_band = 2,
                                 threshold = 0.3, plot = TRUE)
  vals <- as.vector(terra::values(result))
  expect_true(is.na(vals[1]))
  expect_equal(vals[2], 0.6)
})
