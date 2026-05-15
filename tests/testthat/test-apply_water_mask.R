make_raster <- function(values, nrow = 3, ncol = 3,
                        xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                        crs = "EPSG:4326") {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = xmin, xmax = xmax,
                   ymin = ymin, ymax = ymax,
                   crs  = crs)
  terra::values(r) <- values
  return(r)
}

make_satelite <- function(n_bands = 8, nrow = 3, ncol = 3,
                          xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                          crs = "EPSG:4326") {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = xmin, xmax = xmax,
                   ymin = ymin, ymax = ymax,
                   crs  = crs,
                   nlyr = n_bands)
  terra::values(r) <- matrix(rep(0.5, nrow * ncol * n_bands),
                             nrow = nrow * ncol, ncol = n_bands)
  terra::values(r[[3]]) <- rep(0.3, nrow * ncol)
  terra::values(r[[8]]) <- rep(0.6, nrow * ncol)
  return(r)
}

make_water_satelite <- function(n_bands = 8, nrow = 3, ncol = 3,
                                xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                                crs = "EPSG:4326") {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = xmin, xmax = xmax,
                   ymin = ymin, ymax = ymax,
                   crs  = crs,
                   nlyr = n_bands)
  terra::values(r) <- matrix(rep(0.5, nrow * ncol * n_bands),
                             nrow = nrow * ncol, ncol = n_bands)
  terra::values(r[[3]]) <- rep(0.8, nrow * ncol)
  terra::values(r[[8]]) <- rep(0.1, nrow * ncol)
  return(r)
}



# Input validation

test_that("non-SpatRaster target_raster throws error", {
  s <- make_satelite()
  expect_error(apply_water_mask(matrix(1:9, 3, 3), s),
               "'target_raster' must be a SpatRaster")
})

test_that("non-SpatRaster satelite_raster throws error", {
  t <- make_raster(rep(0.5, 9))
  expect_error(apply_water_mask(t, matrix(1:9, 3, 3)),
               "'satelite_raster' must be a SpatRaster")
})

test_that("green_band out of range throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite(n_bands = 8)
  expect_error(apply_water_mask(t, s, green_band = 9),
               "'green_band' must be a single integer between 1 and")
})

test_that("nir_band out of range throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite(n_bands = 8)
  expect_error(apply_water_mask(t, s, nir_band = 9),
               "'nir_band' must be a single integer between 1 and")
})

test_that("green_band == nir_band throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite(n_bands = 8)
  expect_error(apply_water_mask(t, s, green_band = 3, nir_band = 3),
               "'green_band' and 'nir_band' must refer to different bands")
})

test_that("non-numeric threshold throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, threshold = "0.3"),
               "'threshold' must be a single numeric value")
})

test_that("threshold outside -1 to 1 throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, threshold = 1.5),
               "'threshold' must be between -1 and 1")
})

test_that("non-logical smooth throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, smooth = "yes"),
               "'smooth' must be a single logical value")
})

test_that("even smooth_w throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, smooth_w = 4),
               "'smooth_w' must be a single odd integer >= 3")
})

test_that("smooth_w < 3 throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, smooth_w = 1),
               "'smooth_w' must be a single odd integer >= 3")
})

test_that("negative min_water_ha throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, min_water_ha = -1),
               "'min_water_ha' must be a single positive numeric value or NULL")
})

test_that("zero min_water_ha throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, min_water_ha = 0),
               "'min_water_ha' must be a single positive numeric value or NULL")
})

test_that("non-numeric min_water_ha throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, min_water_ha = "1"),
               "'min_water_ha' must be a single positive numeric value or NULL")
})

test_that("non-logical return_ndwi throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, return_ndwi = "yes"),
               "'return_ndwi' must be a single logical value")
})

test_that("non-logical plot throws error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_error(apply_water_mask(t, s, plot = "yes"),
               "'plot' must be a single logical value")
})



# Return type and structure

test_that("result is a SpatRaster", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("result has same dimensions as target_raster", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  expect_equal(terra::nrow(result), terra::nrow(t))
  expect_equal(terra::ncol(result), terra::ncol(t))
})

test_that("result has same CRS as target_raster", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  expect_true(terra::same.crs(result, t))
})

test_that("result preserves layer names from target_raster", {
  t        <- make_raster(rep(0.5, 9))
  names(t) <- "NDVI"
  s        <- make_satelite()
  result   <- apply_water_mask(t, s, smooth = FALSE,
                               min_water_ha = NULL, plot = FALSE)
  expect_equal(names(result), "NDVI")
})

test_that("result is returned invisibly", {
  t   <- make_raster(rep(0.5, 9))
  s   <- make_satelite()
  out <- withVisible(apply_water_mask(t, s, smooth = FALSE,
                                      min_water_ha = NULL, plot = FALSE))
  expect_false(out$visible)
})



# Water masking

test_that("land pixels are unchanged after masking", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(!is.na(vals)))
  expect_true(all(vals == 0.5, na.rm = TRUE))
})

test_that("water pixels are set to NA when min_water_ha = NULL", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_water_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(is.na(vals)))
})

test_that("mixed land/water: only water pixels become NA", {
  t <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  s <- make_satelite(n_bands = 8, nrow = 3, ncol = 3)

  green_vals      <- rep(0.3, 9)
  green_vals[1:4] <- 0.8
  nir_vals        <- rep(0.6, 9)
  nir_vals[1:4]   <- 0.1
  terra::values(s[[3]]) <- green_vals
  terra::values(s[[8]]) <- nir_vals

  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(is.na(vals[1:4])))
  expect_true(all(!is.na(vals[5:9])))
})

test_that("multi-layer target_raster: all layers are masked", {
  t      <- c(make_raster(rep(0.5, 9)), make_raster(rep(0.8, 9)))
  s      <- make_water_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  expect_equal(terra::nlyr(result), 2L)
  expect_true(all(is.na(terra::values(result[[1]]))))
  expect_true(all(is.na(terra::values(result[[2]]))))
})



# Geometry alignment
test_that("mismatched resolution is auto-resampled without error", {
  t <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  s <- make_satelite(nrow = 6, ncol = 6)
  expect_no_error(apply_water_mask(t, s, smooth = FALSE,
                                   min_water_ha = NULL, plot = FALSE))
})

test_that("auto-resampled result matches target_raster geometry", {
  t      <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  s      <- make_satelite(nrow = 6, ncol = 6)
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  expect_equal(terra::nrow(result), terra::nrow(t))
  expect_equal(terra::ncol(result), terra::ncol(t))
})

test_that("geometry mismatch emits a message", {
  t <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  s <- make_satelite(nrow = 6, ncol = 6)
  expect_message(apply_water_mask(t, s, smooth = FALSE,
                                  min_water_ha = NULL, plot = FALSE),
                 "Geometry mismatch detected")
})

test_that("already-aligned rasters do not emit geometry mismatch message", {
  t    <- make_raster(rep(0.5, 9))
  s    <- make_satelite()
  msgs <- capture_messages(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = NULL, plot = FALSE)
  )
  expect_length(msgs[grepl("Geometry mismatch", msgs)], 0)
})



# Smoothing

test_that("smooth = TRUE runs without error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_no_error(apply_water_mask(t, s, smooth = TRUE,
                                   min_water_ha = NULL, plot = FALSE))
})

test_that("smooth = TRUE emits smoothing message", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_message(apply_water_mask(t, s, smooth = TRUE,
                                  min_water_ha = NULL, plot = FALSE),
                 "Smoothing NDWI")
})

test_that("smooth = FALSE does not emit smoothing message", {
  t    <- make_raster(rep(0.5, 9))
  s    <- make_satelite()
  msgs <- capture_messages(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = NULL, plot = FALSE)
  )
  expect_length(msgs[grepl("Smoothing NDWI", msgs)], 0)
})

test_that("smooth_w = 5 runs without error", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_no_error(
    apply_water_mask(t, s, smooth = TRUE, smooth_w = 5,
                     min_water_ha = NULL, plot = FALSE)
  )
})



# min_water_ha

test_that("min_water_ha = NULL masks all water pixels", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_water_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(is.na(vals)))
})

test_that("min_water_ha emits pixel size message", {
  t <- make_raster(rep(0.5, 9))
  s <- make_water_satelite()
  expect_message(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = 1, plot = FALSE),
    "Minimum water patch size"
  )
})

test_that("min_water_ha larger than any patch emits no-patch message", {
  t <- make_raster(rep(0.5, 9), crs = "EPSG:32719",
                   xmin = 0, xmax = 30, ymin = 0, ymax = 30)
  s <- make_water_satelite(nrow = 3, ncol = 3, crs = "EPSG:32719",
                           xmin = 0, xmax = 30, ymin = 0, ymax = 30)
  expect_message(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = 1, plot = FALSE),
    "No water patches"
  )
})

test_that("min_water_ha larger than any patch: no pixels are masked", {
  t      <- make_raster(rep(0.5, 9), crs = "EPSG:32719",
                        xmin = 0, xmax = 30, ymin = 0, ymax = 30)
  s      <- make_water_satelite(nrow = 3, ncol = 3, crs = "EPSG:32719",
                                xmin = 0, xmax = 30, ymin = 0, ymax = 30)
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = 1, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_true(all(!is.na(vals)))
})

test_that("small isolated water pixels are not masked with min_water_ha", {
  t <- make_raster(rep(0.5, 100), nrow = 10, ncol = 10)
  s <- make_satelite(n_bands = 8, nrow = 10, ncol = 10)

  green_vals     <- rep(0.3, 100)
  green_vals[55] <- 0.8
  nir_vals       <- rep(0.6, 100)
  nir_vals[55]   <- 0.1
  terra::values(s[[3]]) <- green_vals
  terra::values(s[[8]]) <- nir_vals

  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = 999999, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_false(is.na(vals[55]))
})



# return_ndwi

test_that("return_ndwi = TRUE attaches ndwi attribute", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL,
                             return_ndwi  = TRUE, plot = FALSE)
  expect_false(is.null(attr(result, "ndwi")))
  expect_true(inherits(attr(result, "ndwi"), "SpatRaster"))
})

test_that("return_ndwi = FALSE does not attach ndwi attribute", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL,
                             return_ndwi  = FALSE, plot = FALSE)
  expect_null(attr(result, "ndwi"))
})

test_that("attached NDWI has correct layer name", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_satelite()
  result <- apply_water_mask(t, s, smooth = FALSE,
                             min_water_ha = NULL,
                             return_ndwi  = TRUE, plot = FALSE)
  expect_equal(names(attr(result, "ndwi")), "NDWI")
})



# message

test_that("apply_water_mask emits NDWI computation message", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_message(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = NULL, plot = FALSE),
    "NDWI computed from bands"
  )
})



# plot parameter

test_that("plot = TRUE runs without error or warning", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = NULL, plot = TRUE)
  )
})

test_that("plot = TRUE is the default", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = NULL)
  )
})

test_that("plot = TRUE still returns correct result", {
  t      <- make_raster(rep(0.5, 9))
  s      <- make_water_satelite()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressWarnings(
    result <- apply_water_mask(t, s, smooth = FALSE,
                               min_water_ha = NULL, plot = TRUE)
  )
  vals <- as.vector(terra::values(result))
  expect_true(all(is.na(vals)))
})

test_that("plot = FALSE still emits NDWI message", {
  t <- make_raster(rep(0.5, 9))
  s <- make_satelite()
  expect_message(
    apply_water_mask(t, s, smooth = FALSE, min_water_ha = NULL, plot = FALSE),
    "NDWI computed from bands"
  )
})
