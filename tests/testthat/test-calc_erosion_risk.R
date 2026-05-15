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



# Input validation

test_that("non-SpatRaster r_factor throws error", {
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(matrix(1:9, 3, 3), ls, c, plot = FALSE),
               "'r_factor' must be a SpatRaster")
})

test_that("non-SpatRaster ls_factor throws error", {
  r <- make_raster(rep(1, 9))
  c <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(r, matrix(1:9, 3, 3), c, plot = FALSE),
               "'ls_factor' must be a SpatRaster")
})

test_that("non-SpatRaster c_factor throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  expect_error(calc_erosion_risk(r, ls, matrix(1:9, 3, 3), plot = FALSE),
               "'c_factor' must be a SpatRaster")
})

test_that("non-logical normalize throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(r, ls, c, normalize = "yes", plot = FALSE),
               "'normalize' must be a single logical value")
})

test_that("non-logical plot throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(calc_erosion_risk(r, ls, c, plot = "yes"),
               "'plot' must be a single logical value")
})

test_that("invalid resample_method throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(
    calc_erosion_risk(r, ls, c, resample_method = "invalid", plot = FALSE),
    "'resample_method' is not a recognised terra resampling method"
  )
})



# Return type and structure

test_that("result is a list with elements 'eri' and 'map'", {
  r      <- make_raster(rep(1, 9))
  ls     <- make_raster(rep(1, 9))
  c      <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_true(is.list(result))
  expect_named(result, c("eri", "map"))
})

test_that("$eri is a SpatRaster", {
  r      <- make_raster(rep(1, 9))
  ls     <- make_raster(rep(1, 9))
  c      <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_true(inherits(result$eri, "SpatRaster"))
})

test_that("$map is NULL when plot = FALSE", {
  r      <- make_raster(rep(1, 9))
  ls     <- make_raster(rep(1, 9))
  c      <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_null(result$map)
})

test_that("$eri has one layer named ERI", {
  r      <- make_raster(rep(1, 9))
  ls     <- make_raster(rep(1, 9))
  c      <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_equal(terra::nlyr(result$eri), 1L)
  expect_equal(names(result$eri), "ERI")
})

test_that("$eri has same dimensions as ls_factor", {
  r      <- make_raster(rep(1, 9))
  ls     <- make_raster(rep(1, 9))
  c      <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_equal(terra::nrow(result$eri), terra::nrow(ls))
  expect_equal(terra::ncol(result$eri), terra::ncol(ls))
})

test_that("result is returned invisibly", {
  r   <- make_raster(rep(1, 9))
  ls  <- make_raster(rep(1, 9))
  c   <- make_raster(rep(0.5, 9))
  out <- withVisible(calc_erosion_risk(r, ls, c, plot = FALSE))
  expect_false(out$visible)
})



# ERI values

test_that("ERI values are in range 0-1 with normalize = TRUE", {
  r      <- make_raster(seq(1, 9))
  ls     <- make_raster(seq(1, 9))
  c      <- make_raster(seq(0.1, 0.9, length.out = 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
  vals   <- as.vector(terra::values(result$eri))
  expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE))
})

test_that("ERI is zero when any normalised factor is constant", {
  r      <- make_raster(rep(5, 9))
  ls     <- make_raster(seq(1, 9))
  c      <- make_raster(seq(0.1, 0.9, length.out = 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
  vals   <- as.vector(terra::values(result$eri))
  expect_true(all(vals == 0, na.rm = TRUE))
})

test_that("normalize = FALSE: ERI equals direct product of inputs", {
  r_vals  <- seq(0.1, 0.9, length.out = 9)
  ls_vals <- seq(0.2, 1.0, length.out = 9)
  c_vals  <- seq(0.1, 0.5, length.out = 9)
  r       <- make_raster(r_vals)
  ls      <- make_raster(ls_vals)
  c       <- make_raster(c_vals)
  result  <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = FALSE)
  expect_equal(as.vector(terra::values(result$eri)),
               r_vals * ls_vals * c_vals, tolerance = 1e-6)
})

test_that("normalize = FALSE: ERI can exceed 1", {
  r      <- make_raster(rep(10, 9))
  ls     <- make_raster(rep(10, 9))
  c      <- make_raster(rep(10, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = FALSE)
  expect_true(terra::global(result$eri, "max", na.rm = TRUE)[[1]] > 1)
})

test_that("normalize = TRUE: max ERI value is 1", {
  r      <- make_raster(seq(1, 9))
  ls     <- make_raster(seq(1, 9))
  c      <- make_raster(seq(1, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
  expect_equal(terra::global(result$eri, "max", na.rm = TRUE)[[1]], 1,
               tolerance = 1e-6)
})

test_that("normalize = TRUE: min ERI value is 0", {
  r      <- make_raster(seq(1, 9))
  ls     <- make_raster(seq(1, 9))
  c      <- make_raster(seq(1, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
  expect_equal(terra::global(result$eri, "min", na.rm = TRUE)[[1]], 0,
               tolerance = 1e-6)
})

test_that("NA pixels in any factor propagate to ERI", {
  r_vals     <- rep(0.5, 9)
  r_vals[1]  <- NA
  ls_vals    <- rep(0.5, 9)
  ls_vals[5] <- NA
  r      <- make_raster(r_vals)
  ls     <- make_raster(ls_vals)
  c      <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = FALSE)
  vals   <- as.vector(terra::values(result$eri))
  expect_true(is.na(vals[1]))
  expect_true(is.na(vals[5]))
})



# Auto-alignment

test_that("mismatched resolution is auto-resampled without error", {
  r  <- make_raster(rep(1, 4), nrow = 2, ncol = 2)
  ls <- make_raster(rep(1, 9), nrow = 3, ncol = 3)
  c  <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  expect_no_error(calc_erosion_risk(r, ls, c, plot = FALSE))
})

test_that("mismatched extent is auto-resampled without error", {
  r  <- make_raster(rep(1, 9), xmin = 0, xmax = 2)
  ls <- make_raster(rep(1, 9), xmin = 0, xmax = 1)
  c  <- make_raster(rep(0.5, 9), xmin = 0, xmax = 1)
  expect_no_error(calc_erosion_risk(r, ls, c, plot = FALSE))
})

test_that("auto-resampled result matches ls_factor geometry", {
  r      <- make_raster(rep(1, 4), nrow = 2, ncol = 2)
  ls     <- make_raster(rep(1, 9), nrow = 3, ncol = 3)
  c      <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_equal(terra::nrow(result$eri), terra::nrow(ls))
  expect_equal(terra::ncol(result$eri), terra::ncol(ls))
  expect_equal(as.vector(terra::ext(result$eri)),
               as.vector(terra::ext(ls)), tolerance = 1e-6)
})

test_that("mismatched resolution emits resampling message", {
  r  <- make_raster(rep(1, 4), nrow = 2, ncol = 2)
  ls <- make_raster(rep(1, 9), nrow = 3, ncol = 3)
  c  <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  expect_message(calc_erosion_risk(r, ls, c, plot = FALSE), "resampling")
})

test_that("mismatched CRS emits reprojecting message", {
  r  <- make_raster(rep(1, 9), crs = "EPSG:4269")
  ls <- make_raster(rep(1, 9), crs = "EPSG:4326")
  c  <- make_raster(rep(0.5, 9), crs = "EPSG:4326")
  expect_message(calc_erosion_risk(r, ls, c, plot = FALSE), "reprojecting")
})

test_that("resample_method = 'near' runs without error", {
  r  <- make_raster(rep(1, 4), nrow = 2, ncol = 2)
  ls <- make_raster(rep(1, 9), nrow = 3, ncol = 3)
  c  <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  expect_no_error(
    calc_erosion_risk(r, ls, c, resample_method = "near", plot = FALSE)
  )
})

test_that("already-aligned rasters do not trigger alignment message", {
  r    <- make_raster(rep(1, 9))
  ls   <- make_raster(rep(1, 9))
  c    <- make_raster(rep(0.5, 9))
  msgs <- capture_messages(calc_erosion_risk(r, ls, c, plot = FALSE))
  expect_length(msgs[grepl("resampling|reprojecting", msgs)], 0)
})

test_that("only misaligned raster triggers alignment message", {
  r    <- make_raster(rep(1, 4), nrow = 2, ncol = 2)
  ls   <- make_raster(rep(1, 9), nrow = 3, ncol = 3)
  c    <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  msgs <- capture_messages(calc_erosion_risk(r, ls, c, plot = FALSE))
  expect_gte(length(msgs[grepl("r_factor", msgs)]), 1)
  expect_length(msgs[grepl("c_factor", msgs)], 0)
})



# message

test_that("calc_erosion_risk emits ERI computation message", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_message(calc_erosion_risk(r, ls, c, plot = FALSE), "ERI computed")
})



# map_title / figure_caption / data_source

test_that("custom map_title runs without error", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  expect_no_error(
    calc_erosion_risk(r, ls, c, plot = FALSE, map_title = "Custom Title")
  )
})

test_that("figure_caption runs without error", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  expect_no_error(
    calc_erosion_risk(r, ls, c, plot = FALSE,
                      figure_caption = "Figure 1: Test")
  )
})

test_that("data_source runs without error", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  expect_no_error(
    calc_erosion_risk(r, ls, c, plot = FALSE,
                      data_source = "Source: Synthetic data")
  )
})



# plot = TRUE returns ggplot object

test_that("plot = TRUE returns a ggplot object", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- calc_erosion_risk(r, ls, c, plot = TRUE)
  expect_true(inherits(result$map, "ggplot"))
})

test_that("plot = TRUE returns correct ERI values", {
  r_vals  <- seq(0.1, 0.9, length.out = 9)
  ls_vals <- seq(0.2, 1.0, length.out = 9)
  c_vals  <- seq(0.1, 0.5, length.out = 9)
  r       <- make_raster(r_vals)
  ls      <- make_raster(ls_vals)
  c       <- make_raster(c_vals)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = TRUE)
  expect_equal(as.vector(terra::values(result$eri)),
               r_vals * ls_vals * c_vals, tolerance = 1e-6)
})

test_that("plot = TRUE runs without error or warning", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(calc_erosion_risk(r, ls, c, plot = TRUE))
})



# plot = FALSE always returns NULL map

test_that("plot = FALSE returns NULL map", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  expect_null(calc_erosion_risk(r, ls, c, plot = FALSE)$map)
})
