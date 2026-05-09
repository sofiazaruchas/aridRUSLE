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

test_that("invalid resample_method throws error", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_error(
    calc_erosion_risk(r, ls, c, resample_method = "invalid"),
    "'resample_method' is not a recognised terra resampling method"
  )
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
  r  <- make_raster(rep(1, 4), nrow = 2, ncol = 2)
  ls <- make_raster(rep(1, 9), nrow = 3, ncol = 3)
  c  <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3)
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_equal(terra::nrow(result), terra::nrow(ls))
  expect_equal(terra::ncol(result), terra::ncol(ls))
  expect_equal(as.vector(terra::ext(result)), as.vector(terra::ext(ls)),
               tolerance = 1e-6)
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
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  msgs <- capture_messages(calc_erosion_risk(r, ls, c, plot = FALSE))
  alignment_msgs <- msgs[grepl("resampling|reprojecting", msgs)]
  expect_length(alignment_msgs, 0)
})

test_that("only misaligned raster triggers alignment message", {
  r  <- make_raster(rep(1, 4), nrow = 2, ncol = 2)  # misaligned
  ls <- make_raster(rep(1, 9), nrow = 3, ncol = 3)
  c  <- make_raster(rep(0.5, 9), nrow = 3, ncol = 3) # aligned
  msgs <- capture_messages(calc_erosion_risk(r, ls, c, plot = FALSE))
  r_msgs <- msgs[grepl("r_factor", msgs)]
  c_msgs <- msgs[grepl("c_factor", msgs)]
  expect_gte(length(r_msgs), 1)
  expect_length(c_msgs, 0)
})



# Return type and structure

test_that("result is a SpatRaster", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("result has one layer named ERI", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_equal(terra::nlyr(result), 1L)
  expect_equal(names(result), "ERI")
})

test_that("result has same dimensions as ls_factor (reference grid)", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, plot = FALSE)
  expect_equal(terra::nrow(result), terra::nrow(ls))
  expect_equal(terra::ncol(result), terra::ncol(ls))
})

test_that("result is returned invisibly", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  out <- withVisible(calc_erosion_risk(r, ls, c, plot = FALSE))
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
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
  vals <- as.vector(terra::values(result))
  expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE))
})

test_that("ERI is zero when any normalised factor is constant", {
  r  <- make_raster(rep(5, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
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
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = FALSE)
  vals   <- as.vector(terra::values(result))
  expect_equal(vals, r_vals * ls_vals * c_vals, tolerance = 1e-6)
})

test_that("NA pixels in any factor propagate to ERI", {
  r_vals     <- rep(0.5, 9)
  r_vals[1]  <- NA
  ls_vals    <- rep(0.5, 9)
  ls_vals[5] <- NA
  r  <- make_raster(r_vals)
  ls <- make_raster(ls_vals)
  c  <- make_raster(rep(0.5, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = FALSE)
  vals <- as.vector(terra::values(result))
  expect_true(is.na(vals[1]))
  expect_true(is.na(vals[5]))
})



# normalize behaviour
test_that("normalize = TRUE: max ERI value is 1", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(1, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
  max_val <- terra::global(result, "max", na.rm = TRUE)[[1]]
  expect_equal(max_val, 1, tolerance = 1e-6)
})

test_that("normalize = TRUE: min ERI value is 0", {
  r  <- make_raster(seq(1, 9))
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(1, 9))
  result <- calc_erosion_risk(r, ls, c, normalize = TRUE, plot = FALSE)
  min_val <- terra::global(result, "min", na.rm = TRUE)[[1]]
  expect_equal(min_val, 0, tolerance = 1e-6)
})



# message
test_that("calc_erosion_risk emits a message about ERI computation", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_message(calc_erosion_risk(r, ls, c, plot = FALSE), "ERI computed")
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

test_that("plot = FALSE still emits ERI message", {
  r  <- make_raster(rep(1, 9))
  ls <- make_raster(rep(1, 9))
  c  <- make_raster(rep(0.5, 9))
  expect_message(calc_erosion_risk(r, ls, c, plot = FALSE), "ERI computed")
})

test_that("plot = TRUE with NA pixels runs without error or warning", {
  r_vals    <- seq(1, 9)
  r_vals[1] <- NA
  r  <- make_raster(r_vals)
  ls <- make_raster(seq(1, 9))
  c  <- make_raster(seq(0.1, 0.9, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_warning(calc_erosion_risk(r, ls, c, plot = TRUE))
})

test_that("plot = TRUE with NA pixels still returns correct non-NA ERI values", {
  r_vals     <- seq(0.1, 0.9, length.out = 9)
  r_vals[1]  <- NA
  ls_vals    <- seq(0.2, 1.0, length.out = 9)
  c_vals     <- seq(0.1, 0.5, length.out = 9)
  r  <- make_raster(r_vals)
  ls <- make_raster(ls_vals)
  c  <- make_raster(c_vals)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- calc_erosion_risk(r, ls, c, normalize = FALSE, plot = TRUE)
  vals <- as.vector(terra::values(result))
  expect_true(is.na(vals[1]))
  expect_equal(vals[-1], (r_vals * ls_vals * c_vals)[-1], tolerance = 1e-6)
})
