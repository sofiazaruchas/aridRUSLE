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

test_that("breaks with fewer than 3 values throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(classify_erosion_risk(eri, breaks = c(0, 1)),
               "'breaks' must be a numeric vector with at least 3 values")
})

test_that("non-numeric breaks throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(classify_erosion_risk(eri, breaks = c("0", "0.5", "1")),
               "'breaks' must be a numeric vector")
})

test_that("breaks not starting at 0 throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(classify_erosion_risk(eri, breaks = c(0.1, 0.5, 1)),
               "'breaks' must start at 0 and end at 1")
})

test_that("breaks not ending at 1 throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(classify_erosion_risk(eri, breaks = c(0, 0.5, 0.9)),
               "'breaks' must start at 0 and end at 1")
})

test_that("non-strictly-increasing breaks throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(classify_erosion_risk(eri, breaks = c(0, 0.5, 0.5, 1)),
               "'breaks' must be strictly increasing")
})

test_that("labels length mismatch throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(
    classify_erosion_risk(eri,
                          breaks = c(0, 0.5, 1),
                          labels = c("Low", "Medium", "High")),
    "'labels' must be a character vector of length"
  )
})

test_that("non-character labels throws error", {
  eri <- make_eri(seq(0, 1, length.out = 9))
  expect_error(
    classify_erosion_risk(eri,
                          breaks = c(0, 0.5, 1),
                          labels = c(1, 2)),
    "'labels' must be a character vector"
  )
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

test_that("result is a list with elements 'classified' and 'summary'", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_true(is.list(result))
  expect_named(result, c("classified", "summary"))
})

test_that("$classified is a SpatRaster", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_true(inherits(result$classified, "SpatRaster"))
})

test_that("$classified has one layer named ERI_class", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(terra::nlyr(result$classified), 1L)
  expect_equal(names(result$classified), "ERI_class")
})

test_that("$classified has same dimensions as input", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(terra::nrow(result$classified), terra::nrow(eri))
  expect_equal(terra::ncol(result$classified), terra::ncol(eri))
})

test_that("$summary is a data.frame", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_true(is.data.frame(result$summary))
})

test_that("$summary has correct columns", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_named(result$summary, c("class", "label", "pixels",
                                 "area_ha", "proportion"))
})

test_that("$summary has one row per class (default: 5)", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(nrow(result$summary), 5L)
})

test_that("$summary has correct number of rows for custom breaks", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri,
                                  breaks = c(0, 0.33, 0.66, 1),
                                  labels = c("Low", "Medium", "High"),
                                  plot   = FALSE)
  expect_equal(nrow(result$summary), 3L)
})

test_that("$summary labels match supplied labels", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri,
                                  breaks = c(0, 0.5, 1),
                                  labels = c("Low", "High"),
                                  plot   = FALSE)
  expect_equal(result$summary$label, c("Low", "High"))
})

test_that("$summary proportion values sum to 1", {
  eri    <- make_eri(seq(0.05, 0.95, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(sum(result$summary$proportion), 1, tolerance = 1e-3)
})

test_that("$summary pixels sum equals total non-NA cell count", {
  eri    <- make_eri(seq(0.05, 0.95, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(sum(result$summary$pixels), terra::ncell(eri))
})

test_that("$summary pixels sum excludes NA pixels", {
  eri_vals    <- seq(0, 1, length.out = 9)
  eri_vals[1] <- NA
  eri_vals[2] <- NA
  eri    <- make_eri(eri_vals)
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_equal(sum(result$summary$pixels), terra::ncell(eri) - 2L)
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

test_that("values in 0-0.2 are classified as class 1 (default breaks)", {
  eri    <- make_eri(c(0.0, 0.1, 0.15, 0.2), nrow = 2, ncol = 2)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(vals == 1, na.rm = TRUE))
})

test_that("values in 0.2-0.4 are classified as class 2 (default breaks)", {
  eri    <- make_eri(c(0.21, 0.3, 0.35, 0.4), nrow = 2, ncol = 2)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(vals == 2, na.rm = TRUE))
})

test_that("values in 0.8-1.0 are classified as class 5 (default breaks)", {
  eri    <- make_eri(c(0.81, 0.9, 0.95, 1.0), nrow = 2, ncol = 2)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(vals == 5, na.rm = TRUE))
})

test_that("result contains only valid class integers", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(vals %in% 1:5, na.rm = TRUE))
})

test_that("NA pixels in ERI are preserved as NA in $classified", {
  eri_vals    <- seq(0, 1, length.out = 9)
  eri_vals[1] <- NA
  eri_vals[5] <- NA
  eri    <- make_eri(eri_vals)
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(is.na(vals[1]))
  expect_true(is.na(vals[5]))
})

test_that("boundary value 0 is classified as class 1", {
  eri    <- make_eri(rep(0, 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(vals == 1, na.rm = TRUE))
})

test_that("boundary value 1 is classified as highest class", {
  eri    <- make_eri(rep(1, 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(vals == 5, na.rm = TRUE))
})

test_that("all-NA ERI returns all-NA $classified without error", {
  eri    <- make_eri(rep(NA_real_, 9))
  expect_no_error(classify_erosion_risk(eri, plot = FALSE))
  result <- classify_erosion_risk(eri, plot = FALSE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(is.na(vals)))
})

test_that("all-NA ERI returns zero pixels and proportion in $summary", {
  eri    <- make_eri(rep(NA_real_, 9))
  result <- classify_erosion_risk(eri, plot = FALSE)
  expect_true(all(result$summary$pixels == 0))
  expect_true(all(result$summary$proportion == 0))
})

test_that("custom breaks produce correct number of classes", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  result <- classify_erosion_risk(eri,
                                  breaks = c(0, 0.33, 0.66, 1),
                                  labels = c("Low", "Medium", "High"),
                                  plot   = FALSE)
  vals <- as.vector(terra::values(result$classified))
  expect_true(all(vals %in% 1:3, na.rm = TRUE))
})



# ── message ───────────────────────────────────────────────────────────────────

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

test_that("plot = TRUE still returns correct $classified values", {
  eri    <- make_eri(seq(0, 1, length.out = 9))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  result <- classify_erosion_risk(eri, plot = TRUE)
  vals   <- as.vector(terra::values(result$classified))
  expect_true(all(vals %in% 1:5, na.rm = TRUE))
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
