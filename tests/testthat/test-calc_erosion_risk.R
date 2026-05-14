# test_calc_erosion_risk.R
# Tests for calc_erosion_risk() using synthetic SpatRasters.
# No real geodata required – runs fully offline.
#
# Run:
#   source("test_calc_erosion_risk.R")
# or in package context:
#   devtools::load_all(); source("tests/test_calc_erosion_risk.R")

library(terra)

# Helper functions

pass <- function(msg) cat(sprintf("  \u2713 PASS  %s\n", msg))
fail <- function(msg) cat(sprintf("  \u2717 FAIL  %s\n", msg))

expect_true <- function(cond, msg) {
  if (isTRUE(cond)) pass(msg) else { fail(msg); stop(msg) }
}
expect_error <- function(expr, msg) {
  ok <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  if (ok) pass(msg) else { fail(msg); stop(msg) }
}
expect_message <- function(expr, pattern, msg) {
  msgs <- character(0)
  withCallingHandlers(force(expr),
                      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  ok <- any(grepl(pattern, msgs))
  if (ok) pass(msg) else { fail(msg); stop(msg) }
}

section <- function(title) cat(sprintf("\n── %s\n", title))

# Create synthetic test rasters

# All rasters: 30 x 30 pixels, geographic CRS (WGS-84).
# Generic extent (0-1 degrees) – no region-specific coordinates,
# since calc_erosion_risk() is designed for global application.

make_rast <- function(vals, crs = "EPSG:4326",
                      xmin = 0, xmax = 1,
                      ymin = 0, ymax = 1,
                      nrow = 30, ncol = 30) {
  r <- terra::rast(nrows = nrow, ncols = ncol,
                   xmin = xmin, xmax = xmax,
                   ymin = ymin, ymax = ymax,
                   crs  = crs)
  terra::values(r) <- vals
  r
}

set.seed(42)
n <- 30 * 30

r_vals   <- runif(n, 0,  200)
ls_vals  <- runif(n, 0,   20)
c_vals   <- runif(n, 0,    1)
dem_vals <- seq(500, 3000, length.out = n) + rnorm(n, sd = 50)

r_rast   <- make_rast(r_vals)
ls_rast  <- make_rast(ls_vals)
c_rast   <- make_rast(c_vals)
dem_rast <- make_rast(dem_vals)

# Reproject to a generic projected CRS to test geometry alignment
r_rast_proj <- terra::project(r_rast, "EPSG:3857")

#
section("1  Input validation")
#

expect_error(
  calc_erosion_risk("not_a_raster", ls_rast, c_rast, plot = FALSE),
  "r_factor must be a SpatRaster"
)
expect_error(
  calc_erosion_risk(r_rast, "not_a_raster", c_rast, plot = FALSE),
  "ls_factor must be a SpatRaster"
)
expect_error(
  calc_erosion_risk(r_rast, ls_rast, "not_a_raster", plot = FALSE),
  "c_factor must be a SpatRaster"
)
expect_error(
  calc_erosion_risk(r_rast, ls_rast, c_rast, dem = "not_a_raster", plot = FALSE),
  "dem must be a SpatRaster or NULL"
)
expect_error(
  calc_erosion_risk(r_rast, ls_rast, c_rast, normalize = "yes", plot = FALSE),
  "normalize must be logical"
)
expect_error(
  calc_erosion_risk(r_rast, ls_rast, c_rast, plot = "yes"),
  "plot must be logical"
)
expect_error(
  calc_erosion_risk(r_rast, ls_rast, c_rast,
                    resample_method = "invalid", plot = FALSE),
  "resample_method invalid"
)

#
section("2  Basic calculation (normalize = TRUE)")
#

result <- calc_erosion_risk(r_rast, ls_rast, c_rast, plot = FALSE)

expect_true(is.list(result),                          "Return value is a list")
expect_true("eri" %in% names(result),                 "List contains $eri")
expect_true("map" %in% names(result),                 "List contains $map")
expect_true(inherits(result$eri, "SpatRaster"),       "$eri is a SpatRaster")
expect_true(is.null(result$map),                      "$map is NULL when plot = FALSE")
expect_true(names(result$eri) == "ERI",               "Layer name is 'ERI'")

eri_min <- terra::global(result$eri, "min", na.rm = TRUE)[[1]]
eri_max <- terra::global(result$eri, "max", na.rm = TRUE)[[1]]

expect_true(eri_min >= 0,   "ERI minimum value >= 0")
expect_true(eri_max <= 1,   "ERI maximum value <= 1")
expect_true(eri_max  > 0,   "ERI has values > 0 (not all zero)")

#
section("3  normalize = FALSE")
#

result_raw <- calc_erosion_risk(r_rast, ls_rast, c_rast,
                                normalize = FALSE, plot = FALSE)

raw_max <- terra::global(result_raw$eri, "max", na.rm = TRUE)[[1]]

expect_true(raw_max > 1,
            "ERI without normalisation can exceed 1 (raw-value multiplication)")

#
section("4  Geometry alignment (mismatched CRS)")
#

expect_message(
  calc_erosion_risk(r_rast_proj, ls_rast, c_rast, plot = FALSE),
  "reprojecting",
  "Reprojection message appears on CRS mismatch"
)

result_aligned <- calc_erosion_risk(r_rast_proj, ls_rast, c_rast, plot = FALSE)

expect_true(
  terra::same.crs(result_aligned$eri, ls_rast),
  "Result CRS matches ls_factor after alignment"
)

#
section("5  All three factors identical (edge case: constant raster)")
#

const_rast   <- make_rast(rep(5, n))
result_const <- calc_erosion_risk(const_rast, const_rast, const_rast,
                                  plot = FALSE)

# When all factors are constant, norm01() returns 0 everywhere
const_max <- terra::global(result_const$eri, "max", na.rm = TRUE)[[1]]
expect_true(const_max == 0,
            "Constant inputs -> ERI is 0 everywhere (norm01 returns 0)")

#
section("6  NA handling")
#

r_na <- r_rast
terra::values(r_na)[1:50] <- NA   # set 50 of 900 pixels to NA

result_na <- calc_erosion_risk(r_na, ls_rast, c_rast, plot = FALSE)

na_count <- sum(is.na(terra::values(result_na$eri)))
expect_true(na_count >= 50,
            "NA pixels from r_factor are preserved in ERI (>= 50 NAs)")
expect_true(na_count < n,
            "Not all pixels are NA (only the injected NAs propagated)")

#
section("7  Output geometry")
#
result_geom <- calc_erosion_risk(r_rast, ls_rast, c_rast, plot = FALSE)

expect_true(
  terra::same.crs(result_geom$eri, ls_rast),
  "ERI CRS matches ls_factor"
)
expect_true(
  all(terra::res(result_geom$eri) == terra::res(ls_rast)),
  "ERI resolution matches ls_factor"
)
expect_true(
  all(as.vector(terra::ext(result_geom$eri)) ==
        as.vector(terra::ext(ls_rast))),
  "ERI extent matches ls_factor"
)

#
section("8  plot = TRUE without DEM (no hillshade)")
#

result_plot <- calc_erosion_risk(r_rast, ls_rast, c_rast,
                                 plot      = TRUE,
                                 map_title = "Test: ERI without DEM")

expect_true(inherits(result_plot$map, "tmap"),
            "$map is a tmap object when plot = TRUE")

#
section("9  plot = TRUE with DEM (hillshade path)")
#

result_hs <- calc_erosion_risk(r_rast, ls_rast, c_rast,
                               dem            = dem_rast,
                               plot           = TRUE,
                               map_title      = "Test: ERI with hillshade",
                               figure_caption = "Figure X: Test plot",
                               data_source    = "Source: Synthetic test data")

expect_true(inherits(result_hs$map, "tmap"),
            "$map is a tmap object with DEM path")

#
section("10  resample_method variants")
#

for (method in c("near", "bilinear", "cubic")) {
  res_m <- calc_erosion_risk(r_rast_proj, ls_rast, c_rast,
                             resample_method = method, plot = FALSE)
  expect_true(inherits(res_m$eri, "SpatRaster"),
              sprintf("resample_method = '%s' produces a valid result", method))
}

#
cat("\n── All tests completed \u2713\n\n")
