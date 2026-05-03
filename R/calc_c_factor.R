#' Calculate Vegetation Cover Factor (C-Factor)
#'
#' Computes the RUSLE C-factor (vegetation cover and management factor) from
#' a water-masked NDVI composite using the exponential relationship proposed
#' by Mahgoub et al. (2012). The C-factor ranges from 0 (dense vegetation,
#' no erosion) to 1 (bare soil, maximum erosion).
#'
#' The C-factor is calculated as:
#' \deqn{C = 0.353 \cdot e^{1.669 \cdot NDVI}}{C = 0.353 * exp(1.669 * NDVI)}
#'
#' Output values are clamped to the range from 0 to 1. Water pixels (NA in the
#' input) remain NA in the output and propagate automatically into the
#' final erosion risk index computed by \code{calc_erosion_risk()}.
#'
#' It is strongly recommended to apply \code{apply_water_mask()} to the
#' NDVI composite before passing it to this function, as open water surfaces
#' produce misleading NDVI values that would otherwise bias the C-factor.
#'
#' @param ndvi_raster SpatRaster. NDVI composite with values in the range from
#'   -1 to 1. Should be water-masked beforehand using
#'   \code{apply_water_mask()}. Water pixels (NA) are preserved as NA in
#'   the output.
#' @param verbose Logical. If TRUE, prints a summary of the computed C-factor
#'   (min, mean, max) to the console. Default: TRUE.
#' @param plot Logical. If TRUE, displays a map of the C-factor after
#'   computation. Default: TRUE.
#'
#' @return SpatRaster with one layer of C-factor values in the range from 0 to 1
#'   (dimensionless), with the same extent, resolution and CRS as
#'   \code{ndvi_raster}. Water pixels are NA. The layer is named
#'   \code{"C_factor"}.
#'
#' @references
#' Mahgoub, M. et al. (2012). Estimation of soil loss from semi-arid area
#' using RUSLE model and remote sensing. \emph{CATENA}, 100, 126-133.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   ndvi <- terra::rast("s2_ndvi.tif")
#'   s2   <- terra::rast("s2_bands.tif")
#'
#'   # Step 1: mask water pixels
#'   ndvi_masked <- apply_water_mask(ndvi, s2)
#'
#'   # Step 2: compute C-factor from masked NDVI
#'   c_factor <- calc_c_factor(ndvi_masked)
#'
#'   # Without plot
#'   c_factor <- calc_c_factor(ndvi_masked, plot = FALSE)
#' }

calc_c_factor <- function(ndvi_raster,
                          verbose = TRUE,
                          plot    = TRUE) {

  # 0. Input validation

  if (!inherits(ndvi_raster, "SpatRaster")) {
    stop("'ndvi_raster' must be a terra::SpatRaster object.")
  }

  if (terra::nlyr(ndvi_raster) != 1) {
    stop(sprintf(
      "'ndvi_raster' must have exactly 1 layer. Found: %d.",
      terra::nlyr(ndvi_raster)
    ))
  }

  # Check NDVI range -- warn if values outside -1 to 1 are detected
  # NA-guard needed: terra::global() returns NA for all-NA rasters
  ndvi_range <- terra::global(ndvi_raster, c("min", "max"), na.rm = TRUE)
  if (!is.na(ndvi_range$min) && !is.na(ndvi_range$max)) {
    if (ndvi_range$min < -1 || ndvi_range$max > 1) {
      warning(sprintf(
        "'ndvi_raster' contains values outside the valid NDVI range -1 to 1: min = %.3f, max = %.3f. Check your input data.",
        ndvi_range$min, ndvi_range$max
      ))
    }
  }

  # 1. Compute C-factor

  # C = 0.353 * exp(1.669 * NDVI)
  # Values are clamped to [0, 1]:
  #   - NDVI close to -1 (water/shadow): exp term very small -> C near 0
  #   - NDVI close to  0 (bare soil):    C ~ 0.353 * exp(0) = 0.353
  #   - NDVI close to  1 (dense veg):    C ~ 0.353 * exp(1.669) ~ 1.85 -> clamped to 1

  c_raster <- terra::app(ndvi_raster, function(ndvi) {
    c_val <- 0.353 * exp(1.669 * ndvi)
    pmin(pmax(c_val, 0), 1)   # clamp to [0, 1]
  })

  names(c_raster) <- "C_factor"

  # 2. Verbose output
  if (verbose) {
    c_stats <- terra::global(c_raster, c("min", "mean", "max"), na.rm = TRUE)
    message(sprintf(
      "[calc_c_factor] C-factor computed  |  min: %.3f  mean: %.3f  max: %.3f",
      c_stats$min, c_stats$mean, c_stats$max
    ))
    message(
      "[calc_c_factor] Formula: C = 0.353 * exp(1.669 * NDVI) | ",
      "Mahgoub et al. (2012)"
    )
  }

  # 3. Plot

  if (plot) {
    c_stats <- terra::global(c_raster, c("min", "mean", "max"), na.rm = TRUE)

    terra::plot(
      c_raster,
      main   = "C-Factor | Mahgoub et al. (2012)",
      sub    = paste0("min: ", round(c_stats$min, 3),
                      "  mean: ", round(c_stats$mean, 3),
                      "  max: ", round(c_stats$max, 3),
                      "  (dimensionless, 0 = dense veg, 1 = bare soil)"),
      col    = grDevices::hcl.colors(100, palette = "RdYlGn", rev = TRUE),
      colNA  = "steelblue",
      axes   = TRUE,
      legend = TRUE,
      mar    = c(3, 3, 3, 8)
    )
  }

  return(invisible(c_raster))
}
