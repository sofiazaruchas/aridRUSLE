#' Calculate Topographic LS-Factor
#'
#' Computes the RUSLE LS-factor (slope length x slope steepness) from a
#' digital elevation model (DEM) using the Moore & Burch (1986) approach.
#' The DEM must be in a projected coordinate reference system with metres
#' as the unit (e.g. UTM).
#'
#' The LS-factor is calculated as:
#' \deqn{LS = \left(\frac{A}{22.13}\right)^{0.4} \cdot \left(\frac{\sin(\beta)}{0.0896}\right)^{1.3}}{LS = (A / 22.13)^0.4 * (sin(beta) / 0.0896)^1.3}
#'
#' where \eqn{A} is the cell size in metres and \eqn{\beta} is the slope
#' in radians.
#'
#' @param dem SpatRaster. Digital elevation model with elevation values in
#'   metres. Must be in a projected CRS with metres as the unit (e.g. UTM).
#'   A geographic CRS (degrees) will trigger an error.
#' @param verbose Logical. If TRUE, prints a summary of the computed LS-factor
#'   (min, mean, max) to the console. Default: TRUE.
#' @param plot Logical. If TRUE, displays a map of the LS-factor after
#'   computation. Default: TRUE.
#'
#' #' @return SpatRaster with one layer of LS-factor values (dimensionless,
#'   >= 0), with the same extent, resolution and CRS as \code{dem}.
#'   The layer is named \code{"LS_factor"}.
#'
#' @references
#' Moore, I.D., & Burch, G.J. (1986). Physical basis of the length-slope
#' factor in the Universal Soil Loss Equation. \emph{Soil Science Society
#' of America Journal}, 50(5), 1294-1298.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dem <- terra::rast("srtm_utm.tif")
#'   ls  <- calc_ls_factor(dem)
#'
#'   # Without plot
#'   ls  <- calc_ls_factor(dem, plot = FALSE)
#' }

calc_ls_factor <- function(dem,
                           verbose = TRUE,
                           plot    = TRUE) {

  # 0. Input validation
  if (!inherits(dem, "SpatRaster")) {
    stop("'dem' must be a terra::SpatRaster object.")
  }

  if (terra::nlyr(dem) != 1) {
    stop(sprintf(
      "'dem' must have exactly 1 layer. Found: %d.",
      terra::nlyr(dem)
    ))
  }

  # Check for projected CRS (metres required for cell size calculation)
  crs_wkt <- terra::crs(dem)

  if (nchar(crs_wkt) == 0) {
    stop(
      "'dem' has no CRS defined. Set a projected CRS with metres as the unit ",
      "(e.g. UTM) using terra::set.crs() or terra::project()."
    )
  }

  is_geographic <- isTRUE(terra::is.lonlat(dem))

  if (is_geographic) {
    stop(
      "'dem' must be in a projected CRS with metres as the unit (e.g. UTM). ",
      "Reproject with terra::project() before calling calc_ls_factor()."
    )
  }

  # 1. Compute slope
  # terra::terrain() returns slope in degrees by default; convert to radians
  slope_deg <- terra::terrain(dem, v = "slope", unit = "degrees")
  slope_rad <- slope_deg * (pi / 180)

  # 2. Get cell size (A)
  # Use the mean of x and y resolution to handle slightly non-square cells
  res_xy <- terra::res(dem)
  A      <- mean(res_xy)

  # 3. Compute LS-factor
  # LS = (A / 22.13)^0.4 * (sin(beta) / 0.0896)^1.3
  ls_raster <- terra::app(slope_rad, function(beta) {
    ls_val <- ((A / 22.13) ^ 0.4) * ((sin(beta) / 0.0896) ^ 1.3)
    pmax(ls_val, 0)   # clamp to 0 (sin can be negative for inverted DEMs)
  })

  names(ls_raster) <- "LS_factor"

  # 4. Verbose output
  if (verbose) {
    ls_stats <- terra::global(ls_raster, c("min", "mean", "max"), na.rm = TRUE)
    message(sprintf(
      "[calc_ls_factor] LS-factor computed  |  min: %.3f  mean: %.3f  max: %.3f",
      ls_stats$min, ls_stats$mean, ls_stats$max
    ))
    message(sprintf(
      "[calc_ls_factor] Cell size used: %.1f m  |  Formula: Moore & Burch (1986)",
      A
    ))
  }

  # 5. Plot

  if (plot) {
    ls_stats <- terra::global(ls_raster, c("min", "mean", "max"), na.rm = TRUE)

    terra::plot(
      ls_raster,
      main   = "LS-Factor | Moore & Burch (1986)",
      sub    = paste0("min: ", round(ls_stats$min, 3),
                      "  mean: ", round(ls_stats$mean, 3),
                      "  max: ", round(ls_stats$max, 3),
                      "  (dimensionless)"),
      col    = grDevices::hcl.colors(100, palette = "Oranges", rev = TRUE),
      colNA  = "lightgrey",
      axes   = TRUE,
      legend = TRUE,
      mar    = c(3, 3, 3, 8)
    )
  }

  return(invisible(ls_raster))
}
