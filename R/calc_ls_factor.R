#' Calculate Topographic LS-Factor
#'
#' Computes the RUSLE LS-factor (slope length x slope steepness) from a
#' digital elevation model (DEM) using the Moore & Burch (1986) approach.
#'
#' The LS-factor is calculated as:
#' \deqn{LS = \left(\frac{A}{22.13}\right)^{0.4} \cdot \left(\frac{\sin(\beta)}{0.0896}\right)^{1.3}}{LS = (A / 22.13)^0.4 * (sin(beta) / 0.0896)^1.3}
#'
#' where \eqn{A} is the cell size in metres and \eqn{\beta} is the slope
#' in radians.
#'
#' If the DEM is in a geographic CRS (degrees), it is automatically
#' reprojected to the appropriate UTM zone before computation.
#'
#' @param dem SpatRaster. Digital elevation model with elevation values in
#'   metres. If in a geographic CRS (degrees), it is automatically reprojected
#'   to the appropriate UTM zone. A missing CRS will trigger an error.
#' @param verbose Logical. If TRUE, prints a summary of the computed LS-factor
#'   (min, mean, max) and the cell size used to the console. Default: TRUE.
#' @param plot Logical. If TRUE, displays a map of the LS-factor after
#'   computation. Default: TRUE.
#'
#' @return SpatRaster with one layer of LS-factor values (dimensionless,
#'   values clamped to 0 or above), with the same extent and CRS as the
#'   (possibly reprojected) input DEM. The layer is named \code{"LS_factor"}.
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
#'   dem <- terra::rast("srtm.tif")
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

  if (nchar(terra::crs(dem)) == 0) {
    stop(
      "'dem' has no CRS defined. Set a CRS using terra::set.crs() before ",
      "calling calc_ls_factor()."
    )
  }

  # 1. CRS check and automatic reprojection to UTM

  if (isTRUE(terra::is.lonlat(dem))) {
    ext        <- terra::ext(dem)
    lon_c      <- (ext$xmin + ext$xmax) / 2
    lat_c      <- (ext$ymin + ext$ymax) / 2
    zone       <- floor((lon_c + 180) / 6) + 1
    hemisphere <- if (lat_c >= 0) "north" else "south"
    epsg       <- if (hemisphere == "north") 32600 + zone else 32700 + zone

    if (verbose) {
      message(sprintf(
        "[calc_ls_factor] Geographic CRS detected. Reprojecting to UTM zone %d%s (EPSG:%d).",
        zone, toupper(substr(hemisphere, 1, 1)), epsg
      ))
    }

    dem <- terra::project(dem, paste0("EPSG:", epsg))
  }

  # 2. Compute slope

  # terra::terrain() returns slope in degrees by default; convert to radians
  slope_deg <- terra::terrain(dem, v = "slope", unit = "degrees")
  slope_rad <- slope_deg * (pi / 180)

  # 3. Get cell size (A)
  # Use the mean of x and y resolution to handle slightly non-square cells
  res_xy <- terra::res(dem)
  A      <- mean(res_xy)

  # 4. Compute LS-factor
  # LS = (A / 22.13)^0.4 * (sin(beta) / 0.0896)^1.3
  ls_raster <- terra::app(slope_rad, function(beta) {
    ls_val <- ((A / 22.13) ^ 0.4) * ((sin(beta) / 0.0896) ^ 1.3)
    pmax(ls_val, 0)
  })

  names(ls_raster) <- "LS_factor"

  # 5. Verbose output
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

  # 6. Plot

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
