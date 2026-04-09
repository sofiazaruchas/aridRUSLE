# apply_water_mask.R
# Mask water pixels in a raster using an NDWI threshold.
# NDWI is computed internally from a multi-band satellite image.
#
# References:
#   McFeeters, S.K. (1996). The use of the Normalised Difference Water Index
#   (NDWI) in the delineation of open water features. International Journal
#   of Remote Sensing, 17(7), 1425-1432.
#
# Requires: terra (>= 1.7-0)

#' Mask water pixels using an NDWI threshold
#'
#' Masks water pixels in a target raster (typically an NDVI composite) based
#' on NDWI values computed internally from a multi-band satellite image.
#' Required preprocessing step before \code{calc_c_factor()}, as open water
#' surfaces produce misleading NDVI values.
#'
#' The NDWI is calculated as:
#' \deqn{NDWI = \frac{Green - NIR}{Green + NIR}}{NDWI = (Green - NIR) / (Green + NIR)}
#'
#' Band indices default to Sentinel-2 band numbering (\code{green_band = 3},
#' \code{nir_band = 8}). Adjust these if using a different sensor or
#' band ordering.
#'
#' If \code{target_raster} and \code{satellite_raster} do not share the same
#' geometry (extent, resolution, CRS), \code{satellite_raster} is automatically
#' resampled to match \code{target_raster} using nearest-neighbour
#' interpolation before masking.
#'
#' Setting \code{plot = TRUE} displays the masked result so the user can
#' visually inspect the water mask and adjust \code{threshold} if needed
#' before proceeding to \code{calc_c_factor()}.
#'
#' @param target_raster SpatRaster. The raster to be masked (e.g. NDVI
#'   composite). All layers are masked simultaneously.
#' @param satellite_raster SpatRaster. Multi-band satellite image containing
#'   at least the green and NIR bands. Used to compute NDWI internally.
#' @param green_band Integer. Band index of the green band in
#'   \code{satellite_raster}. Default: \code{3} (Sentinel-2 Band 3).
#' @param nir_band Integer. Band index of the NIR band in
#'   \code{satellite_raster}. Default: \code{8} (Sentinel-2 Band 8).
#' @param threshold Numeric. NDWI threshold above which pixels are classified
#'   as water and set to \code{NA} in the output. Default: \code{0.3}
#'   (McFeeters 1996).
#' @param return_ndwi Logical. If \code{TRUE}, the internally computed NDWI
#'   raster is stored as an attribute (\code{attr(result, "ndwi")}) on the
#'   returned object. Default: \code{FALSE}.
#' @param plot Logical. If \code{TRUE}, displays the masked raster (first
#'   layer) with water pixels shown in light blue. Useful for visually
#'   inspecting the mask and adjusting \code{threshold} if needed.
#'   Default: \code{FALSE}.
#'
#' @return SpatRaster with the same dimensions and CRS as \code{target_raster}.
#'   Water pixels are set to \code{NA}, all other pixels are unchanged.
#'   If \code{return_ndwi = TRUE}, the computed NDWI raster is accessible via
#'   \code{attr(result, "ndwi")}.
#'
#' @references
#' McFeeters, S.K. (1996). The use of the Normalised Difference Water Index
#' (NDWI) in the delineation of open water features. \emph{International
#' Journal of Remote Sensing}, 17(7), 1425-1432.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ndvi <- terra::rast("s2_ndvi.tif")
#' s2   <- terra::rast("s2_bands.tif")  # multi-band Sentinel-2 image
#'
#' # Mask water pixels with default threshold and Sentinel-2 band indices
#' ndvi_masked <- apply_water_mask(ndvi, s2)
#'
#' # Inspect the result visually before proceeding
#' ndvi_masked <- apply_water_mask(ndvi, s2, threshold = 0.3, plot = TRUE)
#'
#' # Retrieve the internally computed NDWI raster
#' ndvi_masked <- apply_water_mask(ndvi, s2, return_ndwi = TRUE)
#' ndwi        <- attr(ndvi_masked, "ndwi")
#'
#' # Custom band indices (e.g. different sensor or band ordering)
#' ndvi_masked <- apply_water_mask(ndvi, s2, green_band = 2, nir_band = 4)
#' }

apply_water_mask <- function(target_raster, satellite_raster,
                             green_band = 3, nir_band = 8,
                             threshold = 0.3, return_ndwi = FALSE,
                             plot = FALSE) {

  # Input validation

  if (!inherits(target_raster, "SpatRaster"))
    stop("'target_raster' must be a SpatRaster object.")
  if (!inherits(satellite_raster, "SpatRaster"))
    stop("'satellite_raster' must be a SpatRaster object.")

  n_bands <- terra::nlyr(satellite_raster)
  if (!is.numeric(green_band) || length(green_band) != 1 ||
      green_band != as.integer(green_band) ||
      green_band < 1 || green_band > n_bands)
    stop("'green_band' must be a single integer between 1 and nlyr(satellite_raster) (",
         n_bands, ").")
  if (!is.numeric(nir_band) || length(nir_band) != 1 ||
      nir_band != as.integer(nir_band) ||
      nir_band < 1 || nir_band > n_bands)
    stop("'nir_band' must be a single integer between 1 and nlyr(satellite_raster) (",
         n_bands, ").")
  if (green_band == nir_band)
    stop("'green_band' and 'nir_band' must refer to different bands.")
  if (!is.numeric(threshold) || length(threshold) != 1)
    stop("'threshold' must be a single numeric value.")
  if (threshold < -1 || threshold > 1)
    stop("'threshold' must be between -1 and 1 (valid NDWI range).")
  if (!is.logical(return_ndwi) || length(return_ndwi) != 1)
    stop("'return_ndwi' must be a single logical value (TRUE or FALSE).")
  if (!is.logical(plot) || length(plot) != 1)
    stop("'plot' must be a single logical value (TRUE or FALSE).")

  # Geometry alignment

  same_geom <- isTRUE(
    tryCatch(
      terra::compareGeom(target_raster, satellite_raster,
                         res = TRUE, stopOnError = TRUE),
      error = function(e) FALSE
    )
  )

  if (!same_geom) {
    message("Geometry mismatch detected: resampling 'satellite_raster' to match ",
            "'target_raster'.")
    satellite_raster <- terra::resample(satellite_raster, target_raster,
                                        method = "near")
  }

  # Compute NDWI internally

  green <- satellite_raster[[green_band]]
  nir   <- satellite_raster[[nir_band]]
  ndwi  <- (green - nir) / (green + nir)
  names(ndwi) <- "NDWI"

  # Water mask

  # Build binary water mask: NA = water (NDWI >= threshold), 1 = land
  water_mask <- terra::ifel(ndwi >= threshold, NA, 1)

  # Apply mask to all layers of target_raster
  result <- target_raster * water_mask

  # Preserve layer names from input
  names(result) <- names(target_raster)

  # Optionally attach computed NDWI as attribute
  if (return_ndwi) {
    attr(result, "ndwi") <- ndwi
  }

  # Print message
  message("NDWI computed from bands ", green_band, " (green) and ", nir_band,
          " (NIR). Adjust 'threshold' and rerun if the mask does not look correct.")

  # Optional plot
  if (plot) {
    binary <- terra::ifel(is.na(result[[1]]), NA, 1)

    terra::plot(
      binary,
      main       = paste0("Water Mask | NDWI threshold = ", threshold),
      col        = "yellow",
      background = "steelblue",
      legend     = FALSE,
      axes       = TRUE,
      mar        = c(3, 3, 3, 8)
    )

    graphics::par(xpd = TRUE)

    graphics::legend(
      x      = "topright",
      legend = c("Land", "Water"),
      fill   = c("yellow", "steelblue"),
      border = "white",
      bty    = "n",
      inset  = c(-0.15, 0)
    )

    graphics::par(xpd = FALSE)
  }

  return(invisible(result))

}
