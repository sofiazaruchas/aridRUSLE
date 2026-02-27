# apply_water_mask.R
# Mask water pixels in a raster using an NDWI threshold.
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
#' on NDWI values. Required preprocessing step before \code{calc_c_factor()},
#' as open water surfaces produce misleading NDVI values.
#'
#' If \code{target_raster} and \code{ndwi_raster} do not share the same
#' geometry (extent, resolution, CRS), \code{ndwi_raster} is automatically
#' resampled to match \code{target_raster} using nearest-neighbour
#' interpolation before masking.
#'
#' @param target_raster SpatRaster. The raster to be masked (e.g. NDVI
#'   composite). All layers are masked simultaneously.
#' @param ndwi_raster SpatRaster. Single-layer raster with NDWI values.
#'   Pixels with NDWI >= \code{threshold} are classified as water.
#' @param threshold Numeric. NDWI threshold above which pixels are classified
#'   as water and set to \code{NA} in the output. Default: \code{0.3}
#'   (McFeeters 1996).
#'
#' @return SpatRaster with the same dimensions and CRS as \code{target_raster}.
#'   Water pixels are set to \code{NA}, all other pixels are unchanged.
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
#' ndwi <- terra::rast("s2_ndwi.tif")
#'
#' # Mask water pixels with default threshold (0.3)
#' ndvi_masked <- apply_water_mask(ndvi, ndwi)
#'
#' # Use a stricter threshold
#' ndvi_masked <- apply_water_mask(ndvi, ndwi, threshold = 0.1)
#' }

apply_water_mask <- function(target_raster, ndwi_raster, threshold = 0.3) {

  # Input validation

  if (!inherits(target_raster, "SpatRaster"))
    stop("'target_raster' must be a SpatRaster object.")
  if (!inherits(ndwi_raster, "SpatRaster"))
    stop("'ndwi_raster' must be a SpatRaster object.")
  if (!is.numeric(threshold) || length(threshold) != 1)
    stop("'threshold' must be a single numeric value.")
  if (threshold < -1 || threshold > 1)
    stop("'threshold' must be between -1 and 1 (valid NDWI range).")
  if (terra::nlyr(ndwi_raster) != 1)
    stop("'ndwi_raster' must be a single-layer SpatRaster.")

  # Geometry alignment

  # Resample ndwi_raster to match target_raster if geometries differ
  same_geom <- isTRUE(
    tryCatch(
      terra::compareGeom(target_raster, ndwi_raster,
                         res = TRUE, stopOnError = TRUE),
      error = function(e) FALSE
    )
  )

  if (!same_geom) {
    message("Geometry mismatch detected: resampling 'ndwi_raster' to match ",
            "'target_raster'.")
    ndwi_raster <- terra::resample(ndwi_raster, target_raster, method = "near")
  }

  #  Water mask
  # Build binary water mask: NA = water (NDWI >= threshold), 1 = land
  water_mask <- terra::ifel(ndwi_raster >= threshold, NA, 1)

  # Apply mask to all layers of target_raster
  result <- target_raster * water_mask

  # Preserve layer names from input
  names(result) <- names(target_raster)

  return(result)
}
