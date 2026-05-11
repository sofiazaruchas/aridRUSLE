# apply_water_mask.R
# Mask water pixels in a raster using an NDWI threshold.
# NDWI is computed internally from a multi-band satelite image.
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
#' on NDWI values computed internally from a multi-band satelite image.
#' Required preprocessing step before \code{calc_c_factor()}, as open water
#' surfaces produce misleading NDVI values.
#'
#' The NDWI is calculated as:
#' \deqn{NDWI = \frac{Green - NIR}{Green + NIR}}{NDWI = (Green - NIR) / (Green + NIR)}
#'
#' Band indices default to standard Sentinel-2 band numbering
#' (\code{green_band = 3}, \code{nir_band = 8}). Adjust these if using a
#' different sensor or band ordering (e.g. a False Colour Composite where
#' NIR is stored as band 1).
#'
#' If \code{target_raster} and \code{satelite_raster} do not share the same
#' geometry (extent, resolution, CRS), \code{satelite_raster} is automatically
#' resampled to match \code{target_raster} using nearest-neighbour
#' interpolation before masking.
#'
#' If \code{smooth = TRUE}, the NDWI raster is spatially smoothed with a
#' mean filter before thresholding. This reduces isolated salt-and-pepper
#' pixels caused by resampling artefacts at water-land boundaries.
#'
#' If \code{min_water_ha} is set, only connected water patches with an area
#' of at least \code{min_water_ha} hectares are masked. This removes isolated
#' single pixels and small artefacts, retaining only true water bodies such
#' as lakes and reservoirs. The minimum area is converted to pixels
#' automatically based on the resolution of \code{satelite_raster}, making
#' the threshold resolution-independent and scientifically justifiable.
#' Recommended values based on the literature:
#' \itemize{
#'   \item \code{0.1} ha — EU Water Framework Directive minimum mapping unit
#'   \item \code{1.0} ha — FAO Irrigation & Drainage standard
#'   \item \code{4.0} ha — typical for Landsat-based (30 m) studies
#' }
#'
#' Setting \code{plot = TRUE} displays the masked result so the user can
#' visually inspect the water mask and adjust \code{threshold} or
#' \code{min_water_ha} if needed before proceeding to \code{calc_c_factor()}.
#'
#' @param target_raster SpatRaster. The raster to be masked (e.g. NDVI
#'   composite). All layers are masked simultaneously.
#' @param satelite_raster SpatRaster. Multi-band satelite image containing
#'   at least the green and NIR bands. Used to compute NDWI internally.
#' @param green_band Integer. Band index of the green band in
#'   \code{satelite_raster}. Default: \code{3} (Sentinel-2 Band 3).
#'   Use \code{3} for standard Sentinel-2 multiband images.
#'   Adjust if using a False Colour Composite or a different sensor.
#' @param nir_band Integer. Band index of the NIR band in
#'   \code{satelite_raster}. Default: \code{8} (Sentinel-2 Band 8).
#'   Use \code{1} if working with a Sentinel-2 False Colour Composite
#'   (RGB = NIR / Red / Green).
#' @param threshold Numeric. NDWI threshold above which pixels are classified
#'   as water and set to \code{NA} in the output. Default: \code{0.3}
#'   (McFeeters 1996).
#' @param smooth Logical. If \code{TRUE}, applies a mean filter to the NDWI
#'   raster before thresholding to reduce isolated resampling artefacts at
#'   water-land boundaries. Window size is controlled by \code{smooth_w}.
#'   Default: \code{TRUE}.
#' @param smooth_w Integer. Window size for the smoothing filter (must be an
#'   odd integer >= 3). Only used when \code{smooth = TRUE}.
#'   Default: \code{3}.
#' @param min_water_ha Numeric or NULL. Minimum area in hectares for a
#'   connected water patch to be masked. Patches smaller than this threshold
#'   are kept as land. Converted to pixels automatically based on the
#'   resolution of \code{satelite_raster}. If \code{NULL}, all pixels above
#'   \code{threshold} are masked regardless of patch size.
#'   Recommended: \code{1} (FAO standard) for semi-arid regions.
#' @param return_ndwi Logical. If \code{TRUE}, the internally computed NDWI
#'   raster (after smoothing, if applicable) is stored as an attribute
#'   (\code{attr(result, "ndwi")}) on the returned object.
#'   Default: \code{FALSE}.
#' @param plot Logical. If \code{TRUE} (default), displays the masked raster
#'   (first layer) with water pixels shown in steelblue. Useful for visually
#'   inspecting the mask and adjusting \code{threshold} or
#'   \code{min_water_ha} if needed.
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
#' s2   <- terra::rast("s2_bands.tif")  # standard multi-band Sentinel-2 image
#'
#' # Mask water pixels with default settings (Sentinel-2 bands, smooth = TRUE)
#' ndvi_masked <- apply_water_mask(ndvi, s2)
#'
#' # Only mask water bodies >= 1 ha (FAO standard)
#' ndvi_masked <- apply_water_mask(ndvi, s2, min_water_ha = 1)
#'
#' # False Colour Composite (NIR stored as band 1)
#' s2_fcc      <- terra::rast("sentinel_false_colour.tif")
#' ndvi_masked <- apply_water_mask(ndvi, s2_fcc, green_band = 3, nir_band = 1,
#'                                 min_water_ha = 1)
#'
#' # Inspect the result visually before proceeding
#' ndvi_masked <- apply_water_mask(ndvi, s2, min_water_ha = 1, plot = TRUE)
#'
#' # Disable smoothing and patch filtering
#' ndvi_masked <- apply_water_mask(ndvi, s2, smooth = FALSE,
#'                                 min_water_ha = NULL)
#'
#' # Retrieve the internally computed NDWI raster
#' ndvi_masked <- apply_water_mask(ndvi, s2, return_ndwi = TRUE)
#' ndwi        <- attr(ndvi_masked, "ndwi")
#' }

apply_water_mask <- function(target_raster, satelite_raster,
                             green_band   = 3,
                             nir_band     = 8,
                             threshold    = 0.3,
                             smooth       = TRUE,
                             smooth_w     = 3,
                             min_water_ha = 1,
                             return_ndwi  = FALSE,
                             plot         = TRUE) {

  # Input validation

  if (!inherits(target_raster, "SpatRaster"))
    stop("'target_raster' must be a SpatRaster object.")
  if (!inherits(satelite_raster, "SpatRaster"))
    stop("'satelite_raster' must be a SpatRaster object.")

  n_bands <- terra::nlyr(satelite_raster)
  if (!is.numeric(green_band) || length(green_band) != 1 ||
      green_band != as.integer(green_band) ||
      green_band < 1 || green_band > n_bands)
    stop("'green_band' must be a single integer between 1 and nlyr(satelite_raster) (",
         n_bands, ").")
  if (!is.numeric(nir_band) || length(nir_band) != 1 ||
      nir_band != as.integer(nir_band) ||
      nir_band < 1 || nir_band > n_bands)
    stop("'nir_band' must be a single integer between 1 and nlyr(satelite_raster) (",
         n_bands, ").")
  if (green_band == nir_band)
    stop("'green_band' and 'nir_band' must refer to different bands.")
  if (!is.numeric(threshold) || length(threshold) != 1)
    stop("'threshold' must be a single numeric value.")
  if (threshold < -1 || threshold > 1)
    stop("'threshold' must be between -1 and 1 (valid NDWI range).")
  if (!is.logical(smooth) || length(smooth) != 1)
    stop("'smooth' must be a single logical value (TRUE or FALSE).")
  if (!is.numeric(smooth_w) || length(smooth_w) != 1 ||
      smooth_w != as.integer(smooth_w) ||
      smooth_w < 3 || smooth_w %% 2 == 0)
    stop("'smooth_w' must be a single odd integer >= 3.")
  if (!is.null(min_water_ha) &&
      (!is.numeric(min_water_ha) || length(min_water_ha) != 1 ||
       min_water_ha <= 0))
    stop("'min_water_ha' must be a single positive numeric value or NULL.")
  if (!is.logical(return_ndwi) || length(return_ndwi) != 1)
    stop("'return_ndwi' must be a single logical value (TRUE or FALSE).")
  if (!is.logical(plot) || length(plot) != 1)
    stop("'plot' must be a single logical value (TRUE or FALSE).")

  # Geometry alignment

  same_geom <- isTRUE(
    tryCatch(
      terra::compareGeom(target_raster, satelite_raster,
                         res = TRUE, stopOnError = TRUE),
      error = function(e) FALSE
    )
  )

  if (!same_geom) {
    message("Geometry mismatch detected: resampling 'satelite_raster' to match ",
            "'target_raster' (method = 'bilinear').")
    satelite_raster <- terra::resample(satelite_raster, target_raster,
                                       method = "bilinear")
  }

  # Compute NDWI

  green <- satelite_raster[[green_band]]
  nir   <- satelite_raster[[nir_band]]
  ndwi  <- (green - nir) / (green + nir)
  names(ndwi) <- "NDWI"

  # Optional smoothing

  if (smooth) {
    message("Smoothing NDWI with ", smooth_w, "x", smooth_w,
            " mean filter to reduce resampling artefacts.")
    ndwi <- terra::focal(ndwi, w = smooth_w, fun = "mean", na.rm = TRUE)
  }

  # Water mask

  if (!is.null(min_water_ha)) {

    # Compute pixel area in hectares
    if (terra::is.lonlat(satelite_raster)) {
      res_x_m       <- terra::res(satelite_raster)[1] * 111000
      res_y_m       <- terra::res(satelite_raster)[2] * 111000
      pixel_area_ha <- (res_x_m * res_y_m) / 10000
    } else {
      pixel_area_ha <- prod(terra::res(satelite_raster)) / 10000
    }

    min_pixels <- ceiling(min_water_ha / pixel_area_ha)

    message("Minimum water patch size: ", min_water_ha, " ha = ",
            min_pixels, " pixels (pixel area = ",
            round(pixel_area_ha, 4), " ha).")

    # Label connected water patches (8-connectivity)
    water_binary  <- terra::ifel(ndwi >= threshold, 1, NA)
    water_patches <- terra::patches(water_binary, directions = 8,
                                    zeroAsNA = TRUE)

    # Count pixels per patch and keep only large ones
    patch_sizes   <- terra::freq(water_patches)
    large_patches <- patch_sizes$value[patch_sizes$count >= min_pixels]

    if (length(large_patches) == 0) {
      message("No water patches >= ", min_water_ha, " ha found. ",
              "No pixels will be masked. Consider lowering 'min_water_ha' ",
              "or 'threshold'.")
      result <- target_raster

    } else {
      rcl      <- matrix(c(large_patches, rep(1, length(large_patches))),
                         ncol = 2)
      is_water <- terra::classify(water_patches, rcl, others = 0)
      # Use terra::mask() instead of multiplication to avoid geometry issues
      result   <- terra::mask(target_raster, is_water,
                              maskvalues = 1, updatevalue = NA)
    }

  } else {
    # No patch filtering: mask all pixels above threshold
    water_raster <- terra::ifel(ndwi >= threshold, 1, NA)
    result       <- terra::mask(target_raster, water_raster,
                                maskvalues = 1, updatevalue = NA)
  }

  # Preserve layer names from input
  names(result) <- names(target_raster)

  # Optionally attach NDWI as attribute

  if (return_ndwi) {
    attr(result, "ndwi") <- ndwi
  }

  # Message

  message("NDWI computed from bands ", green_band, " (green) and ", nir_band,
          " (NIR). Smoothing: ", smooth,
          ". Min water patch: ",
          ifelse(is.null(min_water_ha), "none", paste0(min_water_ha, " ha")),
          ". Adjust 'threshold' and rerun if the mask does not look correct.")

  # Optional plot
  if (plot) {
    plot_title <- paste0(
      "Water Mask | threshold = ", threshold,
      ifelse(smooth, paste0(" | smooth ", smooth_w, "x", smooth_w), ""),
      ifelse(!is.null(min_water_ha),
             paste0(" | min ", min_water_ha, " ha"), "")
    )

    binary  <- terra::ifel(is.na(result[[1]]), NA, 1)
    all_na  <- all(is.na(terra::values(binary)))

    if (all_na) {
      terra::plot(ndwi,
                  main = paste0(plot_title, " [WARNING: all pixels masked]"),
                  axes = TRUE,
                  mar  = c(3, 3, 3, 8))
      warning("All pixels were masked as water. ",
              "Check 'threshold', 'green_band' and 'nir_band'.")
    } else {
      terra::plot(
        binary,
        main       = plot_title,
        col        = "yellow",
        background = "steelblue",
        legend     = FALSE,
        axes       = TRUE,
        mar        = c(3, 3, 3, 8)
      )
    }

    graphics::par(xpd = TRUE)
    usr <- graphics::par("usr")
    graphics::legend(
      x      = usr[2] + (usr[2] - usr[1]) * 0.02,
      y      = usr[4],
      legend = c("Land", "Water"),
      fill   = c("yellow", "steelblue"),
      border = "white",
      bty    = "n"
    )
    graphics::par(xpd = FALSE)
  }

  return(invisible(result))
}
