#' Calculate Rainfall Erosivity (R-Factor)
#'
#' Computes the RUSLE R-factor using the empirical formula appropriate for the
#' climate zone of the study area. The zone is detected automatically from
#' coordinates via get_climate_zone(), or can be set manually.
#'
#' @param precip_raster SpatRaster. Either 1 layer (cumulative seasonal
#'   precipitation in mm) or 12 layers (monthly, Jan to Dec). The required number
#'   of layers depends on the climate zone. The function throws an informative
#'   error if the wrong count is supplied.
#' @param lat Numeric. Latitude of the study area (decimal degrees). Required
#'   for automatic zone detection unless climate_zone is set manually.
#' @param lon Numeric. Longitude of the study area (decimal degrees).
#' @param climate_zone Character or NULL. Manual override of the climate zone.
#'   Valid values: "winter_rain_north", "winter_rain_south", "summer_monsoon",
#'   "hyperarid", "continental", "australian". If NULL (default), the zone is
#'   detected from lat/lon.
#' @param season_days Numeric. Length of the precipitation season in days.
#'   Only used for winter_rain_north / winter_rain_south (Bonilla & Vidal).
#'   Default: 243 (Southern Hemisphere, Jan to Aug). Northern Hemisphere: 182.
#' @param a Numeric. Coefficient a for Bonilla & Vidal (2011).
#'   Only used for winter_rain_*. Default: 0.171.
#' @param b Numeric. Coefficient b for Bonilla & Vidal (2011).
#'   Only used for winter_rain_*. Default: 1.212.
#' @param verbose Logical. If TRUE, prints the detected zone, formula, parameters
#'   and recommended data period to the console. Default: TRUE.
#' @param plot Logical. If TRUE, displays a map of the R-factor after
#'   computation. Default: TRUE.
#'
#' @return SpatRaster with one layer of R-factor values. The attribute
#'   attr(result, "r_factor_meta") contains a list with: zone, formula_name,
#'   params (list of parameters used), and season_recommendation.
#'
#' @references
#'   Bonilla & Vidal (2011). Rainfall erosivity in central Chile.
#'     Journal of Hydrology, 410(1-2), 126-133.
#'   Arnoldus (1980). An approximation of the rainfall factor in the USLE.
#'     In: De Boodt & Gabriels (eds.), Assessment of Erosion. Wiley, 127-132.
#'   Yu & Rosewell (1996). A robust estimator of the R-factor for the USLE.
#'     Transactions of the ASAE, 39(2), 559-561.
#'
#' @examples
#' \dontrun{
#'   precip <- terra::rast("chirps_jan_aug.tif")
#'   r <- calc_r_factor(precip, lat = -33.0, lon = -71.0)
#'
#'   # Manual zone override (skips get_climate_zone())
#'   r <- calc_r_factor(precip, climate_zone = "winter_rain_south",
#'                      season_days = 243)
#'
#'   # Without plot
#'   r <- calc_r_factor(precip, lat = -33.0, lon = -71.0, plot = FALSE)
#' }
#'
#' @export
calc_r_factor <- function(precip_raster,
                          lat          = NULL,
                          lon          = NULL,
                          climate_zone = NULL,
                          season_days  = 243,
                          a            = 0.171,
                          b            = 1.212,
                          verbose      = TRUE,
                          plot         = TRUE) {

  # 0. Input validation

  if (!inherits(precip_raster, "SpatRaster")) {
    stop("'precip_raster' must be a terra::SpatRaster object.")
  }

  n_layers <- terra::nlyr(precip_raster)
  if (!n_layers %in% c(1, 12)) {
    stop(sprintf(
      "'precip_raster' must have either 1 layer (cumulative) or 12 layers (monthly). Found: %d.",
      n_layers
    ))
  }

  # 1. Determine climate zone

  if (!is.null(climate_zone)) {
    valid_zones <- c("winter_rain_north", "winter_rain_south",
                     "summer_monsoon", "hyperarid", "continental", "australian")
    if (!climate_zone %in% valid_zones) {
      stop(sprintf(
        "Unknown climate zone '%s'. Valid values: %s.",
        climate_zone, paste(valid_zones, collapse = ", ")
      ))
    }
    zone <- climate_zone
    if (verbose) {
      message(sprintf("[calc_r_factor] Climate zone set manually: %s", zone))
    }
  } else {
    if (is.null(lat) || is.null(lon)) {
      stop("Either 'climate_zone' or both 'lat' and 'lon' must be provided.")
    }
    zone <- get_climate_zone(lat = lat, lon = lon, verbose = verbose)
  }

  rec <- get_season_recommendation(zone)

  if (verbose) {
    message(sprintf("[calc_r_factor] Formula:              %s", .r_formula_name(zone)))
    message(sprintf("[calc_r_factor] Recommended period:   %s", rec$label))
  }

  # 2. Validate layer count against zone requirements

  needs_12 <- zone %in% c("summer_monsoon", "continental", "australian")
  needs_1  <- zone %in% c("winter_rain_north", "winter_rain_south", "hyperarid")

  if (needs_12 && n_layers != 12) {
    stop(sprintf(
      "Zone '%s' requires 12 monthly layers (Jan-Dec), but 'precip_raster' has %d layer(s).",
      zone, n_layers
    ))
  }
  if (needs_1 && n_layers != 1) {
    stop(sprintf(
      "Zone '%s' requires 1 cumulative layer, but 'precip_raster' has %d layers.",
      zone, n_layers
    ))
  }

  #  3. Compute R-factor

  r_raster <- switch(zone,

                     # Bonilla & Vidal (2011)
                     # R = a * P^b * (season_days / 365)
                     "winter_rain_north" = ,
                     "winter_rain_south" = {
                       if (verbose) {
                         message(sprintf(
                           "[calc_r_factor] Parameters: a = %.3f, b = %.3f, season_days = %d",
                           a, b, season_days
                         ))
                       }
                       terra::app(precip_raster, function(P) {
                         a * (P ^ b) * (season_days / 365)
                       })
                     },

                     # Simplified MFI with aridity correction
                     # R = 0.085 * P^1.350
                     "hyperarid" = {
                       terra::app(precip_raster, function(P) {
                         0.085 * (P ^ 1.350)
                       })
                     },

                     # Modified Fournier Index / Arnoldus (1980)
                     # MFI = sum(pi^2 / P_ann)
                     # R   = 0.739 * MFI^1.847
                     "summer_monsoon" = {
                       terra::app(precip_raster, function(p_months) {
                         p_ann <- sum(p_months, na.rm = TRUE)
                         if (is.na(p_ann) || p_ann == 0) return(NA_real_)
                         mfi <- sum((p_months ^ 2) / p_ann, na.rm = TRUE)
                         0.739 * (mfi ^ 1.847)
                       })
                     },

                     # Arnoldus (1980), steppe adaptation
                     # MFI = sum(pi^2 / P_ann)
                     # R   = 4.17 * MFI - 152  (negative values clamped to 0)
                     "continental" = {
                       terra::app(precip_raster, function(p_months) {
                         p_ann <- sum(p_months, na.rm = TRUE)
                         if (is.na(p_ann) || p_ann == 0) return(NA_real_)
                         mfi   <- sum((p_months ^ 2) / p_ann, na.rm = TRUE)
                         r_val <- 4.17 * mfi - 152
                         max(r_val, 0)
                       })
                     },

                     # Yu & Rosewell (1996)
                     # R = sum_i [ 1.735 * 10^(1.5 * log10(pi^2 / P_ann) - 0.8188) ]
                     "australian" = {
                       terra::app(precip_raster, function(p_months) {
                         p_ann <- sum(p_months, na.rm = TRUE)
                         if (is.na(p_ann) || p_ann == 0) return(NA_real_)
                         terms <- 1.735 * 10 ^ (1.5 * log10((p_months ^ 2) / p_ann) - 0.8188)
                         sum(terms, na.rm = TRUE)
                       })
                     }
  )

  # 4. Attach metadata

  meta <- list(
    zone                  = zone,
    formula_name          = .r_formula_name(zone),
    season_recommendation = rec$label,
    params                = list(
      a           = if (zone %in% c("winter_rain_north", "winter_rain_south")) a else NULL,
      b           = if (zone %in% c("winter_rain_north", "winter_rain_south")) b else NULL,
      season_days = if (zone %in% c("winter_rain_north", "winter_rain_south")) season_days else NULL
    )
  )
  attr(r_raster, "r_factor_meta") <- meta

  names(r_raster) <- "R_factor"

  if (verbose) {
    r_stats <- terra::global(r_raster, c("min", "mean", "max"), na.rm = TRUE)
    message(sprintf(
      "[calc_r_factor] R-factor computed  |  min: %.1f  mean: %.1f  max: %.1f",
      r_stats$min, r_stats$mean, r_stats$max
    ))
  }

  #  5. Plot

  if (plot) {
    r_stats <- terra::global(r_raster, c("min", "mean", "max"), na.rm = TRUE)

    terra::plot(
      r_raster,
      main   = paste0("R-Factor | ", .r_formula_name(zone)),
      sub    = paste0("Zone: ", zone,
                      "  |  min: ", round(r_stats$min, 1),
                      "  mean: ", round(r_stats$mean, 1),
                      "  max: ", round(r_stats$max, 1),
                      "  (MJ mm ha-1 h-1 yr-1)"),
      col    = grDevices::hcl.colors(100, palette = "Blues", rev = TRUE),
      colNA  = "lightgrey",
      axes   = TRUE,
      legend = TRUE,
      mar    = c(3, 3, 3, 8)
    )
  }

  return(invisible(r_raster))
}


# Internal helper: formula name per zone

.r_formula_name <- function(zone) {
  switch(zone,
         "winter_rain_north" = "Bonilla & Vidal (2011)",
         "winter_rain_south" = "Bonilla & Vidal (2011)",
         "summer_monsoon"    = "Modified Fournier Index / Arnoldus (1980)",
         "hyperarid"         = "Simplified MFI with aridity correction",
         "continental"       = "Arnoldus (1980): steppe adaptation",
         "australian"        = "Yu & Rosewell (1996)",
         "Unknown"
  )
}
