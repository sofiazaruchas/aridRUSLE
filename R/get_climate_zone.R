# Determine climate zone from coordinates using the Koeppen-Geiger system
#
# References:
#   Peel, M.C., Finlayson, B.L., & McMahon, T.A. (2007). Updated world map
#   of the Koeppen-Geiger climate classification. Hydrology and Earth System
#   Sciences, 11, 1633-1644.
#
#   Bryant, C. et al. (2017). kgc: Koeppen-Geiger Climatic Zones.
#   R package version 1.0.0.2. https://CRAN.R-project.org/package=kgc
#
# Requires: kgc (>= 1.0.0.2)



# Internal helper: Map Koeppen-Geiger code to aridRUSLE zone
#
# The kgc package returns standard Koeppen-Geiger codes (e.g. "BSk", "BWh").
# This function maps those codes to the five aridRUSLE zones.
#
# Mapping logic based on:
#   - Zone descriptions and example regions in aridRUSLE documentation (Table 3)
#   - Koeppen-Geiger class definitions from Peel et al. (2007)

.map_koppen_to_zone <- function(koppen_code, lat, lon) {

  # Australian semi-arid: any BS/BW class located on the Australian continent
  if (koppen_code %in% c("BSh", "BSk", "BWh", "BWk") &&
      lat < -15 && lat > -45 &&
      lon > 113 && lon < 155) {
    return("australian")
  }

  # Hyper-arid: BW = true desert (hot BWh or cold BWk)
  if (koppen_code %in% c("BWh", "BWk")) {
    return("hyperarid")
  }

  # Summer monsoon: BSh (hot steppe) outside Australia
  if (koppen_code == "BSh") {
    return("summer_monsoon")
  }

  # Continental semi-arid: BSk (cold steppe) outside Australia
  if (koppen_code == "BSk") {
    return("continental")
  }

  # Fallback: winter rain, split by hemisphere
  # Northern hemisphere (e.g. Morocco, Tunisia, S-Iberia): October - March
  # Southern hemisphere (e.g. Chile): January - August
  if (lat >= 0) {
    return("winter_rain_north")
  } else {
    return("winter_rain_south")
  }
}

# Exported main function


#' Determine climate zone from coordinates
#'
#' Classifies a coordinate into one of five climate categories using the
#' Koeppen-Geiger climate classification system (Peel et al., 2007),
#' accessed via the \pkg{kgc} package (Bryant et al., 2017).
#' No raster download required - the classification table is bundled
#' inside the \pkg{kgc} package.
#'
#' Prints the detected zone, the underlying Koeppen-Geiger code, and the
#' recommended data period to the console. Used internally by
#' \code{calc_r_factor()}, but can also be called directly.
#'
#' @param lat Numeric. Latitude of the study area (decimal degrees).
#' @param lon Numeric. Longitude of the study area (decimal degrees).
#' @param verbose Logical. Print detected zone, Koeppen code, and recommended
#'   period to the console. Default: \code{TRUE}.
#'
#' @return Character string - one of five zones:
#'   \code{"winter_rain"}, \code{"summer_monsoon"}, \code{"hyperarid"},
#'   \code{"continental"}, \code{"australian"}
#'
#' @references
#' Peel, M.C., Finlayson, B.L., & McMahon, T.A. (2007). Updated world map of
#' the Koeppen-Geiger climate classification. \emph{Hydrology and Earth System
#' Sciences}, 11, 1633-1644.
#'
#' Bryant, C., Gruson, H., & Gaborit, P. (2017). kgc: Koeppen-Geiger Climatic
#' Zones. R package version 1.0.0.2.
#' \url{https://CRAN.R-project.org/package=kgc}
#'
#' @export
#'
#' @importFrom utils data
#'
#' @examples
#' get_climate_zone(lat = -29.9, lon = -70.8)   # Chile         -> winter_rain
#' get_climate_zone(lat =  13.5, lon =   2.1)   # Sahel         -> summer_monsoon
#' get_climate_zone(lat =  23.0, lon =  45.0)   # Arabia        -> hyperarid
#' get_climate_zone(lat =  45.0, lon =  65.0)   # Central Asia  -> continental
#' get_climate_zone(lat = -25.0, lon = 134.0)   # Australia     -> australian

get_climate_zone <- function(lat, lon, verbose = TRUE) {

  # Validate inputs
  if (!is.numeric(lat) || length(lat) != 1)
    stop("'lat' must be a single numeric value.")
  if (!is.numeric(lon) || length(lon) != 1)
    stop("'lon' must be a single numeric value.")
  if (lat < -90  || lat >  90)
    stop("'lat' must be between -90 and 90.")
  if (lon < -180 || lon > 180)
    stop("'lon' must be between -180 and 180.")


  # RoundCoordinates() snaps coordinates to the nearest 0.5-degree grid point
  # LookupCZ() returns the Koeppen-Geiger code for that grid point
  # Workaround: kgc::LookupCZ() requires climatezones to be loaded
  if (!exists("climatezones")) {
    data("climatezones", package = "kgc", envir = globalenv())
  }

  # Koeppen-Geiger lookup via kgc package
   pt <- data.frame(
    Site          = "study_area",
    Longitude     = lon,
    Latitude      = lat,
    rndCoord.lon  = kgc::RoundCoordinates(lon),
    rndCoord.lat  = kgc::RoundCoordinates(lat)
  )

  koppen_code <- as.character(kgc::LookupCZ(pt))

  # Handle ocean / missing data (kgc returns NA or empty string over oceans)
  if (is.na(koppen_code) || nchar(trimws(koppen_code)) == 0) {
    stop(
      "No Koeppen-Geiger classification found for lat = ", lat, ", lon = ", lon,
      ". The coordinate may be located in the ocean or outside the data coverage."
    )
  }

  # Map Koeppen code to aridRUSLE zone
  zone <- .map_koppen_to_zone(koppen_code, lat, lon)

  #  Get recommended season
  rec <- get_season_recommendation(zone)

  #  Console output
  if (verbose) {
    message("-----------------------------------------")
    message("Detected zone      : ", zone)
    message("Koeppen-Geiger code: ", koppen_code, " (Peel et al. 2007, via kgc)")
    message("Recommended period : ", rec$label, " (", rec$season_days, " days)")
    message("Recommended months : ", rec$months_label)
    if (!is.null(rec$note) && nchar(rec$note) > 0) {
      message("Note               : ", rec$note)
    }
    message("-----------------------------------------")
  }

  return(zone)
}


