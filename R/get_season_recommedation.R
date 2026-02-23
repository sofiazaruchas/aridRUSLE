# get_season_recommendation.R
# Return recommended aggregation period for a given climate zone
#
# All period definitions are taken directly from:
#   aridRUSLE package documentation, Table 2:
#   "Recommended periods for precipitation, NDVI and NDWI per climate zone"



#' Get recommended data aggregation period for a climate zone
#'
#' Returns the recommended aggregation period for precipitation (CHIRPS),
#' NDVI, and NDWI for a given climate zone. All three input datasets must
#' always cover the same period, corresponding to the rainy season of the
#' respective climate zone.
#'
#' Can be called before downloading data to determine the correct period.
#' Also used internally by \code{get_climate_zone()} and
#' \code{calc_r_factor()}.
#'
#' @param zone Character string. One of five climate zone IDs:
#'   \code{"winter_rain"}, \code{"summer_monsoon"}, \code{"hyperarid"},
#'   \code{"continental"}, \code{"australian"}.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{zone}{Climate zone ID (character)}
#'     \item{label}{Human-readable period label, e.g. \code{"January - August (243 days)"}}
#'     \item{months}{Integer vector of recommended months, e.g. \code{c(1,2,3,4,5,6,7,8)}}
#'     \item{months_label}{Comma-separated month abbreviations, e.g. \code{"Jan, Feb, Mar, ..."}}
#'     \item{season_days}{Number of days in the recommended period (integer)}
#'     \item{note}{Additional guidance note (character, may be empty)}
#'   }
#'
#' @export
#'
#' @examples
#' get_season_recommendation("winter_rain")
#' get_season_recommendation("summer_monsoon")
#' get_season_recommendation("hyperarid")
#' get_season_recommendation("continental")
#' get_season_recommendation("australian")

get_season_recommendation <- function(zone) {

  # Validate input
  valid_zones <- c("winter_rain", "summer_monsoon", "hyperarid",
                   "continental", "australian")

  if (!is.character(zone) || length(zone) != 1) {
    stop("'zone' must be a single character string.")
  }
  if (!zone %in% valid_zones) {
    stop(
      "'zone' must be one of: ",
      paste(valid_zones, collapse = ", "),
      ". Got: '", zone, "'"
    )
  }

  # Period definitions (source: aridRUSLE documentation, Table 2)
  rec <- switch(zone,

                # BSk/BWk (Mediterranean) - January to August
                # Recommended for winter-rain dominated semi-arid climates (e.g. Chile,
                # Morocco, Tunisia, S-Iberia). Covers the main rainy season.
                winter_rain = list(
                  zone         = "winter_rain",
                  label        = "January - August (243 days)",
                  months       = 1:8,
                  months_label = "Jan, Feb, Mar, Apr, May, Jun, Jul, Aug",
                  season_days  = 243L,
                  note         = ""
                ),

                # BSh (Sahel, India) - June to September
                # Captures the summer monsoon season. Requires 12 monthly input layers
                # for the Modified Fournier Index formula.
                summer_monsoon = list(
                  zone         = "summer_monsoon",
                  label        = "June - September (122 days)",
                  months       = 6:9,
                  months_label = "Jun, Jul, Aug, Sep",
                  season_days  = 122L,
                  note         = "Requires 12 monthly input layers for the R-factor formula."
                ),

                # BWh/BWk (Desert core) - Full year
                # Precipitation is highly episodic and not seasonal. The full year is
                # recommended to avoid missing rare extreme events that may cause the
                # majority of annual erosivity.
                hyperarid = list(
                  zone         = "hyperarid",
                  label        = "Full year - January to December (365 days)",
                  months       = 1:12,
                  months_label = "Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec",
                  season_days  = 365L,
                  note         = paste0(
                    "Precipitation in hyper-arid zones is highly episodic and not seasonal. ",
                    "The full year is recommended to avoid missing rare extreme events."
                  )
                ),

                # BSk (Steppe) - April to September
                # Snow-free season for continental semi-arid climates with cold winters.
                # Requires 12 monthly input layers.
                continental = list(
                  zone         = "continental",
                  label        = "April - September (183 days)",
                  months       = 4:9,
                  months_label = "Apr, May, Jun, Jul, Aug, Sep",
                  season_days  = 183L,
                  note         = "Requires 12 monthly input layers for the R-factor formula."
                ),

                # Australian semi-arid (Outback) - October to March
                # Southern hemisphere summer season.
                # Requires 12 monthly input layers.
                australian = list(
                  zone         = "australian",
                  label        = "October - March (182 days)",
                  months       = c(10, 11, 12, 1, 2, 3),
                  months_label = "Oct, Nov, Dec, Jan, Feb, Mar",
                  season_days  = 182L,
                  note         = "Requires 12 monthly input layers for the R-factor formula."
                )
  )

  return(rec)
}
