# list_climate_zones.R
# Print a formatted overview of all five climate zones,
# their R-factor formulas, recommended periods, and input requirements.
#
# Zone definitions and Köppen mapping:
#   Peel, M.C., Finlayson, B.L., & McMahon, T.A. (2007). Updated world map
#   of the Koeppen-Geiger climate classification. Hydrology and Earth System
#   Sciences, 11, 1633-1644.
#
# R-factor formulas:
#   Bonilla, C.A., & Vidal, K.L. (2011). Rainfall erosivity in central
#   Chile. Journal of Hydrology, 410(1-2), 126-133.
#
#   Arnoldus, H.M.J. (1980). An approximation of the rainfall factor in the
#   Universal Soil Loss Equation. In: De Boodt & Gabriels (eds.),
#   Assessment of Erosion. Wiley, 127-132.
#
#   Yu, B., & Rosewell, C.J. (1996). A robust estimator of the R-factor for
#   the Universal Soil Loss Equation. Transactions of the ASAE, 39(2),
#   559-561.
#
# Recommended periods:
#   Derived from the rainfall seasonality and vegetation phenology of each
#   climate zone, consistent with get_season_recommendation.R.



#' List all available climate zones with formulas and recommended periods
#'
#' Prints a formatted overview of all five climate zones supported by
#' aridRUSLE: zone ID, Koppen-Geiger code, R-factor formula, recommended
#' aggregation period, and required input structure (single cumulative layer
#' vs. 12 monthly layers).
#'
#' Useful as a quick reference before calling \code{calc_r_factor()} or
#' \code{get_season_recommendation()}.
#'
#' No parameters. Returns a \code{data.frame} invisibly.
#'
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{zone}{Climate zone ID (character)}
#'     \item{koppen}{Koppen-Geiger code(s)}
#'     \item{formula}{R-factor formula name}
#'     \item{period}{Recommended aggregation period label}
#'     \item{season_days}{Number of days in the recommended period (integer)}
#'     \item{input_layers}{Required number of input layers (1 or 12)}
#'   }
#'
#' @references
#' Peel, M.C., Finlayson, B.L., & McMahon, T.A. (2007). Updated world map of
#' the Koeppen-Geiger climate classification. \emph{Hydrology and Earth System
#' Sciences}, 11, 1633-1644.
#'
#' Bonilla, C.A., & Vidal, K.L. (2011). Rainfall erosivity in central Chile.
#' \emph{Journal of Hydrology}, 410(1-2), 126-133.
#'
#' Arnoldus, H.M.J. (1980). An approximation of the rainfall factor in the
#' Universal Soil Loss Equation. In: De Boodt & Gabriels (eds.),
#' \emph{Assessment of Erosion}. Wiley, 127-132.
#'
#' Yu, B., & Rosewell, C.J. (1996). A robust estimator of the R-factor for
#' the Universal Soil Loss Equation. \emph{Transactions of the ASAE},
#' 39(2), 559-561.
#'
#' @export
#'
#' @examples
#' list_climate_zones()

list_climate_zones <- function() {

  zones <- data.frame(
    zone         = c("winter_rain", "summer_monsoon", "hyperarid",
                     "continental", "australian"),
    koppen       = c("Cs (Mediterranean, fallback)",
                     "BSh (Sahel, India)",
                     "BWh / BWk (desert core)",
                     "BSk (steppe)",
                     "BSh/BSk/BWh/BWk (Australia)"),
    formula      = c("Bonilla & Vidal (2011)",
                     "Modified Fournier Index / Arnoldus (1980)",
                     "Simplified MFI with aridity correction",
                     "Arnoldus formula (1980), steppe adaptation",
                     "Yu & Rosewell adaptation (1996)"),
    period       = c("January - August (243 days)",
                     "June - September (122 days)",
                     "Full year - January to December (365 days)",
                     "April - September (183 days)",
                     "October - March (182 days)"),
    season_days  = c(243L, 122L, 365L, 183L, 182L),
    input_layers = c(1L, 12L, 1L, 12L, 12L),
    stringsAsFactors = FALSE
  )

  # --- Console output ---
  message("=================================================================")
  message("aridRUSLE - Available Climate Zones")
  message("=================================================================")

  for (i in seq_len(nrow(zones))) {
    z <- zones[i, ]
    message("")
    message("Zone          : ", z$zone)
    message("Koppen        : ", z$koppen)
    message("Formula       : ", z$formula)
    message("Period        : ", z$period)
    message("Input layers  : ", z$input_layers,
            if (z$input_layers == 1) " (cumulative)" else " (monthly, Jan-Dec)")
  }

  message("")
  message("=================================================================")
  message("Tip: get_season_recommendation(zone) returns the full period")
  message("     including month vectors.")
  message("=================================================================")

  invisible(zones)
}
