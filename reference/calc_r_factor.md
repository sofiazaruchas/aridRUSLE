# Calculate Rainfall Erosivity (R-Factor)

Computes the RUSLE R-factor using the empirical formula appropriate for
the climate zone of the study area. The zone is detected automatically
from coordinates via get_climate_zone(), or can be set manually.

## Usage

``` r
calc_r_factor(
  precip_raster,
  lat = NULL,
  lon = NULL,
  climate_zone = NULL,
  season_days = 243,
  a = 0.171,
  b = 1.212,
  verbose = TRUE,
  plot = TRUE
)
```

## Arguments

- precip_raster:

  SpatRaster. Either 1 layer (cumulative seasonal precipitation in mm)
  or 12 layers (monthly, Jan to Dec). The required number of layers
  depends on the climate zone. The function throws an informative error
  if the wrong count is supplied.

- lat:

  Numeric. Latitude of the study area (decimal degrees). Required for
  automatic zone detection unless climate_zone is set manually.

- lon:

  Numeric. Longitude of the study area (decimal degrees).

- climate_zone:

  Character or NULL. Manual override of the climate zone. Valid values:
  "winter_rain_north", "winter_rain_south", "summer_monsoon",
  "hyperarid", "continental", "australian". If NULL (default), the zone
  is detected from lat/lon.

- season_days:

  Numeric. Length of the precipitation season in days. Only used for
  winter_rain_north / winter_rain_south (Bonilla & Vidal). Default: 243
  (Southern Hemisphere, Jan to Aug). Northern Hemisphere: 182.

- a:

  Numeric. Coefficient a for Bonilla & Vidal (2011). Only used for
  winter_rain\_\*. Default: 0.171.

- b:

  Numeric. Coefficient b for Bonilla & Vidal (2011). Only used for
  winter_rain\_\*. Default: 1.212.

- verbose:

  Logical. If TRUE, prints the detected zone, formula, parameters and
  recommended data period to the console. Default: TRUE.

- plot:

  Logical. If TRUE, displays a map of the R-factor after computation.
  Default: TRUE.

## Value

SpatRaster with one layer of R-factor values. The attribute attr(result,
"r_factor_meta") contains a list with: zone, formula_name, params (list
of parameters used), and season_recommendation.

## References

Bonilla & Vidal (2011). Rainfall erosivity in central Chile. Journal of
Hydrology, 410(1-2), 126-133. Arnoldus (1980). An approximation of the
rainfall factor in the USLE. In: De Boodt & Gabriels (eds.), Assessment
of Erosion. Wiley, 127-132. Yu & Rosewell (1996). A robust estimator of
the R-factor for the USLE. Transactions of the ASAE, 39(2), 559-561.

## Examples

``` r
if (FALSE) { # \dontrun{
  precip <- terra::rast("chirps_jan_aug.tif")
  r <- calc_r_factor(precip, lat = -33.0, lon = -71.0)

  # Manual zone override (skips get_climate_zone())
  r <- calc_r_factor(precip, climate_zone = "winter_rain_south",
                     season_days = 243)

  # Without plot
  r <- calc_r_factor(precip, lat = -33.0, lon = -71.0, plot = FALSE)
} # }
```
