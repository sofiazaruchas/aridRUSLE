# List all available climate zones with formulas and recommended periods

Prints a formatted overview of all five climate zones supported by
aridRUSLE: zone ID, Koppen-Geiger code, R-factor formula, recommended
aggregation period, and required input structure (single cumulative
layer vs. 12 monthly layers).

## Usage

``` r
list_climate_zones()
```

## Value

A `data.frame` with columns:

- zone:

  Climate zone ID (character)

- koppen:

  Koppen-Geiger code(s)

- formula:

  R-factor formula name

- period:

  Recommended aggregation period label

- season_days:

  Number of days in the recommended period (integer)

- input_layers:

  Required number of input layers (1 or 12)

## Details

Useful as a quick reference before calling
[`calc_r_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_r_factor.md)
or
[`get_season_recommendation()`](https://sofiazaruchas.github.io/aridRUSLE/reference/get_season_recommendation.md).

No parameters. Returns a `data.frame` invisibly.

## References

Peel, M.C., Finlayson, B.L., & McMahon, T.A. (2007). Updated world map
of the Koeppen-Geiger climate classification. *Hydrology and Earth
System Sciences*, 11, 1633-1644.

Bonilla, C.A., & Vidal, K.L. (2011). Rainfall erosivity in central
Chile. *Journal of Hydrology*, 410(1-2), 126-133.

Arnoldus, H.M.J. (1980). An approximation of the rainfall factor in the
Universal Soil Loss Equation. In: De Boodt & Gabriels (eds.),
*Assessment of Erosion*. Wiley, 127-132.

Yu, B., & Rosewell, C.J. (1996). A robust estimator of the R-factor for
the Universal Soil Loss Equation. *Transactions of the ASAE*, 39(2),
559-561.

## Examples

``` r
list_climate_zones()
#> =================================================================
#> aridRUSLE - Available Climate Zones
#> =================================================================
#> 
#> Zone          : winter_rain_north
#> Koppen        : Cs (Mediterranean, northern hemisphere)
#> Formula       : Bonilla & Vidal (2011)
#> Period        : October - March (182 days)
#> Input layers  : 1 (cumulative)
#> 
#> Zone          : winter_rain_south
#> Koppen        : Cs (Mediterranean, southern hemisphere)
#> Formula       : Bonilla & Vidal (2011)
#> Period        : January - August (243 days)
#> Input layers  : 1 (cumulative)
#> 
#> Zone          : summer_monsoon
#> Koppen        : BSh (Sahel, India)
#> Formula       : Modified Fournier Index / Arnoldus (1980)
#> Period        : June - September (122 days)
#> Input layers  : 12 (monthly, Jan-Dec)
#> 
#> Zone          : hyperarid
#> Koppen        : BWh / BWk (desert core)
#> Formula       : Simplified MFI with aridity correction
#> Period        : Full year - January to December (365 days)
#> Input layers  : 1 (cumulative)
#> 
#> Zone          : continental
#> Koppen        : BSk (steppe)
#> Formula       : Arnoldus formula (1980), steppe adaptation
#> Period        : April - September (183 days)
#> Input layers  : 12 (monthly, Jan-Dec)
#> 
#> Zone          : australian
#> Koppen        : BSh/BSk/BWh/BWk (Australia)
#> Formula       : Yu & Rosewell adaptation (1996)
#> Period        : October - March (182 days)
#> Input layers  : 12 (monthly, Jan-Dec)
#> 
#> =================================================================
#> Tip: get_season_recommendation(zone) returns the full period
#>      including month vectors.
#> =================================================================
```
