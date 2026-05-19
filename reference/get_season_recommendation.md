# Get recommended data aggregation period for a climate zone

Returns the recommended aggregation period for precipitation (CHIRPS),
NDVI, and NDWI for a given climate zone. All three input datasets must
always cover the same period, corresponding to the rainy season of the
respective climate zone.

## Usage

``` r
get_season_recommendation(zone)
```

## Arguments

- zone:

  Character string. One of five climate zone IDs: `"winter_rain"`,
  `"summer_monsoon"`, `"hyperarid"`, `"continental"`, `"australian"`.

## Value

A named list with the following elements:

- zone:

  Climate zone ID (character)

- label:

  Human-readable period label, e.g. `"January - August (243 days)"`

- months:

  Integer vector of recommended months, e.g. `c(1,2,3,4,5,6,7,8)`

- months_label:

  Comma-separated month abbreviations, e.g. `"Jan, Feb, Mar, ..."`

- season_days:

  Number of days in the recommended period (integer)

- note:

  Additional guidance note (character, may be empty)

## Details

Can be called before downloading data to determine the correct period.
Also used internally by
[`get_climate_zone()`](https://sofiazaruchas.github.io/aridRUSLE/reference/get_climate_zone.md)
and
[`calc_r_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_r_factor.md).

## Examples

``` r
get_season_recommendation("winter_rain_north")
#> $zone
#> [1] "winter_rain_north"
#> 
#> $label
#> [1] "October - March (182 days)"
#> 
#> $months
#> [1] 10 11 12  1  2  3
#> 
#> $months_label
#> [1] "Oct, Nov, Dec, Jan, Feb, Mar"
#> 
#> $season_days
#> [1] 182
#> 
#> $note
#> [1] "Northern hemisphere Mediterranean climate (e.g. Morocco, Tunisia, S-Iberia)."
#> 
get_season_recommendation("winter_rain_south")
#> $zone
#> [1] "winter_rain_south"
#> 
#> $label
#> [1] "January - August (243 days)"
#> 
#> $months
#> [1] 1 2 3 4 5 6 7 8
#> 
#> $months_label
#> [1] "Jan, Feb, Mar, Apr, May, Jun, Jul, Aug"
#> 
#> $season_days
#> [1] 243
#> 
#> $note
#> [1] "Southern hemisphere Mediterranean climate (e.g. central Chile)."
#> 
get_season_recommendation("summer_monsoon")
#> $zone
#> [1] "summer_monsoon"
#> 
#> $label
#> [1] "June - September (122 days)"
#> 
#> $months
#> [1] 6 7 8 9
#> 
#> $months_label
#> [1] "Jun, Jul, Aug, Sep"
#> 
#> $season_days
#> [1] 122
#> 
#> $note
#> [1] "Requires 12 monthly input layers for the R-factor formula."
#> 
get_season_recommendation("hyperarid")
#> $zone
#> [1] "hyperarid"
#> 
#> $label
#> [1] "Full year - January to December (365 days)"
#> 
#> $months
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12
#> 
#> $months_label
#> [1] "Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec"
#> 
#> $season_days
#> [1] 365
#> 
#> $note
#> [1] "Precipitation in hyper-arid zones is highly episodic and not seasonal. The full year is recommended to avoid missing rare extreme events."
#> 
get_season_recommendation("continental")
#> $zone
#> [1] "continental"
#> 
#> $label
#> [1] "April - September (183 days)"
#> 
#> $months
#> [1] 4 5 6 7 8 9
#> 
#> $months_label
#> [1] "Apr, May, Jun, Jul, Aug, Sep"
#> 
#> $season_days
#> [1] 183
#> 
#> $note
#> [1] "Requires 12 monthly input layers for the R-factor formula."
#> 
get_season_recommendation("australian")
#> $zone
#> [1] "australian"
#> 
#> $label
#> [1] "October - March (182 days)"
#> 
#> $months
#> [1] 10 11 12  1  2  3
#> 
#> $months_label
#> [1] "Oct, Nov, Dec, Jan, Feb, Mar"
#> 
#> $season_days
#> [1] 182
#> 
#> $note
#> [1] "Requires 12 monthly input layers for the R-factor formula."
#> 
```
