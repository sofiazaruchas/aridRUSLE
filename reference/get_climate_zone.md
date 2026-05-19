# Determine climate zone from coordinates

Classifies a coordinate into one of five climate categories using the
Koeppen-Geiger climate classification system (Peel et al., 2007),
accessed via the kgc package (Bryant et al., 2017). No raster download
required - the classification table is bundled inside the kgc package.

## Usage

``` r
get_climate_zone(lat, lon, verbose = TRUE)
```

## Arguments

- lat:

  Numeric. Latitude of the study area (decimal degrees).

- lon:

  Numeric. Longitude of the study area (decimal degrees).

- verbose:

  Logical. Print detected zone, Koeppen code, and recommended period to
  the console. Default: `TRUE`.

## Value

Character string - one of five zones: `"winter_rain"`,
`"summer_monsoon"`, `"hyperarid"`, `"continental"`, `"australian"`

## Details

Prints the detected zone, the underlying Koeppen-Geiger code, and the
recommended data period to the console. Used internally by
[`calc_r_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_r_factor.md),
but can also be called directly.

## References

Peel, M.C., Finlayson, B.L., & McMahon, T.A. (2007). Updated world map
of the Koeppen-Geiger climate classification. *Hydrology and Earth
System Sciences*, 11, 1633-1644.

Bryant, C., Gruson, H., & Gaborit, P. (2017). kgc: Koeppen-Geiger
Climatic Zones. R package version 1.0.0.2.
<https://CRAN.R-project.org/package=kgc>

## Examples

``` r
get_climate_zone(lat = -29.9, lon = -70.8)   # Chile         -> winter_rain
#> -----------------------------------------
#> Detected zone      : hyperarid
#> Koeppen-Geiger code: BWk (Peel et al. 2007, via kgc)
#> Recommended period : Full year - January to December (365 days) (365 days)
#> Recommended months : Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
#> Note               : Precipitation in hyper-arid zones is highly episodic and not seasonal. The full year is recommended to avoid missing rare extreme events.
#> -----------------------------------------
#> [1] "hyperarid"
get_climate_zone(lat =  13.5, lon =   2.1)   # Sahel         -> summer_monsoon
#> -----------------------------------------
#> Detected zone      : summer_monsoon
#> Koeppen-Geiger code: BSh (Peel et al. 2007, via kgc)
#> Recommended period : June - September (122 days) (122 days)
#> Recommended months : Jun, Jul, Aug, Sep
#> Note               : Requires 12 monthly input layers for the R-factor formula.
#> -----------------------------------------
#> [1] "summer_monsoon"
get_climate_zone(lat =  23.0, lon =  45.0)   # Arabia        -> hyperarid
#> -----------------------------------------
#> Detected zone      : hyperarid
#> Koeppen-Geiger code: BWh (Peel et al. 2007, via kgc)
#> Recommended period : Full year - January to December (365 days) (365 days)
#> Recommended months : Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
#> Note               : Precipitation in hyper-arid zones is highly episodic and not seasonal. The full year is recommended to avoid missing rare extreme events.
#> -----------------------------------------
#> [1] "hyperarid"
get_climate_zone(lat =  45.0, lon =  65.0)   # Central Asia  -> continental
#> -----------------------------------------
#> Detected zone      : hyperarid
#> Koeppen-Geiger code: BWk (Peel et al. 2007, via kgc)
#> Recommended period : Full year - January to December (365 days) (365 days)
#> Recommended months : Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
#> Note               : Precipitation in hyper-arid zones is highly episodic and not seasonal. The full year is recommended to avoid missing rare extreme events.
#> -----------------------------------------
#> [1] "hyperarid"
get_climate_zone(lat = -25.0, lon = 134.0)   # Australia     -> australian
#> -----------------------------------------
#> Detected zone      : australian
#> Koeppen-Geiger code: BWh (Peel et al. 2007, via kgc)
#> Recommended period : October - March (182 days) (182 days)
#> Recommended months : Oct, Nov, Dec, Jan, Feb, Mar
#> Note               : Requires 12 monthly input layers for the R-factor formula.
#> -----------------------------------------
#> [1] "australian"
```
