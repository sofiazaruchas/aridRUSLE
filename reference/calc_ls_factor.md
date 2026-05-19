# Calculate Topographic LS-Factor

Computes the RUSLE LS-factor (slope length x slope steepness) from a
digital elevation model (DEM) using the Moore & Burch (1986) approach.

## Usage

``` r
calc_ls_factor(dem, verbose = TRUE, plot = TRUE)
```

## Arguments

- dem:

  SpatRaster. Digital elevation model with elevation values in metres.
  If in a geographic CRS (degrees), it is automatically reprojected to
  the appropriate UTM zone. A missing CRS will trigger an error.

- verbose:

  Logical. If TRUE, prints a summary of the computed LS-factor (min,
  mean, max) and the cell size used to the console. Default: TRUE.

- plot:

  Logical. If TRUE, displays a map of the LS-factor after computation.
  Default: TRUE.

## Value

SpatRaster with one layer of LS-factor values (dimensionless, values
clamped to 0 or above), with the same extent and CRS as the (possibly
reprojected) input DEM. The layer is named `"LS_factor"`.

## Details

The LS-factor is calculated as: \$\$LS =
\left(\frac{A}{22.13}\right)^{0.4} \cdot
\left(\frac{\sin(\beta)}{0.0896}\right)^{1.3}\$\$

where \\A\\ is the cell size in metres and \\\beta\\ is the slope in
radians.

If the DEM is in a geographic CRS (degrees), it is automatically
reprojected to the appropriate UTM zone before computation.

## References

Moore, I.D., & Burch, G.J. (1986). Physical basis of the length-slope
factor in the Universal Soil Loss Equation. *Soil Science Society of
America Journal*, 50(5), 1294-1298.

## Examples

``` r
if (FALSE) { # \dontrun{
  dem <- terra::rast("srtm.tif")
  ls  <- calc_ls_factor(dem)

  # Without plot
  ls  <- calc_ls_factor(dem, plot = FALSE)
} # }
```
