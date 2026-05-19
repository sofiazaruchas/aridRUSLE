# Calculate Vegetation Cover Factor (C-Factor)

Computes the RUSLE C-factor (vegetation cover and management factor)
from a water-masked NDVI composite using the exponential relationship
proposed by Mahgoub et al. (2012). The C-factor ranges from 0 (dense
vegetation, no erosion) to 1 (bare soil, maximum erosion).

## Usage

``` r
calc_c_factor(ndvi_raster, verbose = TRUE, plot = TRUE)
```

## Arguments

- ndvi_raster:

  SpatRaster. NDVI composite with values in the range from -1 to 1.
  Should be water-masked beforehand using
  [`apply_water_mask()`](https://sofiazaruchas.github.io/aridRUSLE/reference/apply_water_mask.md).
  Water pixels (NA) are preserved as NA in the output.

- verbose:

  Logical. If TRUE, prints a summary of the computed C-factor (min,
  mean, max) to the console. Default: TRUE.

- plot:

  Logical. If TRUE, displays a map of the C-factor after computation.
  Default: TRUE.

## Value

SpatRaster with one layer of C-factor values in the range from 0 to 1
(dimensionless), with the same extent, resolution and CRS as
`ndvi_raster`. Water pixels are NA. The layer is named `"C_factor"`.

## Details

The C-factor is calculated as: \$\$C = 0.353 \cdot e^{1.669 \cdot
NDVI}\$\$

Output values are clamped to the range from 0 to 1. Water pixels (NA in
the input) remain NA in the output and propagate automatically into the
final erosion risk index computed by
[`calc_erosion_risk()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_erosion_risk.md).

It is strongly recommended to apply
[`apply_water_mask()`](https://sofiazaruchas.github.io/aridRUSLE/reference/apply_water_mask.md)
to the NDVI composite before passing it to this function, as open water
surfaces produce misleading NDVI values that would otherwise bias the
C-factor.

## References

Mahgoub, M. et al. (2012). Estimation of soil loss from semi-arid area
using RUSLE model and remote sensing. *CATENA*, 100, 126-133.

## Examples

``` r
if (FALSE) { # \dontrun{
  ndvi <- terra::rast("s2_ndvi.tif")
  s2   <- terra::rast("s2_bands.tif")

  # Step 1: mask water pixels
  ndvi_masked <- apply_water_mask(ndvi, s2)

  # Step 2: compute C-factor from masked NDVI
  c_factor <- calc_c_factor(ndvi_masked)

  # Without plot
  c_factor <- calc_c_factor(ndvi_masked, plot = FALSE)
} # }
```
