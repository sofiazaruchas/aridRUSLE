# Mask water pixels using an NDWI threshold

Masks water pixels in a target raster (typically an NDVI composite)
based on NDWI values computed internally from a multi-band satelite
image. Required preprocessing step before
[`calc_c_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_c_factor.md),
as open water surfaces produce misleading NDVI values.

## Usage

``` r
apply_water_mask(
  target_raster,
  satelite_raster,
  green_band = 3,
  nir_band = 8,
  threshold = 0.3,
  smooth = TRUE,
  smooth_w = 3,
  min_water_ha = 1,
  return_ndwi = FALSE,
  plot = TRUE
)
```

## Arguments

- target_raster:

  SpatRaster. The raster to be masked (e.g. NDVI composite). All layers
  are masked simultaneously.

- satelite_raster:

  SpatRaster. Multi-band satelite image containing at least the green
  and NIR bands. Used to compute NDWI internally.

- green_band:

  Integer. Band index of the green band in `satelite_raster`. Default:
  `3` (Sentinel-2 Band 3). Use `3` for standard Sentinel-2 multiband
  images. Adjust if using a False Colour Composite or a different
  sensor.

- nir_band:

  Integer. Band index of the NIR band in `satelite_raster`. Default: `8`
  (Sentinel-2 Band 8). Use `1` if working with a Sentinel-2 False Colour
  Composite (RGB = NIR / Red / Green).

- threshold:

  Numeric. NDWI threshold above which pixels are classified as water and
  set to `NA` in the output. Default: `0.3` (McFeeters 1996).

- smooth:

  Logical. If `TRUE`, applies a mean filter to the NDWI raster before
  thresholding to reduce isolated resampling artefacts at water-land
  boundaries. Window size is controlled by `smooth_w`. Default: `TRUE`.

- smooth_w:

  Integer. Window size for the smoothing filter (must be an odd integer
  \>= 3). Only used when `smooth = TRUE`. Default: `3`.

- min_water_ha:

  Numeric or NULL. Minimum area in hectares for a connected water patch
  to be masked. Patches smaller than this threshold are kept as land.
  Converted to pixels automatically based on the resolution of
  `satelite_raster`. If `NULL`, all pixels above `threshold` are masked
  regardless of patch size. Recommended: `1` (FAO standard) for
  semi-arid regions.

- return_ndwi:

  Logical. If `TRUE`, the internally computed NDWI raster (after
  smoothing, if applicable) is stored as an attribute
  (`attr(result, "ndwi")`) on the returned object. Default: `FALSE`.

- plot:

  Logical. If `TRUE` (default), displays the masked raster (first layer)
  with water pixels shown in steelblue. Useful for visually inspecting
  the mask and adjusting `threshold` or `min_water_ha` if needed.

## Value

SpatRaster with the same dimensions and CRS as `target_raster`. Water
pixels are set to `NA`, all other pixels are unchanged. If
`return_ndwi = TRUE`, the computed NDWI raster is accessible via
`attr(result, "ndwi")`.

## Details

The NDWI is calculated as: \$\$NDWI = \frac{Green - NIR}{Green +
NIR}\$\$

Band indices default to standard Sentinel-2 band numbering
(`green_band = 3`, `nir_band = 8`). Adjust these if using a different
sensor or band ordering (e.g. a False Colour Composite where NIR is
stored as band 1).

If `target_raster` and `satelite_raster` do not share the same geometry
(extent, resolution, CRS), `satelite_raster` is automatically resampled
to match `target_raster` using nearest-neighbour interpolation before
masking.

If `smooth = TRUE`, the NDWI raster is spatially smoothed with a mean
filter before thresholding. This reduces isolated salt-and-pepper pixels
caused by resampling artefacts at water-land boundaries.

If `min_water_ha` is set, only connected water patches with an area of
at least `min_water_ha` hectares are masked. This removes isolated
single pixels and small artefacts, retaining only true water bodies such
as lakes and reservoirs. The minimum area is converted to pixels
automatically based on the resolution of `satelite_raster`, making the
threshold resolution-independent and scientifically justifiable.
Recommended values based on the literature:

- `0.1` ha — EU Water Framework Directive minimum mapping unit

- `1.0` ha — FAO Irrigation & Drainage standard

- `4.0` ha — typical for Landsat-based (30 m) studies

Setting `plot = TRUE` displays the masked result so the user can
visually inspect the water mask and adjust `threshold` or `min_water_ha`
if needed before proceeding to
[`calc_c_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_c_factor.md).

## References

McFeeters, S.K. (1996). The use of the Normalised Difference Water Index
(NDWI) in the delineation of open water features. *International Journal
of Remote Sensing*, 17(7), 1425-1432.

## Examples

``` r
if (FALSE) { # \dontrun{
ndvi <- terra::rast("s2_ndvi.tif")
s2   <- terra::rast("s2_bands.tif")  # standard multi-band Sentinel-2 image

# Mask water pixels with default settings (Sentinel-2 bands, smooth = TRUE)
ndvi_masked <- apply_water_mask(ndvi, s2)

# Only mask water bodies >= 1 ha (FAO standard)
ndvi_masked <- apply_water_mask(ndvi, s2, min_water_ha = 1)

# False Colour Composite (NIR stored as band 1)
s2_fcc      <- terra::rast("sentinel_false_colour.tif")
ndvi_masked <- apply_water_mask(ndvi, s2_fcc, green_band = 3, nir_band = 1,
                                min_water_ha = 1)

# Inspect the result visually before proceeding
ndvi_masked <- apply_water_mask(ndvi, s2, min_water_ha = 1, plot = TRUE)

# Disable smoothing and patch filtering
ndvi_masked <- apply_water_mask(ndvi, s2, smooth = FALSE,
                                min_water_ha = NULL)

# Retrieve the internally computed NDWI raster
ndvi_masked <- apply_water_mask(ndvi, s2, return_ndwi = TRUE)
ndwi        <- attr(ndvi_masked, "ndwi")
} # }
```
