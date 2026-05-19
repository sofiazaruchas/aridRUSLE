# Combine factors into an Erosion Risk Index (ERI)

Normalises R, LS and C to 0-1 and multiplies them pixel-wise to produce
the Erosion Risk Index (ERI). If the rasters do not share the same
geometry (extent, resolution, CRS), they are automatically reprojected
and resampled to match `ls_factor` (used as reference grid).

## Usage

``` r
calc_erosion_risk(
  r_factor,
  ls_factor,
  c_factor,
  normalize = TRUE,
  resample_method = "bilinear",
  plot = TRUE,
  map_title = "Soil Erosivity",
  figure_caption = NULL,
  data_source = NULL
)
```

## Arguments

- r_factor:

  SpatRaster. Output of
  [`calc_r_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_r_factor.md).

- ls_factor:

  SpatRaster. Output of
  [`calc_ls_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_ls_factor.md).
  Used as the reference grid for alignment.

- c_factor:

  SpatRaster. Output of
  [`calc_c_factor()`](https://sofiazaruchas.github.io/aridRUSLE/reference/calc_c_factor.md).

- normalize:

  Logical. If `TRUE` (default), each factor is min-max normalised to 0-1
  before multiplication.

- resample_method:

  Character. Resampling method passed to
  [`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html).
  Default: `"bilinear"`.

- plot:

  Logical. If `TRUE` (default), displays a publication-style map of the
  ERI.

- map_title:

  Character. Title printed above the map. Default: `"Soil Erosivity"`.

- figure_caption:

  Character. Figure caption printed below the map as a plot subtitle.
  Default: `NULL` (no caption).

- data_source:

  Character. Data-source string printed below the map in small type.
  Default: `NULL` (no source line).

## Value

Invisibly returns a list with two elements:

- `eri`:

  SpatRaster (0-1) with Erosion Risk Index values, named `"ERI"`.

- `map`:

  A `ggplot` object, or `NULL` if `plot = FALSE`.

## Details

The ERI is calculated as: \$\$ERI = R\_{norm} \times LS\_{norm} \times
C\_{norm}\$\$

where each factor is min-max normalised to \\\[0, 1\]\\ before
multiplication. Higher ERI values indicate greater erosion hazard.

If `plot = TRUE`, the ERI is displayed in a publication-quality
cartographic layout built with ggplot2, tidyterra and ggspatial:
cream-to-brown colour ramp, coordinate grid with degree labels, north
arrow, scale bar, continuous legend, and optional title, figure caption
and data source line.

## References

Mahgoub, M. et al. (2012). Estimation of soil loss from semi-arid area
using RUSLE model and remote sensing. *CATENA*, 100, 126-133.

## Examples

``` r
if (FALSE) { # \dontrun{
r  <- calc_r_factor(precip, lat = -33.0, lon = -71.0)
ls <- calc_ls_factor(dem)
c  <- calc_c_factor(ndvi_masked)

# ERI with full publication layout
result <- calc_erosion_risk(r, ls, c,
  map_title      = "Soil Erosivity for Elqui Valley, Coquimbo Region",
  figure_caption = "Figure 4: Map of Erosivity Index",
  data_source    = "Source: NASA JPL (2013). SRTM Global 1 arc second.")

# Save the map
ggplot2::ggsave("erosion_risk_map.png", result$map,
                width = 250, height = 200, units = "mm", dpi = 300)

# Access only the raster
eri_raster <- result$eri

# Without plot
result <- calc_erosion_risk(r, ls, c, plot = FALSE)
} # }
```
