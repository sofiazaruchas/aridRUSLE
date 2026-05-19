# aridRUSLE
aridRUSLE estimates soil erosion risk in semi-arid and arid regions worldwide by computing the three remotely-sensed RUSLE factors R, LS, and C from freely available satellite and climate data

##overview
aridRUSLE is an R package implementing a modified RUSLE (Revised Universal 
Soil Loss Equation) approach for soil erosion risk analysis in semi-arid and
arid regions worldwide. Rather than requiring all five classical RUSLE factors, 
the package focuses on three key factors derivable from easy accessible 
remote sensing and climate datasets: rainfall erosivity (R), topographic 
influence (LS), and vegetation cover (C). Soil erodibility (K) and 
conservation practices (P) are deliberately omitted due to typical data 
limitations in semi-arid research areas.

A core design feature of aridRUSLE is its global applicability. The R-factor 
module supports five different empirical formulas, each calibrated for a 
different semi-arid and arid climate regime. The appropriate formula is automatically 
selected based on the geographic coordinates of the study area using a 
Köppen-like climate zone classification or can be set manually. This makes 
the package readily applicable across the Mediterranean, Sahel, Arabian 
Peninsula, Central Asia, Australia, and South America without any manual 
formula lookup.

## Dependencies
aridRUSLE requires the following R packages, which are installed automatically:
`terra`, `ggplot2`, `tidyterra`, `ggspatial`, `kgc`

## Installation

```r
# install.packages("devtools")
devtools::install_github("sofiazaruchas/aridRUSLE")
```
## Data Requirements

The required input data are summarized in Table 1. The aggregation period 
for precipitation, NDVI, and satellite imagery (used for NDWI computation) 
depends on the climate zone of the study area and should correspond to the 
respective rainy season (see Table 2). The package automatically prints the 
recommended period at each call of `calc_r_factor()`.

![Table 1](man/figures/table_requirements.png)
