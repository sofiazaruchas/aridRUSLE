# calc_erosion_risk.R
# Combine R, LS, and C factors into an Erosion Risk Index (ERI).
# ERI = R_norm x LS_norm x C_norm  (each factor normalised to 0-1)
#
# Requires: terra (>= 1.7-0), tmap (>= 4.0), sf

#' Combine factors into an Erosion Risk Index (ERI)
#'
#' Normalises R, LS and C to 0-1 and multiplies them pixel-wise to produce
#' the Erosion Risk Index (ERI). If the rasters do not share the same geometry
#' (extent, resolution, CRS), they are automatically reprojected and resampled
#' to match \code{ls_factor} (used as reference grid).
#'
#' The ERI is calculated as:
#' \deqn{ERI = R_{norm} \times LS_{norm} \times C_{norm}}
#'
#' where each factor is min-max normalised to \eqn{[0, 1]} before
#' multiplication. Higher ERI values indicate greater erosion hazard.
#'
#' If \code{plot = TRUE}, the ERI is displayed in a publication-quality
#' cartographic layout matching the aridRUSLE house style:
#' \itemize{
#'   \item Main map: ERI overlay on hillshade background (if \code{dem}
#'         supplied), axes with coordinate grid, cream-to-brown colour ramp.
#'   \item Legend panel: gradient colour bar for the Erosivity Index and,
#'         when a DEM is supplied, a separate bar for the Hillshade.
#'   \item Cartographic furniture: north arrow (top-right of main map),
#'         scale bar (bottom-left of main map).
#'   \item Source line and figure caption below the map frame.
#' }
#'
#' The map is rendered with \pkg{tmap} (>= 4.0) in \code{"plot"} mode and
#' returned invisibly as a \code{tmap} object so that it can be saved with
#' \code{tmap::tmap_save()}.
#'
#' @param r_factor  SpatRaster. Output of \code{calc_r_factor()}.
#' @param ls_factor SpatRaster. Output of \code{calc_ls_factor()}.
#'   Used as the reference grid for alignment.
#' @param c_factor  SpatRaster. Output of \code{calc_c_factor()}.
#' @param dem       SpatRaster or NULL. Digital Elevation Model used to
#'   compute hillshade for the map background.  If \code{NULL} (default),
#'   the ERI is plotted on a white / light-grey background without hillshade.
#' @param normalize Logical. If \code{TRUE} (default), each factor is
#'   min-max normalised to 0-1 before multiplication.
#' @param resample_method Character. Resampling method passed to
#'   \code{terra::resample()}. Default: \code{"bilinear"}.
#' @param plot Logical. If \code{TRUE} (default), displays a
#'   publication-style map of the ERI.
#' @param map_title Character. Title printed at the top of the map frame.
#'   Default: \code{"Soil Erosivity"}.
#' @param figure_caption Character. Figure caption printed below the map
#'   frame (e.g. \code{"Figure 4: Map of Erosivity Index"}).
#'   Default: \code{NULL} (no caption).
#' @param data_source Character. Data-source string printed in small type
#'   below the map frame (e.g. NASA JPL / Sentinel-2 credits).
#'   Default: \code{NULL} (no source line).
#'
#' @return Invisibly returns a list with two elements:
#' \describe{
#'   \item{\code{eri}}{SpatRaster (0-1) with Erosion Risk Index values,
#'     named \code{"ERI"}.}
#'   \item{\code{map}}{A \code{tmap} object (or \code{NULL} if
#'     \code{plot = FALSE}).}
#' }
#'
#' @references
#' Mahgoub, M. et al. (2012). Estimation of soil loss from semi-arid area
#' using RUSLE model and remote sensing. \emph{CATENA}, 100, 126-133.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' r   <- calc_r_factor(precip, lat = -33.0, lon = -71.0)
#' ls  <- calc_ls_factor(dem)
#' c   <- calc_c_factor(ndvi_masked)
#' dem <- terra::rast("srtm.tif")
#'
#' # ERI with full publication layout
#' result <- calc_erosion_risk(r, ls, c, dem = dem,
#'   map_title      = "Soil Erosivity for Elqui Valley, Coquimbo Region",
#'   figure_caption = "Figure 4: Map of Erosivity Index",
#'   data_source    = paste0(
#'     "Source: NASA JPL. (2013). NASA Shuttle Radar Topography Mission ",
#'     "(SRTM) Global 1 arc second (30 m). NASA EOSDIS Land Processes ",
#'     "Distributed Active Archive Center (LP DAAC)"))
#'
#' # Save the map to a file
#' tmap::tmap_save(result$map, "erosion_risk_map.png",
#'                 width = 250, height = 200, units = "mm", dpi = 300)
#'
#' # Access only the raster
#' eri_raster <- result$eri
#' }

calc_erosion_risk <- function(r_factor, ls_factor, c_factor,
                              dem             = NULL,
                              normalize       = TRUE,
                              resample_method = "bilinear",
                              plot            = TRUE,
                              map_title       = "Soil Erosivity",
                              figure_caption  = NULL,
                              data_source     = NULL) {

  # Input validation

  if (!inherits(r_factor,  "SpatRaster"))
    stop("'r_factor' must be a SpatRaster object.")
  if (!inherits(ls_factor, "SpatRaster"))
    stop("'ls_factor' must be a SpatRaster object.")
  if (!inherits(c_factor,  "SpatRaster"))
    stop("'c_factor' must be a SpatRaster object.")
  if (!is.null(dem) && !inherits(dem, "SpatRaster"))
    stop("'dem' must be a SpatRaster object or NULL.")
  if (!is.logical(normalize) || length(normalize) != 1)
    stop("'normalize' must be a single logical value (TRUE or FALSE).")
  if (!is.logical(plot) || length(plot) != 1)
    stop("'plot' must be a single logical value (TRUE or FALSE).")
  if (!resample_method %in% c("near", "bilinear", "cubic", "cubicspline",
                              "lanczos", "sum", "mode", "max", "min",
                              "med", "q1", "q3", "rms"))
    stop("'resample_method' is not a recognised terra resampling method.")

  # Package checks

  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' (>= 1.7-0) is required.")
  if (plot) {
    if (!requireNamespace("tmap", quietly = TRUE))
      stop("Package 'tmap' (>= 4.0) is required for plot = TRUE.")
    if (!requireNamespace("sf",   quietly = TRUE))
      stop("Package 'sf' is required for plot = TRUE.")
    tmap_ver <- utils::packageVersion("tmap")
    if (tmap_ver < "4.0")
      warning("tmap >= 4.0 is recommended. Some layout features may differ ",
              "with version ", tmap_ver, ".")
  }


  # Geometry helpers

  geom_match <- function(a, b) {
    isTRUE(tryCatch(
      terra::compareGeom(a, b, res = TRUE, stopOnError = TRUE),
      error = function(e) FALSE
    ))
  }

  align <- function(x, ref, name) {
    if (!terra::same.crs(x, ref)) {
      message("Geometry mismatch detected: reprojecting '", name,
              "' to match 'ls_factor'.")
      x <- terra::project(x, ref)
    }
    if (!geom_match(x, ref)) {
      message("Geometry mismatch detected: resampling '", name,
              "' to match 'ls_factor' (method = '", resample_method, "').")
      x <- terra::resample(x, ref, method = resample_method)
    }
    x
  }

  r_factor <- align(r_factor, ls_factor, "r_factor")
  c_factor <- align(c_factor, ls_factor, "c_factor")

  # Optional min-max normalisation

  norm01 <- function(x) {
    mn <- terra::global(x, "min", na.rm = TRUE)[[1]]
    mx <- terra::global(x, "max", na.rm = TRUE)[[1]]
    if (is.na(mn) || is.na(mx) || mx == mn) return(x * 0)
    (x - mn) / (mx - mn)
  }

  if (normalize) {
    r  <- norm01(r_factor)
    ls <- norm01(ls_factor)
    c  <- norm01(c_factor)
  } else {
    r  <- r_factor
    ls <- ls_factor
    c  <- c_factor
  }

  # Compute ERI

  eri        <- r * ls * c
  names(eri) <- "ERI"

  message("ERI computed as R_norm x LS_norm x C_norm. ",
          "Pass result$eri to classify_erosion_risk() for risk classification.")

  # Plot

  map_obj <- NULL

  if (plot) {

    # Colour palettes

    # ERI: cream -> light tan -> orange-brown -> dark brown
    eri_palette <- c("#FFFFFF", "#F5E8C0", "#E8C97A", "#C8843A", "#8B4513")

    # Hillshade: white -> dark grey
    hs_palette  <- grDevices::grey(seq(1, 0, length.out = 256))

    # Hillshade raster

    hillshade <- NULL

    if (!is.null(dem)) {
      dem_al <- dem
      if (!terra::same.crs(dem_al, eri))
        dem_al <- terra::project(dem_al, eri)
      if (!geom_match(dem_al, eri))
        dem_al <- terra::resample(dem_al, eri, method = "bilinear")

      slope     <- terra::terrain(dem_al, v = "slope",  unit = "radians")
      aspect    <- terra::terrain(dem_al, v = "aspect", unit = "radians")
      hillshade <- terra::shade(slope, aspect, angle = 45, direction = 315)
      names(hillshade) <- "Hillshade"
    }

    # Build tmap layers

    tmap::tmap_mode("plot")

    # Hillshade base layer (if available)
    if (!is.null(hillshade)) {
      main_map <- tmap::tm_shape(hillshade) +
        tmap::tm_raster(
          col.scale  = tmap::tm_scale_continuous(
            values   = hs_palette,
            value.na = "white"
          ),
          col.legend = tmap::tm_legend(
            title       = "Hillshade\nValue Range",
            orientation = "portrait",
            position    = tmap::tm_pos_out("right", "bottom"),
            frame       = FALSE,
            text.size   = 0.7
          )
        ) +
        tmap::tm_shape(eri)
    } else {
      main_map <- tmap::tm_shape(eri)
    }

    main_map <- main_map +
      tmap::tm_raster(
        col        = "ERI",
        col.scale  = tmap::tm_scale_continuous(
          values   = eri_palette,
          value.na = NA,
          midpoint = NA
        ),
        col.legend = tmap::tm_legend(
          title       = "Erosivity Index\nValue range",
          orientation = "portrait",
          position    = tmap::tm_pos_out("right", "top"),
          frame       = FALSE,
          text.size   = 0.7
        ),
        col_alpha  = if (!is.null(hillshade)) 0.65 else 1.0
      ) +
      tmap::tm_scalebar(
        breaks      = c(0, 10, 20),
        position    = tmap::tm_pos_in("left", "bottom"),
        text.size   = 0.7,
        color.dark  = "black",
        color.light = "white",
        lwd         = 1
      ) +
      tmap::tm_compass(
        type      = "arrow",
        position  = tmap::tm_pos_in("right", "top"),
        size      = 1.5,
        text.size = 0.8
      ) +
      tmap::tm_graticules(
        lines       = TRUE,
        labels.size = 0.65,
        ticks       = TRUE,
        col         = "grey60",
        lwd         = 0.4,
        n.x         = 4,
        n.y         = 4
      ) +
      tmap::tm_title(
        text     = map_title,
        size     = 1.0,
        fontface = "plain",
        position = tmap::tm_pos_out("left", "top")
      ) +
      tmap::tm_layout(
        frame                   = TRUE,
        frame.lwd               = 1,
        bg.color                = "white",
        outer.bg.color          = "#f5f0eb",
        legend.outside          = FALSE,
        legend.position         = tmap::tm_pos_in("right", "top"),
        legend.frame            = FALSE,
        inner.margins           = c(0.02, 0.02, 0.02, 0.02),
        outer.margins           = 0.02
      )

    # Source / caption annotation

    if (!is.null(data_source)) {
      main_map <- main_map +
        tmap::tm_credits(
          text     = data_source,
          size     = 0.5,
          position = tmap::tm_pos_out("left", "bottom"),
          align    = "left"
        )
    }

    if (!is.null(figure_caption)) {
      main_map <- main_map +
        tmap::tm_credits(
          text     = figure_caption,
          size     = 0.65,
          fontface = "bold",
          position = tmap::tm_pos_out("center", "bottom"),
          align    = "center"
        )
    }

    # Print map

    print(main_map)
    map_obj <- main_map
  }

  # Return

  return(invisible(list(eri = eri, map = map_obj)))
}
