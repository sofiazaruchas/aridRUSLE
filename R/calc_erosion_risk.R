# calc_erosion_risk.R
# Combine R, LS, and C factors into an Erosion Risk Index (ERI).
# ERI = R_norm x LS_norm x C_norm  (each factor normalised to 0-1)
#
# Requires: terra (>= 1.7-0), tmap (>= 4.0)

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
#' cartographic layout with north arrow, scale bar, coordinate grid,
#' gradient legend, title, and optional figure caption and data source line.
#' The map is rendered with \pkg{tmap} (>= 4.0) in \code{"plot"} mode.
#'
#' @param r_factor  SpatRaster. Output of \code{calc_r_factor()}.
#' @param ls_factor SpatRaster. Output of \code{calc_ls_factor()}.
#'   Used as the reference grid for alignment.
#' @param c_factor  SpatRaster. Output of \code{calc_c_factor()}.
#' @param normalize Logical. If \code{TRUE} (default), each factor is
#'   min-max normalised to 0-1 before multiplication.
#' @param resample_method Character. Resampling method passed to
#'   \code{terra::resample()}. Default: \code{"bilinear"}.
#' @param plot Logical. If \code{TRUE} (default), displays a
#'   publication-style map of the ERI.
#' @param map_title Character. Title printed above the map.
#'   Default: \code{"Soil Erosivity"}.
#' @param figure_caption Character. Figure caption printed below the map.
#'   Default: \code{NULL} (no caption).
#' @param data_source Character. Data-source string printed below the map.
#'   Default: \code{NULL} (no source line).
#'
#' @return Invisibly returns a list with two elements:
#' \describe{
#'   \item{\code{eri}}{SpatRaster (0-1) with Erosion Risk Index values,
#'     named \code{"ERI"}.}
#'   \item{\code{map}}{A \code{tmap} object, or \code{NULL} if
#'     \code{plot = FALSE}.}
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
#' r  <- calc_r_factor(precip, lat = -33.0, lon = -71.0)
#' ls <- calc_ls_factor(dem)
#' c  <- calc_c_factor(ndvi_masked)
#'
#' # ERI with full publication layout
#' result <- calc_erosion_risk(r, ls, c,
#'   map_title      = "Soil Erosivity for Elqui Valley, Coquimbo Region",
#'   figure_caption = "Figure 4: Map of Erosivity Index",
#'   data_source    = "Source: NASA JPL (2013). SRTM Global 1 arc second.")
#'
#' # Save the map
#' tmap::tmap_save(result$map, "erosion_risk_map.png",
#'                 width = 250, height = 200, units = "mm", dpi = 300)
#'
#' # Access only the raster
#' eri_raster <- result$eri
#'
#' # Without plot
#' result <- calc_erosion_risk(r, ls, c, plot = FALSE)
#' }

calc_erosion_risk <- function(r_factor,
                              ls_factor,
                              c_factor,
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

  # Optional plot

  map_obj <- NULL

  if (plot) {

    tmap::tmap_mode("plot")

    # ERI colour palette: cream -> tan -> orange -> dark brown
    eri_palette <- c("#FFFFFF", "#F5E8C0", "#E8C97A", "#C8843A", "#8B4513")

    # Expand extent slightly to avoid clipping at map edges
    ext_orig   <- terra::ext(eri)
    x_pad      <- (ext_orig$xmax - ext_orig$xmin) * 0.02
    y_pad      <- (ext_orig$ymax - ext_orig$ymin) * 0.02
    ext_padded <- terra::ext(
      ext_orig$xmin - x_pad, ext_orig$xmax + x_pad,
      ext_orig$ymin - y_pad, ext_orig$ymax + y_pad
    )
    eri_padded <- terra::extend(eri, ext_padded)

    map_obj <- tmap::tm_shape(eri_padded) +
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
          frame       = TRUE,
          text.size   = 0.75
        )
      ) +
      tmap::tm_graticules(
        lines       = TRUE,
        labels.size = 0.7,
        ticks       = TRUE,
        col         = "grey50",
        lwd         = 0.5,
        n.x         = 4,
        n.y         = 5
      ) +
      tmap::tm_scalebar(
        breaks      = c(0, 10, 20),
        position    = tmap::tm_pos_in("left", "bottom"),
        text.size   = 0.7,
        color.dark  = "black",
        color.light = "white",
        lwd         = 1.2
      ) +
      tmap::tm_compass(
        type      = "arrow",
        position  = tmap::tm_pos_in("right", "top"),
        size      = 2,
        text.size = 0.9
      ) +
      tmap::tm_title(
        text     = map_title,
        size     = 1.1,
        fontface = "plain",
        position = tmap::tm_pos_out("center", "top")
      ) +
      tmap::tm_layout(
        frame          = TRUE,
        frame.lwd      = 1.2,
        bg.color       = "white",
        outer.bg.color = "white",
        inner.margins  = c(0.05, 0.05, 0.05, 0.05),
        outer.margins  = c(0.05, 0.02, 0.05, 0.02),
        legend.outside          = TRUE,
        legend.outside.position = "right",
        legend.frame            = TRUE,
        legend.text.size        = 0.75,
        legend.title.size       = 0.85
      )

    # Optional source and caption
    if (!is.null(data_source)) {
      map_obj <- map_obj +
        tmap::tm_credits(
          text     = data_source,
          size     = 0.5,
          position = tmap::tm_pos_out("left", "bottom"),
          align    = "left"
        )
    }

    if (!is.null(figure_caption)) {
      map_obj <- map_obj +
        tmap::tm_credits(
          text     = figure_caption,
          size     = 0.65,
          fontface = "bold",
          position = tmap::tm_pos_out("center", "bottom"),
          align    = "center"
        )
    }

    print(map_obj)
  }

  return(invisible(list(eri = eri, map = map_obj)))
}
