# calc_erosion_risk.R
# Combine R, LS, and C factors into an Erosion Risk Index (ERI).
# ERI = R_norm x LS_norm x C_norm  (each factor normalised to 0-1)
#
# Requires: terra (>= 1.7-0), ggplot2, tidyterra, ggspatial

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
#' cartographic layout built with \pkg{ggplot2}, \pkg{tidyterra} and
#' \pkg{ggspatial}: cream-to-brown colour ramp, coordinate grid with degree
#' labels, north arrow, scale bar, continuous legend, and optional title,
#' figure caption and data source line.
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
#' @param figure_caption Character. Figure caption printed below the map
#'   as a plot subtitle. Default: \code{NULL} (no caption).
#' @param data_source Character. Data-source string printed below the map
#'   in small type. Default: \code{NULL} (no source line).
#'
#' @return Invisibly returns a list with two elements:
#' \describe{
#'   \item{\code{eri}}{SpatRaster (0-1) with Erosion Risk Index values,
#'     named \code{"ERI"}.}
#'   \item{\code{map}}{A \code{ggplot} object, or \code{NULL} if
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
#' ggplot2::ggsave("erosion_risk_map.png", result$map,
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
    for (pkg in c("ggplot2", "tidyterra", "ggspatial")) {
      if (!requireNamespace(pkg, quietly = TRUE))
        stop("Package '", pkg, "' is required for plot = TRUE.")
    }
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

    # ERI colour palette
    eri_colours <- c("#FFFFFF", "#F5E8C0", "#E8C97A", "#C8843A", "#8B4513")

    # Build caption string
    caption_str <- paste(
      c(data_source, figure_caption),
      collapse = "\n"
    )
    if (nchar(trimws(caption_str)) == 0) caption_str <- NULL

    map_obj <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = eri) +
      ggplot2::scale_fill_gradientn(
        colours  = eri_colours,
        na.value = NA,
        name     = "Erosivity\nIndex"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tr",
        which_north = "true",
        height   = ggplot2::unit(1.2, "cm"),
        width    = ggplot2::unit(1.0, "cm"),
        style    = ggspatial::north_arrow_fancy_orienteering(
          fill      = c("black", "white"),
          line_col  = "grey20",
          text_col  = "grey20",
          text_size = 10
        )
      ) +
      ggspatial::annotation_scale(
        location   = "bl",
        width_hint = 0.25,
        text_cex   = 0.75,
        line_width = 1,
        height     = ggplot2::unit(0.3, "cm"),
        pad_x      = ggplot2::unit(0.4, "cm"),
        pad_y      = ggplot2::unit(0.4, "cm")
      ) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::labs(
        title   = map_title,
        caption = caption_str
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        # Title
        plot.title         = ggplot2::element_text(
          size   = 13,
          hjust  = 0.5,
          face   = "plain",
          margin = ggplot2::margin(b = 6)
        ),
        # Caption
        plot.caption       = ggplot2::element_text(
          size  = 7,
          hjust = 0,
          color = "grey40"
        ),
        # Legend
        legend.position    = "right",
        legend.title       = ggplot2::element_text(size = 9),
        legend.text        = ggplot2::element_text(size = 8),
        legend.key.height  = ggplot2::unit(1.5, "cm"),
        legend.key.width   = ggplot2::unit(0.4, "cm"),
        legend.frame       = ggplot2::element_rect(
          colour = "grey60", linewidth = 0.4
        ),
        # Axis
        axis.text          = ggplot2::element_text(size = 8),
        axis.title         = ggplot2::element_blank(),
        # Panel
        panel.grid.major   = ggplot2::element_line(
          colour = "grey70", linewidth = 0.3, linetype = "dashed"
        ),
        panel.grid.minor   = ggplot2::element_blank(),
        panel.border       = ggplot2::element_rect(
          colour = "grey30", linewidth = 0.8
        ),
        # Margins
        plot.margin        = ggplot2::margin(10, 10, 10, 10)
      )

    print(map_obj)
  }

  return(invisible(list(eri = eri, map = map_obj)))
}
