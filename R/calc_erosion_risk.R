# calc_erosion_risk.R
# Combine R, LS, and C factors into an Erosion Risk Index (ERI).
# ERI = R_norm x LS_norm x C_norm  (each factor normalised to 0-1)
#
# Requires: terra (>= 1.7-0)

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
#' If \code{plot = TRUE} and \code{dem} is supplied, the ERI is displayed
#' overlaid on a hillshade computed internally from the DEM, matching the
#' cartographic style of the aridRUSLE output maps. The plot includes a
#' north arrow, scale bar, and two gradient legends (ERI and hillshade).
#'
#' @param r_factor SpatRaster. Output of \code{calc_r_factor()}.
#' @param ls_factor SpatRaster. Output of \code{calc_ls_factor()}.
#'   Used as the reference grid for alignment.
#' @param c_factor SpatRaster. Output of \code{calc_c_factor()}.
#' @param dem SpatRaster or NULL. Digital Elevation Model used to compute
#'   hillshade for the map background. If \code{NULL} (default), no hillshade
#'   is rendered and the ERI is plotted on a white background.
#' @param normalize Logical. If \code{TRUE} (default), each factor is
#'   min-max normalised to 0-1 before multiplication.
#' @param resample_method Character. Resampling method passed to
#'   \code{terra::resample()}. Default: \code{"bilinear"}.
#' @param plot Logical. If \code{TRUE} (default), displays a publication-style
#'   map of the ERI with hillshade background (if \code{dem} is supplied),
#'   north arrow, scale bar, and gradient legends.
#'
#' @return SpatRaster (0-1) with Erosion Risk Index values, named
#'   \code{"ERI"}.
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
#' # ERI with hillshade background map
#' eri <- calc_erosion_risk(r, ls, c, dem = dem)
#'
#' # Without hillshade
#' eri <- calc_erosion_risk(r, ls, c)
#'
#' # Without plot
#' eri <- calc_erosion_risk(r, ls, c, plot = FALSE)
#' }

calc_erosion_risk <- function(r_factor, ls_factor, c_factor,
                              dem             = NULL,
                              normalize       = TRUE,
                              resample_method = "bilinear",
                              plot            = TRUE) {

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

  # Helper: geometry check

  geom_match <- function(a, b) {
    isTRUE(tryCatch(
      terra::compareGeom(a, b, res = TRUE, stopOnError = TRUE),
      error = function(e) FALSE
    ))
  }

  # Auto-alignment to ls_factor reference grid

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
          "Pass result to classify_erosion_risk() for risk classification.")

  # Optional plot

  if (plot) {

    # ERI colour palette: cream -> yellow -> orange -> brown
    eri_pal <- grDevices::colorRampPalette(
      c("#FFFFFF", "#F5E8C0", "#E8C97A", "#C8843A", "#8B4513")
    )

    # Hillshade

    hillshade <- NULL

    if (!is.null(dem)) {
      dem_aligned <- dem
      if (!terra::same.crs(dem_aligned, eri))
        dem_aligned <- terra::project(dem_aligned, eri)
      if (!geom_match(dem_aligned, eri))
        dem_aligned <- terra::resample(dem_aligned, eri, method = "bilinear")

      slope     <- terra::terrain(dem_aligned, v = "slope",  unit = "radians")
      aspect    <- terra::terrain(dem_aligned, v = "aspect", unit = "radians")
      hillshade <- terra::shade(slope, aspect, angle = 45, direction = 315)
    }

    # Layout
    # Wide right margin for legends; bottom margin for scale bar

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(mar = c(5, 4, 4, 12), xpd = FALSE)

    # Base plot

    if (!is.null(hillshade)) {
      terra::plot(
        hillshade,
        col    = grDevices::grey(seq(0, 1, length.out = 256)),
        legend = FALSE,
        axes   = TRUE,
        main   = "Erosion Risk Index (ERI)"
      )
      terra::plot(
        eri,
        col    = grDevices::adjustcolor(eri_pal(100), alpha.f = 0.65),
        legend = FALSE,
        axes   = FALSE,
        add    = TRUE
      )
    } else {
      terra::plot(
        eri,
        col    = eri_pal(100),
        legend = FALSE,
        axes   = TRUE,
        main   = "Erosion Risk Index (ERI)"
      )
    }

    # Coordinate helpers

    graphics::par(xpd = TRUE)
    usr <- graphics::par("usr")
    x_range <- usr[2] - usr[1]
    y_range <- usr[4] - usr[3]

    # Gradient colour bar helper

    draw_colorbar <- function(x_left, y_top, w, h, pal, n,
                              label_hi, label_lo, bar_title) {
      y_steps <- seq(y_top, y_top - h, length.out = n + 1)
      cols    <- pal(n)
      for (i in seq_len(n)) {
        graphics::rect(
          xleft   = x_left,
          ybottom = y_steps[i + 1],
          xright  = x_left + w,
          ytop    = y_steps[i],
          col     = cols[i],
          border  = NA
        )
      }
      graphics::rect(x_left, y_top - h, x_left + w, y_top,
                     border = "grey40", lwd = 0.8)
      graphics::text(x_left + w / 2,
                     y_top + y_range * 0.025,
                     labels = bar_title,
                     adj = c(0.5, 0), cex = 0.8, font = 2)
      graphics::text(x_left + w + x_range * 0.01,
                     y_top,
                     labels = label_hi, adj = c(0, 0.5), cex = 0.75)
      graphics::text(x_left + w + x_range * 0.01,
                     y_top - h,
                     labels = label_lo, adj = c(0, 0.5), cex = 0.75)
    }

    bar_x  <- usr[2] + x_range * 0.04
    bar_w  <- x_range * 0.04
    bar_h  <- y_range * 0.30

    # ERI colour bar
    eri_max <- round(terra::global(eri, "max", na.rm = TRUE)[[1]], 4)
    eri_min <- round(terra::global(eri, "min", na.rm = TRUE)[[1]], 4)

    draw_colorbar(
      x_left    = bar_x,
      y_top     = usr[4] - y_range * 0.05,
      w         = bar_w,
      h         = bar_h,
      pal       = eri_pal,
      n         = 100,
      label_hi  = format(eri_max, nsmall = 4),
      label_lo  = format(eri_min, nsmall = 4),
      bar_title = "Erosivity\nIndex"
    )

    # Hillshade colour bar
    if (!is.null(hillshade)) {
      hs_y_top <- usr[4] - y_range * 0.05 - bar_h - y_range * 0.12
      hs_max   <- round(terra::global(hillshade, "max", na.rm = TRUE)[[1]], 3)
      hs_min   <- round(terra::global(hillshade, "min", na.rm = TRUE)[[1]], 3)

      draw_colorbar(
        x_left    = bar_x,
        y_top     = hs_y_top,
        w         = bar_w,
        h         = bar_h,
        pal       = function(n) grDevices::grey(seq(1, 0, length.out = n)),
        n         = 100,
        label_hi  = format(hs_max, nsmall = 3),
        label_lo  = format(hs_min, nsmall = 3),
        bar_title = "Hillshade"
      )
    }

    # North arrow
    # Positioned top-right inside the plot

    arrow_x  <- usr[2] - x_range * 0.06
    arrow_y  <- usr[4] - y_range * 0.04
    arrow_h  <- y_range * 0.06

    # Filled north-pointing arrow
    graphics::polygon(
      x   = c(arrow_x, arrow_x - x_range * 0.015,
              arrow_x, arrow_x + x_range * 0.015),
      y   = c(arrow_y, arrow_y - arrow_h,
              arrow_y - arrow_h * 0.5, arrow_y - arrow_h),
      col = "black"
    )
    graphics::text(arrow_x, arrow_y + y_range * 0.02,
                   labels = "N", cex = 0.9, font = 2, adj = c(0.5, 0))

    # Scale bar
    # Positioned bottom-left inside the plot

    # Determine scale bar length in map units
    # For geographic CRS: 1 degree ≈ 111 km; pick a round number of km
    if (terra::is.lonlat(eri)) {
      # Try scale bars of 10, 20, 25, 50 km — pick the one that fits ~20% width
      km_options   <- c(5, 10, 20, 25, 50, 100)
      deg_per_km   <- 1 / 111
      deg_widths   <- km_options * deg_per_km
      fits         <- deg_widths < x_range * 0.25
      bar_km       <- km_options[max(which(fits))]
      bar_deg      <- bar_km * deg_per_km
      bar_label    <- paste0(bar_km, " km")
    } else {
      # Projected CRS: units are metres
      m_options  <- c(1000, 2000, 5000, 10000, 20000, 50000)
      fits       <- m_options < x_range * 0.25
      bar_m      <- m_options[max(which(fits))]
      bar_deg    <- bar_m
      bar_label  <- paste0(bar_m / 1000, " km")
    }

    sb_x0 <- usr[1] + x_range * 0.04
    sb_x1 <- sb_x0 + bar_deg
    sb_y  <- usr[3] + y_range * 0.04
    sb_h  <- y_range * 0.012

    # Alternating black/white segments (2 segments)
    seg_w <- bar_deg / 2
    graphics::rect(sb_x0,          sb_y, sb_x0 + seg_w, sb_y + sb_h,
                   col = "black",  border = "black")
    graphics::rect(sb_x0 + seg_w,  sb_y, sb_x1,         sb_y + sb_h,
                   col = "white",  border = "black")

    # Labels
    graphics::text(sb_x0, sb_y - y_range * 0.015,
                   labels = "0", cex = 0.7, adj = c(0.5, 1))
    graphics::text(sb_x0 + seg_w, sb_y - y_range * 0.015,
                   labels = format(bar_km / 2, nsmall = 0),
                   cex = 0.7, adj = c(0.5, 1))
    graphics::text(sb_x1, sb_y - y_range * 0.015,
                   labels = bar_label, cex = 0.7, adj = c(0.5, 1))

    graphics::par(xpd = FALSE)
  }

  return(invisible(eri))
}
