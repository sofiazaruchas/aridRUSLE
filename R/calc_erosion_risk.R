# calc_erosion_risk.R
# Combine R, LS, and C factors into an Erosion Risk Index (ERI).
# ERI = R_norm x LS_norm x C_norm  (each factor normalised to 0-1)
#
# Requires: terra (>= 1.7-0)

#' Combine factors into an Erosion Risk Index (ERI)
#'
#' Normalises R, LS and C to 0-1 and multiplies them pixel-wise to produce
#' the Erosion Risk Index (ERI). Automatically checks that all three rasters
#' share the same geometry (extent, resolution, CRS).
#'
#' The ERI is calculated as:
#' #' \deqn{ERI = R_{norm} \times LS_{norm} \times C_{norm}}
#'
#' where each factor is min-max normalised to \eqn{[0, 1]} before multiplication.
#'
#' Higher ERI values indicate greater erosion hazard.
#'
#' @param r_factor SpatRaster. Output of \code{calc_r_factor()}.
#' @param ls_factor SpatRaster. Output of \code{calc_ls_factor()}.
#' @param c_factor SpatRaster. Output of \code{calc_c_factor()}.
#' @param normalize Logical. If \code{TRUE} (default), each factor is
#'   min-max normalised to 0-1 before multiplication.
#' @param plot Logical. If \code{TRUE} (default), displays the ERI raster
#'   with a continuous colour ramp from green (low risk) to red (high risk).
#'   Useful for visually inspecting the result before passing it to
#'   \code{classify_erosion_risk()}.
#'
#' @return SpatRaster (0-1) with Erosion Risk Index values.
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
#' # Compute ERI with default normalisation and plot
#' eri <- calc_erosion_risk(r, ls, c)
#'
#' # Without plot
#' eri <- calc_erosion_risk(r, ls, c, plot = FALSE)
#'
#' # Without normalisation
#' eri <- calc_erosion_risk(r, ls, c, normalize = FALSE)
#' }

calc_erosion_risk <- function(r_factor, ls_factor, c_factor,
                              normalize = TRUE, plot = TRUE) {

  # Input validation

  if (!inherits(r_factor, "SpatRaster"))
    stop("'r_factor' must be a SpatRaster object.")
  if (!inherits(ls_factor, "SpatRaster"))
    stop("'ls_factor' must be a SpatRaster object.")
  if (!inherits(c_factor, "SpatRaster"))
    stop("'c_factor' must be a SpatRaster object.")
  if (!is.logical(normalize) || length(normalize) != 1)
    stop("'normalize' must be a single logical value (TRUE or FALSE).")
  if (!is.logical(plot) || length(plot) != 1)
    stop("'plot' must be a single logical value (TRUE or FALSE).")

  # Geometry check

  geom_ok <- isTRUE(tryCatch(
    terra::compareGeom(r_factor, ls_factor, res = TRUE, stopOnError = TRUE),
    error = function(e) FALSE
  )) && isTRUE(tryCatch(
    terra::compareGeom(r_factor, c_factor,  res = TRUE, stopOnError = TRUE),
    error = function(e) FALSE
  ))

  if (!geom_ok)
    stop("'r_factor', 'ls_factor' and 'c_factor' must share the same ",
         "extent, resolution and CRS. Use terra::project() and ",
         "terra::resample() to align them before calling calc_erosion_risk().")

  # Optional min-max normalisation

  norm01 <- function(x) {
    mn <- terra::global(x, "min", na.rm = TRUE)[[1]]
    mx <- terra::global(x, "max", na.rm = TRUE)[[1]]
    if (mx == mn) return(x * 0)   # constant raster -> all zeros
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

  eri <- r * ls * c
  names(eri) <- "ERI"

  # Print message

  message("ERI computed as R_norm x LS_norm x C_norm. ",
          "Pass result to classify_erosion_risk() for risk classification.")

  # Optional plot

  if (plot) {
    pal <- grDevices::colorRampPalette(c("darkgreen", "yellow", "red"))

    terra::plot(
      eri,
      main       = "Erosion Risk Index (ERI)",
      col        = pal(100),
      legend     = TRUE,
      axes       = TRUE,
      mar        = c(3, 3, 3, 8)
    )

    graphics::par(xpd = TRUE)

    usr <- graphics::par("usr")
    graphics::legend(
      x      = usr[2] + (usr[2] - usr[1]) * 0.02,
      y      = usr[4],
      legend = c("Hoch", "Mittel", "Niedrig"),
      fill   = c("red", "yellow", "darkgreen"),
      border = "white",
      bty    = "n"
    )

    graphics::par(xpd = FALSE)
  }

  return(invisible(eri))

}
