# classify_erosion_risk.R
# Classify an Erosion Risk Index (ERI) raster into three risk classes.
#
# Requires: terra (>= 1.7-0)

#' Classify an Erosion Risk Index into risk classes
#'
#' Classifies a continuous ERI raster (0-1) into three discrete risk classes
#' based on equal-interval thresholds:
#'
#' \itemize{
#'   \item \strong{1 - Low}:    ERI in \eqn{[0,\ 0.33]}
#'   \item \strong{2 - Medium}: ERI in \eqn{(0.33,\ 0.66]}
#'   \item \strong{3 - High}:   ERI in \eqn{(0.66,\ 1]}
#' }
#'
#' NA pixels (e.g. water surfaces masked upstream) are preserved as NA in the
#' output.
#'
#' @param eri SpatRaster. Output of \code{calc_erosion_risk()}. Expected to
#'   contain normalised ERI values in the range \eqn{[0, 1]}.
#' @param plot Logical. If \code{TRUE} (default), displays the classified
#'   raster with a three-colour scheme (green / yellow / red) and a
#'   categorical legend. NA pixels are shown in white.
#'
#' @return SpatRaster with integer class values (1 = Low, 2 = Medium,
#'   3 = High). NA pixels are preserved. The layer is named
#'   \code{"ERI_class"}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eri        <- calc_erosion_risk(r, ls, c)
#' eri_class  <- classify_erosion_risk(eri)
#'
#' # Without plot
#' eri_class  <- classify_erosion_risk(eri, plot = FALSE)
#' }

classify_erosion_risk <- function(eri, plot = TRUE) {

  # Input validation

  if (!inherits(eri, "SpatRaster"))
    stop("'eri' must be a SpatRaster object.")
  if (terra::nlyr(eri) != 1)
    stop("'eri' must be a single-layer SpatRaster.")
  if (!is.logical(plot) || length(plot) != 1)
    stop("'plot' must be a single logical value (TRUE or FALSE).")

  # Warn if values appear to be outside the expected 0-1 range
  eri_min <- terra::global(eri, "min", na.rm = TRUE)[[1]]
  eri_max <- terra::global(eri, "max", na.rm = TRUE)[[1]]
  if (!is.na(eri_min) && !is.na(eri_max)) {
    if (eri_min < 0 || eri_max > 1)
      warning("ERI values outside [0, 1] detected. Did you pass the output ",
              "of calc_erosion_risk() with normalize = TRUE?")
  }

  # Classification

  # Reclassification matrix: from, to, class
  rcl <- matrix(
    c(0.00, 0.33, 1,
      0.33, 0.66, 2,
      0.66, 1.00, 3),
    ncol  = 3,
    byrow = TRUE
  )

  eri_class        <- terra::classify(eri, rcl, include.lowest = TRUE)
  names(eri_class) <- "ERI_class"

  message("ERI classified into 3 classes: ",
          "1 = Low (0-0.33), 2 = Medium (0.33-0.66), 3 = High (0.66-1).")

  # Optional plot

  if (plot) {
    class_cols <- c("darkgreen", "yellow", "red")

    terra::plot(
      eri_class,
      main   = "Erosion Risk Classification",
      col    = class_cols,
      legend = FALSE,
      axes   = TRUE,
      mar    = c(3, 3, 3, 9)
    )

    graphics::par(xpd = TRUE)
    usr <- graphics::par("usr")

    graphics::legend(
      x      = usr[2] + (usr[2] - usr[1]) * 0.03,
      y      = usr[4],
      legend = c("High (> 0.66)", "Medium (0.33 - 0.66)", "Low (0 - 0.33)", "NA"),
      fill   = c("red", "yellow", "darkgreen", "white"),
      border = c("white", "white", "white", "grey60"),
      bty    = "n",
      cex    = 0.9
    )

    graphics::par(xpd = FALSE)
  }

  return(invisible(eri_class))
}
