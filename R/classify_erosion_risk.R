# classify_erosion_risk.R
# Classify an Erosion Risk Index (ERI) raster into discrete risk classes
# and compute per-class area statistics.
#
# Requires: terra (>= 1.7-0)

#' Classify an Erosion Risk Index into risk classes
#'
#' Classifies a continuous ERI raster (0-1) into discrete risk classes using
#' configurable break points and computes per-class area statistics.
#'
#' @param eri SpatRaster. Output of \code{calc_erosion_risk()}. Expected to
#'   contain normalised ERI values in the range \eqn{[0, 1]}.
#' @param breaks Numeric vector of class boundaries. Must start at 0 and end
#'   at 1. Default: \code{c(0, 0.2, 0.4, 0.6, 0.8, 1)} (five equal classes).
#' @param labels Character vector of class labels. Length must equal
#'   \code{length(breaks) - 1}. Default: \code{c("Very Low", "Low",
#'   "Medium", "High", "Very High")}.
#' @param plot Logical. If \code{TRUE} (default), displays the classified
#'   raster with a colour scheme from green (low risk) to red (high risk)
#'   and a categorical legend. NA pixels are shown in white.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{\code{$classified}}{SpatRaster with integer class values
#'       (1 to \code{length(labels)}). NA pixels are preserved.
#'       The layer is named \code{"ERI_class"}.}
#'     \item{\code{$summary}}{data.frame with columns:
#'       \code{class} (integer), \code{label} (character),
#'       \code{pixels} (integer), \code{area_ha} (numeric),
#'       \code{proportion} (numeric, 0-1).}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eri    <- calc_erosion_risk(r, ls, c)
#'
#' # Default classification (5 classes)
#' result <- classify_erosion_risk(eri)
#' print(result$summary)
#'
#' # Custom breaks and labels (3 classes)
#' result <- classify_erosion_risk(
#'   eri,
#'   breaks = c(0, 0.33, 0.66, 1),
#'   labels = c("Low", "Medium", "High")
#' )
#'
#' # Without plot
#' result <- classify_erosion_risk(eri, plot = FALSE)
#' }

classify_erosion_risk <- function(eri,
                                  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                  labels = c("Very Low", "Low", "Medium",
                                             "High", "Very High"),
                                  plot   = TRUE) {

  # Input validation

  if (!inherits(eri, "SpatRaster"))
    stop("'eri' must be a SpatRaster object.")
  if (terra::nlyr(eri) != 1)
    stop("'eri' must be a single-layer SpatRaster.")
  if (!is.numeric(breaks) || length(breaks) < 3)
    stop("'breaks' must be a numeric vector with at least 3 values.")
  if (breaks[1] != 0 || breaks[length(breaks)] != 1)
    stop("'breaks' must start at 0 and end at 1.")
  if (any(diff(breaks) <= 0))
    stop("'breaks' must be strictly increasing.")
  if (!is.character(labels) || length(labels) != length(breaks) - 1)
    stop("'labels' must be a character vector of length length(breaks) - 1 (",
         length(breaks) - 1, ").")
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

  n_classes <- length(labels)

  # Build reclassification matrix: from, to, class integer
  rcl <- matrix(
    c(breaks[-length(breaks)], breaks[-1], seq_len(n_classes)),
    ncol  = 3,
    byrow = FALSE
  )

  eri_class        <- terra::classify(eri, rcl, include.lowest = TRUE)
  names(eri_class) <- "ERI_class"

  # Area statistics

  # Cell area in hectares
  cell_area_ha <- prod(terra::res(eri_class)) / 10000

  # Count pixels per class
  vals        <- as.vector(terra::values(eri_class))
  total_valid <- sum(!is.na(vals))

  summary_df <- data.frame(
    class  = seq_len(n_classes),
    label  = labels,
    pixels = vapply(seq_len(n_classes),
                    function(i) sum(vals == i, na.rm = TRUE),
                    integer(1)),
    stringsAsFactors = FALSE
  )

  summary_df$area_ha    <- summary_df$pixels * cell_area_ha
  summary_df$proportion <- ifelse(
    sum(summary_df$pixels) > 0,
    round(summary_df$pixels / sum(summary_df$pixels), 4),
    0
  )

  message("ERI classified into ", n_classes, " classes using breaks: ",
          paste(breaks, collapse = ", "), ".")

  # Optional plot

  if (plot) {
    pal <- grDevices::colorRampPalette(c("darkgreen", "yellow", "red"))
    class_cols <- pal(n_classes)

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

    # Build legend labels with break ranges
    legend_labels <- c(
      mapply(function(lbl, lo, hi)
        sprintf("%s (%.2f - %.2f)", lbl, lo, hi),
        rev(labels),
        rev(breaks[-length(breaks)]),
        rev(breaks[-1])
      ),
      "NA"
    )

    graphics::legend(
      x      = usr[2] + (usr[2] - usr[1]) * 0.03,
      y      = usr[4],
      legend = legend_labels,
      fill   = c(rev(class_cols), "white"),
      border = c(rep("white", n_classes), "grey60"),
      bty    = "n",
      cex    = 0.9
    )

    graphics::par(xpd = FALSE)
  }

  return(invisible(list(
    classified = eri_class,
    summary    = summary_df
  )))
}
