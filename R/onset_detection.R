#' Detect activity onsets
#'
#' @description Finds activity onsets as rising threshold crossings of the activity
#'   trace: the points where (optionally smoothed) activity rises from below to above
#'   a threshold, with a refractory gap so each circadian cycle contributes at most
#'   one onset. Onsets are the standard phase marker for estimating the free-running
#'   period (see [estimate_tau()]); they are drawn in red on actograms.
#'
#' @param df A data.frame whose first column is a POSIXct `datetime` and whose last
#'   column is the activity values.
#' @param sampling_rate Character sampling rate, e.g. `"30 minutes"` (used only to
#'   size the default smoothing window).
#' @param threshold Activity threshold for an onset. If `NULL` (default) the mean of
#'   the (smoothed) activity is used; pass a number, or `"median"` to use the median.
#' @param smooth_hours Width, in hours, of the centred moving-average pre-smoothing.
#'   Default 1 h; set 0 to disable smoothing.
#' @param min_gap_hours Refractory gap: minimum hours between successive onsets, so a
#'   single noisy cycle is not split into several. Default 8.
#' @return A data.frame with `onset_index` (1, 2, ... in time order), `onset_datetime`,
#'   and `onset_hours` (hours from the first sample).
#' @export
detect_onsets <- function(df, sampling_rate, threshold = NULL,
                          smooth_hours = 3, min_gap_hours = 16) {
  dt <- df[["datetime"]]
  v  <- df[[ncol(df)]]
  hours <- as.numeric(difftime(dt, dt[1], units = "hours"))

  # Per-sample spacing in hours, to convert smoothing/gap windows into sample counts.
  step_h <- stats::median(diff(hours), na.rm = TRUE)
  if (!is.finite(step_h) || step_h <= 0) step_h <- 1

  if (smooth_hours > 0) {
    k <- max(1L, round(smooth_hours / step_h))
    v <- as.numeric(stats::filter(v, rep(1 / k, k), sides = 2))
    v[is.na(v)] <- df[[ncol(df)]][is.na(v)]  # keep edges (filter() pads with NA)
  }

  thr <- if (is.null(threshold)) mean(v, na.rm = TRUE)
         else if (identical(threshold, "median")) stats::median(v, na.rm = TRUE)
         else threshold

  above <- v >= thr
  above[is.na(above)] <- FALSE
  n <- length(above)
  # Rising edges at i (2..n): above[i] & !above[i-1]. Excludes a series that simply
  # starts already above threshold (not a true onset).
  rise <- which(above[-1] & !above[-n]) + 1L

  # Enforce the refractory gap.
  keep <- integer(0); last <- -Inf
  for (i in rise) {
    if (hours[i] - last >= min_gap_hours) { keep <- c(keep, i); last <- hours[i] }
  }

  data.frame(
    onset_index = seq_along(keep),
    onset_datetime = dt[keep],
    onset_hours = hours[keep]
  )
}

#' Estimate the free-running period (tau) from activity onsets
#'
#' @description Fits a least-squares line through the activity onsets, regressing
#'   onset time (hours from start) on the onset's sequence number. Because onsets
#'   recur once per cycle (the refractory gap in [detect_onsets()] enforces this),
#'   the slope is the inter-onset interval, i.e. the free-running period tau:
#'   leftward onset drift across days gives tau < 24 h, rightward drift tau > 24 h.
#'   (A calendar-day predictor was tried but is biased when tau != 24, since drift
#'   compresses/expands the day range; sequential index is unbiased for clean
#'   one-per-cycle detection.) At least ~6 onsets are recommended.
#'
#' @param onsets A data.frame from [detect_onsets()] with `onset_index` and
#'   `onset_hours` columns.
#' @return A list with `tau` (hours; the fitted slope), `intercept` (fitted onset
#'   time at index 0), `n` (number of onsets used), and `fit` (the `lm` object, or
#'   `NULL` if fewer than two onsets).
#' @export
estimate_tau <- function(onsets) {
  n <- if (is.null(onsets)) 0L else nrow(onsets)
  if (n < 2) return(list(tau = NA_real_, intercept = NA_real_, n = n, fit = NULL))
  idx <- onsets$onset_index
  fit <- stats::lm(onsets$onset_hours ~ idx)
  list(
    tau = as.numeric(stats::coef(fit)[2]),
    intercept = as.numeric(stats::coef(fit)[1]),
    n = n,
    fit = fit
  )
}
