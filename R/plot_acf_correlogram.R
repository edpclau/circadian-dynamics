#' Plot the autocorrelation function (correlogram)
#'
#' @description Draws the autocorrelogram the way chronobiology rhythm analyses
#'   conventionally show it: autocorrelation against lag (in hours), with the
#'   white-noise 95% significance band at +/- 1.96 / sqrt(N) (N held constant
#'   across lags, per Levine et al. 2002) and the detected period peak marked. The
#'   height of that peak is the Rhythmicity Index. This complements
#'   [plot_acf_results()], which tracks the scalar period/rhythm-strength across
#'   windows; here the whole correlogram is drawn.
#'
#' @param df The `utils` table from [simplify_data()], which carries the
#'   autocorrelation vector (`acf`), the per-timepoint `datetime` (used to derive
#'   the lag axis), and the detected peak (`acf_period`, `acf_peak`).
#' @param max_lag Upper limit (hours) for the lag axis. By default (`NULL`) the
#'   axis is zoomed to ~4 cycles of the detected period so the oscillating recurrence
#'   peaks stay readable; pass a number to fix it, or `Inf` for the full range.
#' @return A named list of ggplots, one per individual. When the pipeline was
#'   windowed, each individual's plot is faceted by window.
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr group_by across all_of mutate ungroup slice n
#' @examples
#' \dontrun{
#' plots <- plot_acf_correlogram(simplify_data(out)$utils)
#' }
plot_acf_correlogram <- function(df, max_lag = NULL) {
  windowed <- "window" %in% names(df)
  keys <- if (windowed) c("data", "window") else "data"

  # Lag axis (hours from each unit's first timepoint) and series length N per unit.
  prepped <- dplyr::ungroup(dplyr::mutate(
    dplyr::group_by(df, dplyr::across(dplyr::all_of(keys))),
    lag_hours = as.numeric(difftime(.data$datetime, min(.data$datetime), units = "hours")),
    n_obs = dplyr::n()
  ))
  accent <- okabe_ito(8)

  by_ind <- split(prepped, prepped$data)
  plots <- lapply(names(by_ind), function(nm) {
    g <- by_ind[[nm]]
    # One summary row per unit carries the peak and the (constant-N) white-noise band.
    summ <- dplyr::ungroup(dplyr::slice(dplyr::group_by(g, dplyr::across(dplyr::all_of(keys))), 1))
    summ$bound <- 1.96 / sqrt(summ$n_obs)

    # Mark the detected period peak ON the curve: take the autocorrelation at the
    # lag nearest the detected period (so the marker sits on the line, not floating).
    grp <- if (windowed) split(g, g$window) else list(g)
    peaks <- do.call(rbind, lapply(grp, function(gg) {
      per <- gg$acf_period[1]
      if (length(per) == 0 || !is.finite(per)) return(NULL)
      i <- which.min(abs(gg$lag_hours - per))
      row <- data.frame(period = per, ri = gg$acf[i])
      if (windowed) row$window <- gg$window[1]
      row
    }))
    if (is.null(peaks)) {
      peaks <- data.frame(period = numeric(0), ri = numeric(0))
      if (windowed) peaks$window <- numeric(0)
    }

    if (is.null(max_lag)) {
      per <- stats::median(summ$acf_period, na.rm = TRUE)
      xmax <- if (is.finite(per)) min(max(g$lag_hours), 4 * per) else max(g$lag_hours)
    } else {
      xmax <- min(max_lag, max(g$lag_hours))
    }

    p <- ggplot2::ggplot(g, ggplot2::aes(x = .data$lag_hours, y = .data$acf)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
      ggplot2::geom_line() +
      ggplot2::geom_hline(data = summ, ggplot2::aes(yintercept = .data$bound),
                          linetype = "dashed", colour = accent[6], na.rm = TRUE) +
      ggplot2::geom_hline(data = summ, ggplot2::aes(yintercept = -.data$bound),
                          linetype = "dashed", colour = accent[6], na.rm = TRUE) +
      ggplot2::geom_point(data = peaks, ggplot2::aes(x = .data$period, y = .data$ri),
                          colour = accent[5], size = 2, na.rm = TRUE) +
      ggplot2::labs(x = "Lag (hours)", y = "Autocorrelation", title = nm) +
      ggplot2::coord_cartesian(xlim = c(0, xmax)) +
      theme_window()
    if (windowed) p <- p + ggplot2::facet_grid(window ~ ., switch = "y")
    p
  })
  stats::setNames(plots, names(by_ind))
}
