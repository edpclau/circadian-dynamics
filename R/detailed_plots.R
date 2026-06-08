#' Generate detailed per-unit analysis plots
#'
#' @description Saves a stacked multi-panel PDF (raw signal, optional
#'   detrend/smooth/butterworth traces, autocorrelation, Lomb-Scargle, and the
#'   fitted cosinors) for each analysis unit.
#'
#' @param trikinetics_analyzed Output from [process_timeseries_main].
#' @param sampling_rate Character sampling-rate unit, e.g. `"minutes"` or `"hours"`.
#' @param windows Logical. If `TRUE` (default) one PDF per individual is written,
#'   each with one page per window; if `FALSE` a single `Window_plots.pdf` with
#'   one page per individual.
#' @return Invisibly `NULL`; called for the PDF side effect.
#' @export
#' @importFrom purrr iwalk
#' @importFrom lubridate duration
detailed_plots <- function(trikinetics_analyzed, sampling_rate = "minutes", windows = TRUE) {
  if (windows) {
    generate_plots_with_windows(trikinetics_analyzed, sampling_rate)
  } else {
    generate_plots_no_windows(trikinetics_analyzed, sampling_rate)
  }
}

# One PDF, one page per individual (no time windows). Internal.
generate_plots_no_windows <- function(trikinetics_analyzed, sampling_rate = "hours") {
  pdf("Window_plots.pdf")
  on.exit(dev.off())
  iwalk(trikinetics_analyzed, function(ind, name) .plot_analysis_page(ind, paste("IND", name), sampling_rate))
  invisible()
}

# One PDF per individual, one page per window. Internal.
generate_plots_with_windows <- function(trikinetics_analyzed, sampling_rate = "minutes") {
  iwalk(trikinetics_analyzed, function(ind, name) {
    pdf(paste0("Window_plots_", name, ".pdf"))
    on.exit(dev.off())
    iwalk(ind, function(window, i) .plot_analysis_page(window, paste("window", i), sampling_rate))
  })
  invisible()
}

# Render one analysis unit (an individual or a window) as a stacked page.
.plot_analysis_page <- function(unit, label, sampling_rate) {
  d <- unit$data
  detrended <- "detrended" %in% names(d)
  smoothed <- "smoothed" %in% names(d)
  butterworth <- "butterworth" %in% names(d)
  acf_run <- !is.na(unit$acf$results$period)
  lomb_run <- !is.na(unit$lomb$results$period)
  n_rows <- detrended + smoothed + butterworth + 4
  l_adjust <- -2

  par(mfrow = c(n_rows, 1), mar = c(2, 4, 1.5, 0))

  plot(d$datetime, d$value, type = "l", xlab = "", ylab = "Raw Activity")
  mtext(label, side = 3, adj = 0.9)
  par(mar = c(2, 4, 0, 0))

  if (detrended) plot(d$datetime, d$detrended, type = "l", xlab = "", ylab = "Detrended Activity")
  if (smoothed) plot(d$datetime, d$smoothed, type = "l", xlab = "", ylab = "MovAvg Activity")
  if (butterworth) plot(d$datetime, d$butterworth, type = "l", xlab = "", ylab = "Butterworth Activity")

  if (acf_run) {
    x <- as.numeric(duration(seq(0, length(d$datetime) - 1, 1), sampling_rate), "hours")
    plot(x, unit$acf$results$autocorrelation, type = "l", xlab = "", ylab = "Power")
    ci <- 1.965 / sqrt(length(x / 60))
    abline(h = c(ci, -ci), col = "red", lty = 2)
    text(x = as.numeric(duration(unit$acf$results$datetime, sampling_rate), "hour"),
         y = unit$acf$results$max_peak_of_int, label = "*", col = "red", cex = 3)
    text(x = max(x) * 0.5, y = max(unit$acf$results$autocorrelation) * 0.9,
         label = paste("Autocorrelation:", "Period =", round(unit$acf$results$period, 2), "|",
                       "Power (R.I.) =", round(unit$acf$results$max_peak_of_int, 2)), col = "red")
  } else {
    plot.new()
    mtext("Autocorrelation Not Run", side = 1, line = l_adjust)
  }

  if (lomb_run) {
    plot(unit$lomb$results$scanned, unit$lomb$results$power, type = "l", xlab = "", ylab = "Power")
    abline(h = unit$lomb$results$sig_level, col = "blue", lty = 2)
    text(x = max(unit$lomb$results$scanned) * 0.605, y = max(unit$lomb$results$power) * 0.9,
         label = paste("Lomb-Scargle:", "Period =", round(unit$lomb$results$period, 2), "|",
                       "Power (R.I.) =", round(unit$lomb$results$peak, 2)), col = "blue")
  } else {
    plot.new()
    mtext("Lomb-Scargle Periodogram Not Run", side = 1, line = l_adjust)
  }

  if (acf_run || lomb_run) {
    plot(d$datetime, d[[ncol(d)]], type = "l", xlab = "", ylab = "")
  } else {
    plot.new()
    mtext("Cosinor Not Run", side = 1, line = l_adjust)
  }
  if (lomb_run) {
    lines(d$datetime, unit$lomb$cosinor$wave, col = "blue")
    text(x = mean(d$datetime), y = max(d[[ncol(d)]]) * 0.9,
         label = paste("Lomb-Scargle Cosinor:", "R^2 =", round(unit$lomb$cosinor$adj_r_squared, 4)), col = "blue")
  }
  if (acf_run) {
    lines(d$datetime, unit$acf$cosinor$wave, col = "red")
    text(x = mean(d$datetime), y = max(d[[ncol(d)]]) * 0.7,
         label = paste("Acf Cosinor:", "R^2 =", round(unit$acf$cosinor$adj_r_squared, 4)), col = "red")
  }
}
