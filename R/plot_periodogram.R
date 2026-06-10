#' Plot the Lomb-Scargle periodogram (power versus period)
#'
#' @description Draws the full Lomb-Scargle spectrum the way chronobiology
#'   periodograms are conventionally shown: spectral power against period in hours,
#'   with the significance threshold as a dashed line and the detected peak marked.
#'   This complements [plot_lsp_results()], which tracks the *scalar* peak period
#'   across windows; here the whole spectrum is drawn.
#'
#' @param df The `utils` table from [simplify_data()], which carries the scanned
#'   periods (`lsp_scanned`), spectral power (`lsp_power`), significance threshold
#'   (`lsp_sig_level`), and the detected peak (`lsp_period`, `lsp_peak`).
#' @param max_period Upper limit (hours) for the period axis. Lomb-Scargle scans a
#'   very wide period range, so by default (`NULL`) the axis is zoomed to just past
#'   the significant structure (1.5x the longest period above the significance
#'   threshold) to keep the biologically relevant peaks readable. Pass a number to
#'   fix the limit, or `Inf` to show the full scanned range.
#' @return A named list of ggplots, one per individual. When the pipeline was
#'   windowed, each individual's plot is faceted by window.
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr group_by across all_of slice ungroup
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' plots <- plot_periodogram(simplify_data(out)$utils)
#' }
plot_periodogram <- function(df, max_period = NULL) {
  windowed <- "window" %in% names(df)
  keys <- if (windowed) c("data", "window") else "data"
  # The spectrum is constant within an analysis unit (list-cols repeated per
  # timepoint), so keep one representative row per group.
  reps <- dplyr::ungroup(dplyr::slice(dplyr::group_by(df, dplyr::across(dplyr::all_of(keys))), 1))
  accent <- okabe_ito(8)

  by_ind <- split(reps, reps$data)
  plots <- lapply(names(by_ind), function(nm) {
    g <- by_ind[[nm]]
    wins <- if (windowed) g$window else rep(1L, nrow(g))

    spec <- purrr::pmap_dfr(list(g$lsp_scanned, g$lsp_power, wins), function(scanned, power, w) {
      if (is.null(scanned) || length(scanned) == 0 || all(is.na(scanned))) return(NULL)
      tibble::tibble(window = w, period = as.numeric(scanned), power = as.numeric(power))
    })
    if (nrow(spec) == 0) return(NULL)  # nothing significant scanned for this unit

    peaks <- tibble::tibble(window = wins, period = g$lsp_period,
                            power = g$lsp_peak, sig = g$lsp_sig_level)
    peaks <- peaks[peaks$window %in% spec$window, , drop = FALSE]

    # Zoom the period axis to the significant structure (the scan range is huge).
    if (is.null(max_period)) {
      thr <- suppressWarnings(min(peaks$sig, na.rm = TRUE))
      sig_periods <- spec$period[is.finite(thr) & spec$power >= thr]
      xmax <- if (length(sig_periods)) min(max(sig_periods) * 1.5, max(spec$period)) else max(spec$period)
    } else {
      xmax <- min(max_period, max(spec$period))
    }

    p <- ggplot2::ggplot(spec, ggplot2::aes(x = .data$period, y = .data$power)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(data = peaks, ggplot2::aes(yintercept = .data$sig),
                          linetype = "dashed", colour = accent[6], na.rm = TRUE) +
      ggplot2::geom_point(data = peaks, ggplot2::aes(x = .data$period, y = .data$power),
                          colour = accent[5], size = 2, na.rm = TRUE) +
      ggplot2::labs(x = "Period (hours)", y = "Power", title = nm) +
      ggplot2::coord_cartesian(xlim = c(0, xmax)) +
      theme_window()
    if (windowed) p <- p + ggplot2::facet_grid(window ~ ., switch = "y")
    p
  })
  stats::setNames(plots, names(by_ind))
}
