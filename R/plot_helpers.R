# Shared scaffolding for the window-metric result plots
# (plot_acf_results / plot_lsp_results). Internal.

# The common theme, formerly copy-pasted into every panel.
theme_window <- function(base_size = 12) {
  ggplot2::theme(
    panel.spacing = ggplot2::unit(0, "cm"),
    axis.title = ggplot2::element_text(face = "bold", size = base_size),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = base_size),
    strip.background = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.text.y.left = ggplot2::element_text(angle = 0, size = base_size, vjust = 0),
    plot.margin = ggplot2::unit(c(0, 0.5, 0, 0), "cm"),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5, vjust = -0.5),
    legend.position = "none"
  )
}

# Nest a simplify_data result table into one data.frame per individual.
.prep_window_df <- function(df, cols) {
  if (!"window" %in% names(df)) df$window <- 1
  df <- dplyr::distinct(dplyr::select(df, dplyr::all_of(c("data", "window", cols))))
  df$window <- as.numeric(df$window)
  nested <- tidyr::nest(df, metrics = -data)
  stats::setNames(nested$metrics, nested$data)
}

# One line+point plot of `y` versus window, per individual. When `se` names a
# standard-error column present in the data, a 95% CI (y +/- 1.96*se) error bar is
# drawn under the points (uncertainty is shown explicitly, never inferred from overlap).
.window_metric <- function(nested, y, title, ylab = "", hline = NULL, se = NULL) {
  furrr::future_map(nested, function(d) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$window, y = .data[[y]]))
    if (!is.null(se) && se %in% names(d)) {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[y]] - 1.96 * .data[[se]],
                     ymax = .data[[y]] + 1.96 * .data[[se]]),
        width = 0.2, colour = "grey40", na.rm = TRUE)
    }
    p <- p +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::labs(x = "Window", y = ylab, title = title) +
      theme_window()
    if (!is.null(hline)) p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dashed")
    p
  })
}

# The exact inline theme shared by plot_raw_values() + plot_actogram_windows().
# Distinct from theme_window(): visible axis lines, no panel border, hidden y-axis text.
# legend = FALSE additionally suppresses the legend (used by plot_raw_values).
theme_actogram <- function(base_size = 12, legend = TRUE) {
  th <- ggplot2::theme(
    panel.spacing = ggplot2::unit(0, "cm"),
    axis.title = ggplot2::element_text(face = "bold", size = base_size),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(),
    axis.line.x = ggplot2::element_line(),
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = base_size),
    strip.background = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.text.y.left = ggplot2::element_text(angle = 0, size = base_size, vjust = 0),
    strip.text.x = ggplot2::element_blank(),
    plot.margin = ggplot2::unit(c(0, 0.5, 0, 0), "cm"),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5, vjust = -0.5)
  )
  if (!legend) th <- th + ggplot2::theme(legend.position = "none")
  th
}

# Contiguous dark-phase intervals, for shading the dark phase as a full-height
# background band (the standard chronobiology idiom, vs. variable-height tiles).
# `times` is ordered by time; `is_dark` a logical of equal length (NA treated as
# not-dark). Returns a data.frame of xmin/xmax spanning each run of darkness.
.ld_intervals <- function(times, is_dark) {
  is_dark <- !is.na(is_dark) & is_dark
  if (!any(is_dark)) return(data.frame(xmin = times[0], xmax = times[0]))
  r <- rle(is_dark)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  d <- which(r$values)
  data.frame(xmin = times[starts[d]], xmax = times[ends[d]])
}

# Named list of one data.frame per individual, keyed by the `data` column. Replaces the
# nest()/as.list()/names<- dance previously repeated in both per-individual plotters.
.nest_by_individual <- function(df) {
  nested <- tidyr::nest(df, cols = -data)
  stats::setNames(nested$cols, nested$data)
}
