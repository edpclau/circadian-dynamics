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

# One line+point plot of `y` versus window, per individual.
.window_metric <- function(nested, y, title, ylab = "", hline = NULL) {
  furrr::future_map(nested, function(d) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$window, y = .data[[y]])) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::labs(x = "Window", y = ylab, title = title) +
      theme_window()
    if (!is.null(hline)) p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dashed")
    p
  })
}
