#' Double-plotted actogram (ggplot2) with activity onsets and free-running period
#'
#' @description Publication-style double-plotted actogram: each row spans 48 h with
#'   day N on the left and day N+1 on the right, days stacked top to bottom. Activity
#'   bars share one global y-scale across all days (so day-to-day amplitude is
#'   comparable). Activity onsets (see [detect_onsets()]) are marked and joined by a
#'   line whose slope is the free-running period tau (see [estimate_tau()]); leftward
#'   drift means tau < 24 h, rightward means tau > 24 h.
#'
#' @param df A `data`-style table (e.g. `simplify_data(out)$data`) with columns
#'   `data` (individual id), `datetime`, and `raw_values`.
#' @param sampling_rate Character sampling rate, e.g. `"30 minutes"` (passed to
#'   [detect_onsets()]).
#' @param threshold Onset threshold, forwarded to [detect_onsets()] (`NULL` = mean).
#' @param onsets Logical; draw onset markers and the tau line. Default `TRUE`.
#' @param lane Fraction of a day-row's height the tallest bar fills (0-1). Default 0.9.
#' @return A named list of ggplots, one double-plotted actogram per individual.
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr distinct select any_of
#' @examples
#' \dontrun{
#' plots <- plot_actogram(simplify_data(out)$data, sampling_rate = "15 minutes")
#' }
plot_actogram <- function(df, sampling_rate, threshold = NULL, onsets = TRUE, lane = 0.9) {
  df <- dplyr::distinct(dplyr::select(df, dplyr::any_of(c("data", "datetime", "raw_values", "ld"))))
  light <- "ld" %in% names(df)
  accent <- okabe_ito(8)
  by_ind <- split(df, df$data)

  plots <- lapply(names(by_ind), function(nm) {
    g <- by_ind[[nm]]
    g <- g[order(g$datetime), ]
    t0 <- min(g$datetime)
    hours <- as.numeric(difftime(g$datetime, t0, units = "hours"))
    day <- floor(hours / 24)
    tod <- hours - day * 24
    maxday <- max(day)

    gmax <- max(g$raw_values, na.rm = TRUE)
    if (!is.finite(gmax) || gmax <= 0) gmax <- 1
    a <- pmin(pmax(g$raw_values, 0) / gmax, 1) * lane  # global, consistent scaling

    # Double-plot: each day's data appears at x in [0,24) of its own row and at
    # x+24 in [24,48) of the previous row. Bars grow up from each row's baseline (-row).
    bars <- rbind(
      data.frame(row = day,     x = tod,      a = a),
      data.frame(row = day - 1, x = tod + 24, a = a)
    )
    bars <- bars[bars$row >= 0 & bars$row <= maxday & is.finite(bars$a), ]

    p <- ggplot2::ggplot()

    # Dark-phase shading: grey background bands filling each day's lane, in both
    # double-plot halves (the standard chronobiology "night shading").
    if (light) {
      gg <- data.frame(day = day, tod = tod, dark = (g$ld == 0))
      gg$dark[is.na(gg$dark)] <- FALSE
      bands <- do.call(rbind, lapply(split(gg, gg$day), function(dd) {
        dd <- dd[order(dd$tod), ]
        iv <- .ld_intervals(dd$tod, dd$dark)
        if (!nrow(iv)) return(NULL)
        iv$day <- dd$day[1]; iv
      }))
      if (!is.null(bands) && nrow(bands)) {
        dark_rects <- rbind(
          data.frame(xmin = bands$xmin,      xmax = bands$xmax,      row = bands$day),
          data.frame(xmin = bands$xmin + 24, xmax = bands$xmax + 24, row = bands$day - 1))
        dark_rects <- dark_rects[dark_rects$row >= 0 & dark_rects$row <= maxday, ]
        p <- p + ggplot2::geom_rect(data = dark_rects,
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                       ymin = -.data$row, ymax = -.data$row + 1),
          fill = "grey85", inherit.aes = FALSE)
      }
    }

    p <- p +
      ggplot2::geom_segment(
        data = bars,
        ggplot2::aes(x = .data$x, xend = .data$x, y = -.data$row, yend = -.data$row + .data$a),
        linewidth = 0.3) +
      ggplot2::geom_vline(xintercept = 24, colour = "grey80", linewidth = 0.4)

    tau <- list(tau = NA_real_, n = 0L)
    if (onsets) {
      # Pass activity explicitly: detect_onsets() uses the last column, and `g` may
      # carry an `ld` column that must NOT be mistaken for the activity signal.
      on <- detect_onsets(data.frame(datetime = g$datetime, value = g$raw_values),
                          sampling_rate, threshold = threshold)
      tau <- estimate_tau(on)
      if (nrow(on)) {
        oh <- as.numeric(difftime(on$onset_datetime, t0, units = "hours"))
        oday <- floor(oh / 24); otod <- oh - oday * 24
        opts <- rbind(data.frame(row = oday, x = otod),
                      data.frame(row = oday - 1, x = otod + 24))
        opts <- opts[opts$row >= 0 & opts$row <= maxday, ]
        p <- p + ggplot2::geom_point(data = opts,
          ggplot2::aes(x = .data$x, y = -.data$row), colour = accent[6], size = 1.1)

        # Straight tau regression line through the onsets: fitted onset time per
        # onset, placed at its row's clock-time and drawn in both double-plot halves.
        if (is.finite(tau$tau)) {
          fh <- tau$intercept + tau$tau * on$onset_index   # fitted onset hours
          ftod <- fh - 24 * oday
          fitline <- rbind(data.frame(row = oday,     x = ftod,      copy = "L"),
                           data.frame(row = oday - 1, x = ftod + 24, copy = "R"))
          fitline <- fitline[fitline$row >= 0 & fitline$row <= maxday &
                               fitline$x >= 0 & fitline$x <= 48, ]
          fitline <- fitline[order(fitline$copy, fitline$row), ]
          p <- p + ggplot2::geom_line(data = fitline,
            ggplot2::aes(x = .data$x, y = -.data$row, group = .data$copy),
            colour = accent[6], linewidth = 0.6)
        }
      }
    }

    sub <- if (is.finite(tau$tau)) sprintf("onsets: %d    tau = %.2f h", tau$n, tau$tau)
           else "onsets: insufficient for tau"
    p +
      ggplot2::scale_x_continuous(breaks = seq(0, 48, 6), limits = c(0, 48), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = -(0:maxday), labels = (0:maxday) + 1, expand = c(0.01, 0)) +
      ggplot2::labs(x = "Time (hours, double-plotted)", y = "Day", title = nm,
                    subtitle = if (onsets) sub else NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9))
  })
  stats::setNames(plots, names(by_ind))
}
