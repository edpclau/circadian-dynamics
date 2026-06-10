#' Polar (clock) plot of the cosinor acrophase with a joint confidence ellipse
#'
#' @description Shows the cosinor rhythm as a vector on a 24-h-style clock: the
#'   angle is the acrophase (time of peak), the length is the amplitude, and the
#'   shaded outline is the Bingham (1982) joint 95% confidence region for the
#'   (amplitude, acrophase) estimate. A rhythm is statistically present when the
#'   ellipse does not cover the origin (the zero-amplitude/no-rhythm test).
#'
#'   The clock is oriented with 0 h at the top and time increasing clockwise; a
#'   full turn equals the fitted period. The ellipse is computed in the Cartesian
#'   (cosine, sine)-coefficient plane and the plane is rigidly rotated to clock
#'   orientation, so the confidence region stays a true ellipse (no `coord_polar`
#'   distortion).
#'
#' @param df The `lombscargle` or `autocorrelation` table from [simplify_data()];
#'   must carry `cos_coeff`, `sin_coeff`, `var_sin`, `var_cos`, `cov_sincos`,
#'   `cosinor_n`, and `cosinor_period`.
#' @param level Confidence level for the joint ellipse. Default 0.95.
#' @return A named list of ggplots, one per individual. When the input is
#'   windowed, each window's vector and ellipse are drawn and coloured by window.
#' @export
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' plots <- plot_acrophase_polar(simplify_data(out)$lombscargle)
#' }
plot_acrophase_polar <- function(df, level = 0.95) {
  windowed <- "window" %in% names(df)
  accent <- okabe_ito(8)
  by_ind <- split(df, df$data)

  plots <- lapply(names(by_ind), function(nm) {
    rows <- by_ind[[nm]]
    if (windowed) {
      w <- as.numeric(rows$window)
      rows$wgrp <- factor(w, levels = sort(unique(w)))  # numeric, not string, order
    } else {
      rows$wgrp <- factor(1L)
    }
    per <- stats::median(rows$cosinor_period, na.rm = TRUE)
    if (!is.finite(per) || per <= 0) per <- 24

    rows <- rows[is.finite(rows$cos_coeff) & is.finite(rows$sin_coeff), , drop = FALSE]
    if (!nrow(rows)) return(NULL)

    # Vector tips: math point (cos_coeff, sin_coeff) -> clock coords by swapping axes
    # (0 h at top, clockwise). Rigid transform, so ellipses stay ellipses.
    vecs <- data.frame(wgrp = rows$wgrp, xend = rows$sin_coeff, yend = rows$cos_coeff)

    ell <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      if (!is.finite(r$var_sin) || !is.finite(r$cosinor_n) || r$cosinor_n <= 3) return(NULL)
      m <- .cosinor_ellipse(r$cos_coeff, r$sin_coeff, r$var_cos, r$var_sin,
                            r$cov_sincos, r$cosinor_n - 3, level)
      data.frame(wgrp = r$wgrp, x = m$y, y = m$x)  # swap to clock coords
    }))

    rmax <- max(c(sqrt(vecs$xend^2 + vecs$yend^2),
                  if (!is.null(ell)) sqrt(ell$x^2 + ell$y^2)), na.rm = TRUE)
    if (!is.finite(rmax) || rmax <= 0) rmax <- 1

    # Background: concentric amplitude rings + radial hour spokes with hour labels.
    rings <- pretty(c(0, rmax), 3); rings <- rings[rings > 0 & rings <= rmax * 1.05]
    circ <- do.call(rbind, lapply(rings, function(rr) {
      t <- seq(0, 2 * pi, length.out = 120)
      data.frame(x = rr * cos(t), y = rr * sin(t), r = rr)
    }))
    hh <- seq(0, per, length.out = 9)[1:8]
    spokes <- do.call(rbind, lapply(hh, function(h) {
      data.frame(x = c(0, rmax * sin(2 * pi * h / per)),
                 y = c(0, rmax * cos(2 * pi * h / per)), h = h)
    }))
    labs <- data.frame(x = rmax * 1.13 * sin(2 * pi * hh / per),
                       y = rmax * 1.13 * cos(2 * pi * hh / per),
                       lab = paste0(round(hh), "h"))

    p <- ggplot2::ggplot() +
      ggplot2::geom_path(data = circ, ggplot2::aes(.data$x, .data$y, group = .data$r), colour = "grey88") +
      ggplot2::geom_line(data = spokes, ggplot2::aes(.data$x, .data$y, group = .data$h), colour = "grey88") +
      ggplot2::geom_text(data = labs, ggplot2::aes(.data$x, .data$y, label = .data$lab),
                         colour = "grey40", size = 3)
    if (!is.null(ell)) {
      p <- p + ggplot2::geom_path(data = ell,
                                  ggplot2::aes(.data$x, .data$y, group = .data$wgrp, colour = .data$wgrp))
    }
    p <- p + ggplot2::geom_segment(
      data = vecs, ggplot2::aes(x = 0, y = 0, xend = .data$xend, yend = .data$yend, colour = .data$wgrp),
      arrow = grid::arrow(length = grid::unit(0.18, "cm")), linewidth = 0.8)

    p <- p +
      ggplot2::coord_fixed(clip = "off") +
      ggplot2::labs(title = nm,
                    subtitle = sprintf("acrophase vector + %d%% joint CI (period %.1f h)",
                                       round(level * 100), per)) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
                     legend.position = if (windowed) "right" else "none")

    if (windowed) {
      p + ggplot2::scale_colour_viridis_d(name = "Window")
    } else {
      p + ggplot2::scale_colour_manual(values = accent[5], guide = "none")
    }
  })
  stats::setNames(plots, names(by_ind))
}

# 95% (or `level`) joint confidence ellipse for a cosinor fit, in the Cartesian
# (cosine, sine)-coefficient plane. Boundary of {z : (z-mu)' Sigma^-1 (z-mu) <= 2 F},
# the Bingham (1982) region scaled by sqrt(2 * F_{2, df_resid, level}).
.cosinor_ellipse <- function(cx, cy, var_x, var_y, cov_xy, df_resid, level = 0.95, n = 120) {
  Sigma <- matrix(c(var_x, cov_xy, cov_xy, var_y), 2, 2)
  e <- eigen(Sigma, symmetric = TRUE)
  e$values[e$values < 0] <- 0
  c_scale <- sqrt(2 * stats::qf(level, 2, df_resid))
  t <- seq(0, 2 * pi, length.out = n)
  pts <- (e$vectors %*% diag(sqrt(e$values), 2, 2) * c_scale) %*% rbind(cos(t), sin(t))
  data.frame(x = cx + pts[1, ], y = cy + pts[2, ])
}
