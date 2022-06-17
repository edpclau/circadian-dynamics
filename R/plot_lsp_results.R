#' Plot Lomb-Scargle Results
#'
#' @param df Use the third output from 'simplify_data'.
#'
#' @return Returns a list of plots for the autocorrelation results.
#' @export
#'
#' @examples plots = plot_lsp_results(df = simplified_data$lombscargle)
#'
#' @import magrittr
#' @importFrom dplyr select distinct filter mutate
#' @importFrom tidyr nest
#' @importFrom future plan sequential
#' @importFrom furrr future_map2
#' @importFrom ggplot2 ggplot geom_line labs theme element_text element_blank element_line scale_x_continuous element_rect geom_hline

plot_lsp_results <- function(df) {

  if (!'window' %in% names(df)) {df$window = 1}
  #Preprocess the data to plot
  lsp_df = df %>%
    select(data, window, rythm_strength, gc_raw_to_cos, gc_cos_to_raw, period, amplitude, phase) %>%
    distinct() %>%
    mutate(window = as.numeric(window),
           granger = ifelse(gc_raw_to_cos < gc_cos_to_raw, gc_raw_to_cos, gc_cos_to_raw)) %>%
    nest(cols = -data) %>%
    as.list()
  names(lsp_df$cols) = lsp_df$data
  id = lsp_df$data
  lsp_df = lsp_df$cols

  # Plot the figures
  plan(sequential)
  ### Period ####
  period_plots = future_map2(.x = lsp_df,
                             .y = id,
                             .f = ~ {
                               ggplot(.x, aes(x = window, y = period)) +
                                 geom_point(na.rm = TRUE) +
                                 geom_line(na.rm = TRUE) +
                                 labs(y = 'Hours', title = 'Lomb-Scargle Period', x = 'Window') +
                                 scale_x_continuous(labels = window, limits = c(NA,NA)) +
                                 theme(
                                   panel.spacing = unit(0, "cm", data = NULL),
                                   axis.title = element_text(face = "bold", size = 12),
                                   # axis.ticks = element_blank(),
                                   axis.line = element_blank(),
                                   axis.line.x = element_blank(),
                                   axis.text.y = element_text(size = 12),
                                   axis.text.x = element_text(size = 12),
                                   strip.background = element_blank(),
                                   strip.placement = "outside",
                                   strip.text.y.left = element_text(
                                     angle = 0,
                                     size = 12,
                                     vjust = 0
                                   ),
                                   plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
                                   panel.border = element_rect(colour = "black", fill=NA, size = 1),
                                   panel.background = element_blank(),
                                   plot.title = element_text(hjust = 0.5, vjust =  -0.5),
                                   legend.position = 'none'
                                 )
                             })
  ### Rhythms ###
  rhythm_plots = future_map2(.x = lsp_df,
                             .y = id,
                             .f = ~ {
                               ggplot(.x, aes(x = window, y = rythm_strength)) +
                                 geom_point(na.rm = TRUE) +
                                 geom_line(na.rm = TRUE) +
                                 geom_hline(aes(yintercept = 1), lty = 'dashed') +
                                 labs(y = '', title = 'Lomb-Scargle Rhythm Strength', x = 'Window') +
                                 scale_x_continuous(labels = window, limits = c(NA,NA)) +
                                 theme(
                                   panel.spacing = unit(0, "cm", data = NULL),
                                   axis.title = element_text(face = "bold", size = 12),
                                   # axis.ticks = element_blank(),
                                   axis.line = element_blank(),
                                   axis.line.x = element_blank(),
                                   axis.text.y = element_text(size = 12),
                                   axis.text.x = element_text(size = 12),
                                   strip.background = element_blank(),
                                   strip.placement = "outside",
                                   strip.text.y.left = element_text(
                                     angle = 0,
                                     size = 12,
                                     vjust = 0
                                   ),
                                   plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
                                   panel.border = element_rect(colour = "black", fill=NA, size = 1),
                                   panel.background = element_blank(),
                                   plot.title = element_text(hjust = 0.5, vjust =  -0.5),
                                   legend.position = 'none'
                                 )
                             })
  ### Granger ###
  granger_plots = future_map2(.x = lsp_df,
                              .y = id,
                              .f = ~ {
                                ggplot(.x, aes(x = window, y = granger)) +
                                  geom_point(na.rm = TRUE) +
                                  geom_line(na.rm = TRUE) +
                                  geom_hline(aes(yintercept = 0.05), lty = 'dashed') +
                                  labs(y = 'p.value', title = 'Lomb-Scargle Granger Test', x = 'Window') +
                                  scale_x_continuous(labels = window, limits = c(NA,NA)) +
                                  theme(
                                    panel.spacing = unit(0, "cm", data = NULL),
                                    axis.title = element_text(face = "bold", size = 12),
                                    # axis.ticks = element_blank(),
                                    axis.line = element_blank(),
                                    axis.line.x = element_blank(),
                                    axis.text.y = element_text(size = 12),
                                    axis.text.x = element_text(size = 12),
                                    strip.background = element_blank(),
                                    strip.placement = "outside",
                                    strip.text.y.left = element_text(
                                      angle = 0,
                                      size = 12,
                                      vjust = 0
                                    ),
                                    plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
                                    panel.border = element_rect(colour = "black", fill=NA, size = 1),
                                    panel.background = element_blank(),
                                    plot.title = element_text(hjust = 0.5, vjust =  -0.5),
                                    legend.position = 'none'
                                  )
                              })
  ### Amplitude ###
  amplitude_plots = future_map2(.x = lsp_df,
                              .y = id,
                              .f = ~ {
                                ggplot(.x, aes(x = window, y = amplitude)) +
                                  geom_point(na.rm = TRUE) +
                                  geom_line(na.rm = TRUE) +
                                  labs(y = '', title = 'Cosinor Amplitude (lsp)', x = 'Window') +
                                  scale_x_continuous(labels = window, limits = c(NA,NA)) +
                                  theme(
                                    panel.spacing = unit(0, "cm", data = NULL),
                                    axis.title = element_text(face = "bold", size = 12),
                                    # axis.ticks = element_blank(),
                                    axis.line = element_blank(),
                                    axis.line.x = element_blank(),
                                    axis.text.y = element_text(size = 12),
                                    axis.text.x = element_text(size = 12),
                                    strip.background = element_blank(),
                                    strip.placement = "outside",
                                    strip.text.y.left = element_text(
                                      angle = 0,
                                      size = 12,
                                      vjust = 0
                                    ),
                                    plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
                                    panel.border = element_rect(colour = "black", fill=NA, size = 1),
                                    panel.background = element_blank(),
                                    plot.title = element_text(hjust = 0.5, vjust =  -0.5),
                                    legend.position = 'none'
                                  )
                              })

  ### Phase ###
  phase_plots = future_map2(.x = lsp_df,
                                .y = id,
                                .f = ~ {
                                  ggplot(.x, aes(x = window, y = phase)) +
                                    geom_point(na.rm = TRUE) +
                                    geom_line(na.rm = TRUE) +
                                    labs(y = '', title = 'Cosinor Phase (lsp)', x = 'Window') +
                                    scale_x_continuous(labels = window, limits = c(NA,NA)) +
                                    theme(
                                      panel.spacing = unit(0, "cm", data = NULL),
                                      axis.title = element_text(face = "bold", size = 12),
                                      # axis.ticks = element_blank(),
                                      axis.line = element_blank(),
                                      axis.line.x = element_blank(),
                                      axis.text.y = element_text(size = 12),
                                      axis.text.x = element_text(size = 12),
                                      strip.background = element_blank(),
                                      strip.placement = "outside",
                                      strip.text.y.left = element_text(
                                        angle = 0,
                                        size = 12,
                                        vjust = 0
                                      ),
                                      plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
                                      panel.border = element_rect(colour = "black", fill=NA, size = 1),
                                      panel.background = element_blank(),
                                      plot.title = element_text(hjust = 0.5, vjust =  -0.5),
                                      legend.position = 'none'
                                    )
                                })
  return(list(
    period_plots = period_plots,
    rhythm_plots = rhythm_plots,
    granger_plots = granger_plots,
    amplitude_plots = amplitude_plots,
    phase_plots = phase_plots
  ))
}
