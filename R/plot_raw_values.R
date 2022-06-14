#' Plot Raw Values
#'
#' @param df Use the first output from 'simplify_data'.
#'
#' @return Returns a list of plots for the raw values.
#' @export
#'
#' @examples plots = plot_raw_values(df = simplified_data$data)
#'
#' @import magrittr
#' @importFrom dplyr select distinct filter mutate
#' @importFrom tidyr nest
#' @importFrom future plan sequential
#' @importFrom furrr future_map2
#' @importFrom ggplot2 ggplot geom_line geom_tile labs theme element_text element_blank element_line scale_fill_grey
plot_raw_values <- function(df) {
  #find out if there is Light/Dark data in the df
  light = 'ld' %in% names(df)



  #Preprocess the data to plot
  df_raw = df %>%
    select(data, datetime, any_of('ld'), raw_values) %>%
    distinct() %>%
    filter(datetime >= ceiling_date(min(datetime), unit = "1 day"))

  #If there is no Light/Dark data, skip
  if (light) {
    df_raw =  mutate(df_raw, ld = ifelse(ld == 0, 'Dark', 'Light')) %>%
      mutate(ld = factor(ld, levels = c('Dark', 'Light')))
  }

  #Finalize the data wrangling
  df_raw = df_raw %>%
    nest(cols = -data) %>%
    as.list()
  names(df_raw$cols) = df_raw$data
  id = df_raw$data
  df_raw = df_raw$cols

  # Plot the figures
  plan(sequential)
  raw_plots = future_map2(.x = df_raw,
                          .y = id,
                          .f = ~ {
                            ggplot(.x, aes(x = datetime, y = raw_values)) +
                              geom_line() +
                              scale_fill_grey(start = 0, end = 1) +
                              labs(y = 'Raw Values', title = .y, x = 'Datetime') +
                              theme(
                                panel.spacing = unit(0, "cm", data = NULL),
                                axis.title = element_text(face = "bold", size = 12),
                                axis.ticks = element_blank(),
                                axis.line = element_line(),
                                axis.line.x = element_line(),
                                axis.text.y = element_blank(),
                                axis.text.x = element_text(size = 12),
                                strip.background = element_blank(),
                                strip.placement = "outside",
                                strip.text.y.left = element_text(
                                  angle = 0,
                                  size = 12,
                                  vjust = 0
                                ),
                                strip.text.x = element_blank(),
                                plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
                                panel.border = element_blank(),
                                panel.background = element_blank(),
                                plot.title = element_text(hjust = 0.5, vjust =  -0.5),
                                legend.position = 'none'
                              ) +
                              if (light) {
                                geom_tile(aes(
                                  x = datetime,
                                  y = max(raw_values) / 2,
                                  height = max(raw_values),
                                  fill = ld
                                ),
                                alpha = 0.4)
                              }
                          })

  return(raw_plots)
}
