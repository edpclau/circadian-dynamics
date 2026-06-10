#' Plot Raw Values
#'
#' @param df Use the first output from 'simplify_data'.
#'
#' @return Returns a list of plots for the raw values.
#' @export
#'
#' @examples
#' \dontrun{
#' plots = plot_raw_values(df = simplified_data$data)
#' }
#'
#' @import magrittr
#' @importFrom dplyr select distinct filter mutate
#' @importFrom tidyr nest
#' @importFrom furrr future_map2
plot_raw_values <- function(df) {
  light <- 'ld' %in% names(df)

  df_raw <- df %>%
    select(data, datetime, any_of('ld'), raw_values) %>%
    distinct() %>%
    filter(datetime >= ceiling_date(min(datetime), unit = "1 day"))

  if (light) {
    df_raw <- mutate(df_raw, ld = factor(ifelse(ld == 0, 'Dark', 'Light'),
                                         levels = c('Dark', 'Light')))
  }

  df_raw <- .nest_by_individual(df_raw)
  id <- names(df_raw)

  # One line plot per individual; when ld is present the dark phase is shaded as a
  # full-height background band behind the trace (consistent y-scale, no tile hack).
  future_map2(.x = df_raw, .y = id, .f = ~ {
    p <- ggplot2::ggplot(.x, ggplot2::aes(x = datetime, y = raw_values))
    if (light) {
      dark <- .ld_intervals(.x$datetime, .x$ld == 'Dark')
      if (nrow(dark)) {
        p <- p + ggplot2::geom_rect(
          data = dark,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          inherit.aes = FALSE, fill = 'grey80')
      }
    }
    p +
      ggplot2::geom_line() +
      ggplot2::labs(y = 'Raw Values', title = .y, x = 'Datetime') +
      theme_actogram(legend = FALSE)
  })
}
