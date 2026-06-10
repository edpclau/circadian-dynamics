#' Plot Window Actogram
#'
#' @param df The 'data' output from simplify_data
#'
#' @return a list of actograms binned by window
#'
#'
#' @importFrom dplyr distinct filter select distinct
#' @importFrom tidyr nest
#' @importFrom lubridate days
#'
#' @export
#'
#'
#'
plot_actogram_windows <- function(df) {
  ### Check for windows and ld data
  windows <- 'window' %in% names(df)
  lights <- 'ld' %in% names(df)

  #### If there are no windows, split into 2-day actogram windows ####
  if (!windows) {
    window_size <- days(2)
    times <- unique(df$datetime)
    step <- seq(from = min(times), to = max(times), by = '1 day')
    df <- purrr::map_dfr(
      .x = step,
      .f = ~ filter(df, (datetime >= .x) & (datetime <= .x + window_size)),
      .id = 'window'
    )
  }

  ### Prepare Data
  df_acto <- df %>%
    select(-any_of(c('detrended', 'butterworth', 'lomb_cosinor', 'autocorr_cosinor'))) %>%
    distinct() %>%
    filter(datetime >= ceiling_date(min(datetime), unit = "1 day")) %>%
    group_by(data, window) %>%
    mutate(
      window = as.numeric(window),
      # Elapsed hours from the start of each window, taken straight from the
      # datetime column (correct for any sampling rate, not just hourly data).
      dur = as.numeric(difftime(datetime, min(datetime), units = 'hours')),
      xlabs = ifelse(dur %% 24 == 0 & dur != 0, dur - 0.25, NA),
      across(any_of('ld'), ~ ifelse(.x == 0, 'Dark', 'Light'))
    ) %>%
    ungroup()

  df_acto <- .nest_by_individual(df_acto)
  id <- names(df_acto)

  # One actogram per individual; the light/dark shading tile is added only when ld is present.
  future_map2(.x = df_acto, .y = id, .f = ~ {
    p <- ggplot(.x, aes(x = dur, y = raw_values / 2, height = raw_values))
    if (lights) {
      p <- p + geom_tile(aes(height = max(raw_values), y = max(raw_values) / 2, fill = ld),
                         colour = NA, alpha = 0.3)
    }
    p +
      geom_tile() +
      geom_vline(aes(xintercept = xlabs), col = 'blue') +
      facet_grid(window ~ ., switch = 'y') +
      labs(x = 'Hours', y = '', title = .y) +
      scale_fill_grey() +
      theme_actogram()
  })
}
