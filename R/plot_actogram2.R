#' Plot Actograms
#'
#' @param df The 'data' output from simplify_data
#'
#' @return a list of actograms
#'
#'
#' @importFrom dplyr distinct filter select
#' @importFrom tidyr nest
#' @importFrom lubridate days
#'
#' @export
#'
#'


plot_actogram2 <- function(df) {
### Check for windows and ld data
windows = 'window' %in% names(df)
lights = 'ld' %in% names(df)

#### If there are no windows, make 2 day windows ####
#Actograms have a 2 day window
# Set parameters

if (!windows) {
window_size <- days(2) #Width of the window
times <- unique(df$datetime)
step = seq(from = min(times), to = max(times), by = '1 day')

df = purrr::map_dfr(
  .x = step,
  .f = ~ filter(df, (datetime >= .x) & (datetime <= .x + window_size)),
  .id = 'window'
  )
}

### Prepare Data
## Build Actogram
df_acto = df %>%
  select(-any_of(c('detrended', 'butterworth', 'lomb_cosinor', 'autocorr_cosinor'))) %>%
  distinct() %>%
  filter(datetime >= ceiling_date(min(datetime), unit = "1 day")) %>%
  group_by(data, window) %>%
  mutate(
    window = as.numeric(window),
    dur = as.numeric(duration('1 hour') * seq(0, n()-1, by = 1), 'hours'),
    xlabs = dur%%24,
    across(any_of('ld'), ~ ifelse(.x == 0, 'Dark', 'Light'))
  ) %>%
  ungroup()  %>%
  nest(cols = -data) %>%
  as.list()
names(df_acto$cols) = df_acto$data
id = df_acto$data
df_acto = df_acto$cols



if (lights) {
  return(plot_ld_actogram(df_acto, id))
} else {
  return(plot_non_ld_actogram(df_acto, id))
}

}


