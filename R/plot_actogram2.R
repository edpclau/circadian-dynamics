#' Plot Actograms
#'
#' @param df The 'data' output from simplify_data
#'
#' @return a list of actograms
#'
#'
#' @importFrom dplyr distinct filter select distinct
#' @importFrom tidyr nest
#' @importFrom lubridate days
#' @importFrom future sequential plan
#' @importFrom furrr future_map_dfr
#'
#' @export
#'
#'


plot_actogram2 <- function(df) {
### Check for windows and ld data
windows = 'window' %in% names(df)
lights = 'ld' %in% names(df)

#### If there are windows, drop them and then make 2 day windows ####
#Actograms have a 2 day window
# Set parameters

if (windows) {
  df = select(df, -window)
  df = distinct(df)
}

## Start at the first midnight ##
df = filter(df, datetime >= ceiling_date(min(datetime), unit = "1 day"))

window_size <- days(2) #Width of the window
times <- unique(df$datetime)
step = seq(from = min(times), to = max(times), by = '1 day')

plan(sequential)
df = future_map_dfr(
  .x = step,
  .f = ~ filter(df, (datetime >= .x) & (datetime <= .x + window_size)),
  .id = 'window'
  )

### Prepare Data
## Build Actogram
df_acto = df %>%
  select(-any_of(c('detrended', 'butterworth', 'lomb_cosinor', 'autocorr_cosinor'))) %>%
  distinct() %>%
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
  plan(sequential)
  #Data for Actogram
  return(
    future_map2(
      .x = df_acto,
      .y = id,
      .f = ~ {
        ggplot(.x, aes(x = dur, y = raw_values/2, height = raw_values)) +
          geom_tile(aes(height = max(raw_values), y= max(raw_values)/2, fill = ld), colour = NA, alpha = 0.3) +
          geom_tile(colour = NA) +
          geom_vline(aes(xintercept = 24 - 0.025), col = 'blue') +
          facet_grid(window ~ ., switch = 'y') +
          labs(x = 'Hours', y = '', title = .y) +
          scale_fill_grey(start = 0, end = 1) +
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
            strip.text.y.left = element_text(angle = 0, size = 12, vjust = 0),
            strip.text.x = element_blank(),
            plot.margin = unit(c(0,0.5,0,0), "cm"),
            panel.border= element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust =  - 0.5)
          )


      }
    )

  )
} else {
  plan(sequential)
  #Data for Actogram
  return(
    future_map2(
      .x = df_acto,
      .y = id,
      .f = ~ {ggplot(.x, aes(x = dur, y = raw_values/2, height = raw_values)) +
          geom_tile() +
          geom_vline(aes(xintercept = 24 - 0.025), col = 'blue') +
          facet_grid(window ~ ., switch = 'y') +
          labs(x = 'Hours', y = '', title = .y, fill = 'Dark/Light') +
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
            strip.text.y.left = element_text(angle = 0, size = 12, vjust = 0),
            strip.text.x = element_blank(),
            plot.margin = unit(c(0,0.5,0,0), "cm"),
            panel.border= element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust =  - 0.5)
          )
      }
    )

  )
}

}


