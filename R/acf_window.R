#' Auto Correlation with a moving window
#'
#' @description Uses autocorrelation to find a circadian period for a given timeseries
#'
#' @usage function(df = NULL,  from = 18, to = 30,
#' sampling_rate = "1 hour", window_vector = NULL, values = NULL)
#'
#' @param df A data.frame with 2 columns. Column 1 must contain the windows to iterate over.
#' Column 2 must supply the values. This parameter is optional if window_vector and values are supplied.
#' df must not have gaps in the dates, acf asumes data is evenly spaced.
#'
#' @param from The period (in hours) from which to start looking for peaks in the autocorrelation. Default = 18.
#'
#' @param to The period (in hours) up to which  to look for peaks in the autocorrelation. Default = 30.
#'
#' @param sampling_rate A charater string which indicates the sampling rate of the data.
#' For example: "1 second", "2 minutes", "1 hour" (default),"3 days", "11 months".
#'
#' @param window_vector A vector containing the windows to iterate over and to label the group
#' to which each value belongs. Usually this will be the output from make_time_windows.
#'
#' @param values The data for which we want to find the period.
#'
#' @return A data.frame with the autocorrelation results for each window which include: period, peaks,
#' power, lags for the peaks.
#'
#' @seealso [stats::acf()] which this functions uses to run the autocorrelation.
#'
#' @export
#'
#' @examples
#' autocorrelations_multipeak <- acf_window(df = df_with_windows,
#' multipeak_period = FALSE, peak_of_interest = 2,
#' sampling_unit = "hours")
#'
#' @importFrom dplyr if_else pull filter
#' @importFrom purrr map map2 map_if
#' @importFrom lubridate duration
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @importFrom pracma findpeaks movavg
#' @importFrom rlang is_empty
#' @importFrom magrittr "%>%"
#'
acf_window <- function(df = NULL,  from = 18, to = 30,
                       sampling_rate = "1 hour", window_vector = NULL, values = NULL) {

##### Flow Control Parameters #####
  #1. Either a df or two lists with the time_series and values must be supplied. If a df is supplied,
  # Change the names of the df columns so we can work with it inside the function. If a df is not supplied, turn the
  # two lists into a df.
  if (!is.null(df)) {
    names(df) <- c("window_vector", "values")
  } else if (is.null(df) & (is.null(window_vector) | is.null(values))) {
    stop("If a data.frame is not supplied. Must include both window_vector and values.")
  } else if (is.null(df)) { df = tibble(window_vector, values) }

  #2. Sampling_rate must exist
  if (is.null(sampling_rate)){
    stop("must provide a sampling_rate")
  } else {
    sampling_rate = str_remove(sampling_rate, "\\d.")
  }

  #3. the period must be calculated from the data on the second day.
  from = from + 24
  to = to + 24

########## Autocorrelation for a moving window of values ########

# Get the autocorrelation values
autocorrelations <- purrr::map(unique(df$window_vector), # Iterate over the windows
                               .f =  ~ as.numeric(acf(x = dplyr::pull(dplyr::filter(df, window_vector == .), values), #Select the values in the window
                                                      y = dplyr::pull(dplyr::filter(df, window_vector == .), values),
                                                      na.action = na.pass,
                                                      type = "correlation",
                                                      plot = FALSE,
                                                      lag.max = length(dplyr::pull(dplyr::filter(df, window_vector == .), values)))$acf)) #$acf is to keep only the acf values. We can infer lag position from those



# Case when we are interested in looking at a specific range of lags/periods.
if (!is.null(to) &  !is.null(from) ) {
autocorrelations <- purrr::map(unique(df$window_vector),
~ autocorrelations[[.]][(as.numeric(lubridate::duration(from, "hours"), sampling_rate) + 1):(as.numeric(lubridate::duration(to, "hours"), sampling_rate) + 1)]
)
} else if (!is.null(to) & is.null(from)) {
 autocorrelations <- purrr::map(unique(df$window_vector),
~ autocorrelations[[.]][1:(as.numeric(lubridate::duration(to, "hours"), sampling_rate + 1))]
)
} else if (is.null(to) & !is.null(from)) {
autocorrelations <-  purrr::map(unique(df$window_vector),
 ~  autocorrelations[[.]][(as.numeric(lubridate::duration(from, "hours"), sampling_rate) + 1):length(autocorrelations[[.]])]
  )
  }


# Change change all NA autocorrelations to 0
# The function that looks for peaks doesn't allow NA, 0 would be ignored if we put a threshold, therefoere it won't affect
# the results
autocorrelations_no_na <- purrr::map(autocorrelations,  .f =  ~ dplyr::if_else(is.na(.) == TRUE, 0, .))



# Find the peaks
peaks <- purrr::map(autocorrelations_no_na, .f = ~ pracma::findpeaks(.)[,1])
positive_peak_index <- purrr::map(1:length(peaks),
                      .f = ~ which(peaks[[.]] > 0))

# Keep only the positive peaks
positive_peaks <-  purrr::map2(peaks, positive_peak_index,
     .f = ~ .x[.y])

#handle when there are less positive peaks than the peak_of_interest and multivariate_peaks = FALSE
#In that case,don't draw any peaks.
# Find the average peak size (autopower)
mean_peaks <- purrr::map(positive_peaks, .f = ~ if(!(rlang::is_empty(.))) { max(.)} ) # We use map_if so that we don't process the NULL values
mean_peak_index <- purrr::map(unique(df$window_vector), ~ which(positive_peaks[[.]] == mean_peaks[[.]]))


############### Find lags in the autocorrelation #####
if (is.null(from) & is.null(to)) {

# Get the lag that corresponds to each peak
lags <- purrr::map(unique(df$window_vector), # Iterate over the windows
                   .f =  ~ seq(from = 0,
                               to = length(dplyr::pull(dplyr::filter(df, window_vector == .), values)) - 1,
                               by = 1))
} else if (!is.null(to) &  !is.null(from) ) {
  lags <- purrr::map(unique(df$window_vector),
                     .f =  ~ seq(from = as.numeric(lubridate::duration(from, "hours"), sampling_rate),
                                 to = as.numeric(lubridate::duration(to, "hours"), sampling_rate),
                                 by = 1)
                     )

} else if (!is.null(to) & is.null(from)) {
  lags <- purrr::map(unique(df$window_vector),
                     .f =  ~ seq(from = 0,
                                 to = as.numeric(lubridate::duration(to, "hours"), sampling_rate),
                                 by = 1))

} else if (is.null(to) & !is.null(from)) {
  lags <- purrr::map(unique(df$window_vector),
                     .f =  ~ seq(from = as.numeric(lubridate::duration(from, "hours"), sampling_rate) - 1,
                                 to = length(dplyr::pull(dplyr::filter(df, window_vector == .), values)) - 1,
                                 by = 1))
}

peak_index <- purrr::map(autocorrelations_no_na, .f = ~ pracma::findpeaks(.)[,2])
peak_lags <- purrr::map2(lags, peak_index, .f = ~ .x[.y])



# Positive Peak Lags
pos_peak_lag_index <- purrr::map2(peak_index, positive_peak_index, .f = ~ .x[.y])
pos_peak_lags <- purrr::map2(lags, pos_peak_lag_index, .f = ~ .x[.y])



#### The period ####
# First, find the windows with null and empty values by turning mean_peaks into a tibble. This will eliminate all the null and is.numeric(0) values

names(mean_peaks) <- seq_along(mean_peaks)



windows <- mean_peaks %>% names() %>% as.numeric()



# Now calculate the period.

period <- purrr::map_if(usable_windows,
                        .p = ~ !(is.null(mean_peaks[[.]])), # If the mean_peaks (autopower) is doesn't exist, give out an NA
                        .else =  ~ NA,
                        .f = ~  pos_peak_lags[[.]][mean_peak_index[[.]]]/2
)






# Transform the period into hours
period_hours <- map_if(1:length(period),
                    .p = ~ !is.na(period[[.]]), # If the period is NA, don't turn it into hours
                    .else =  ~ NA,
                    .f = ~ lubridate::duration(period[[.]], sampling_rate) %>% as.numeric("hours") %>% paste("hours")
    )


###### Arranging Data #####
autocorrelation_power <- map(1:length(mean_peaks), ~ ifelse(is.null(mean_peaks[[.]]), NA, mean_peaks[[.]]))

usable_peak_lags <- map(usable_windows, ~ pos_peak_lags[[.]][mean_peak_index[[.]]])
usable_peaks <- map(usable_windows, ~ positive_peaks[[.]][mean_peak_index[[.]]])




##### Results ####
results <- tibble::tibble(window = usable_windows,
                          period = unlist(period),
                          period_hours = unlist(period_hours),
                          autocorrelation_power = unlist(autocorrelation_power),
                          peak_lags = usable_peak_lags,
                          peaks = usable_peaks
                          )

return(results)
}
