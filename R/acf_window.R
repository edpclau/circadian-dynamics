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

    sampling_bin_size = as.numeric(stringr::str_extract(sampling_rate, "\\d*"))
    sampling_rate = stringr::str_remove(sampling_rate, "\\d* *")
  }

  #3. the period must be calculated from the data on the second day.
  from <- from*2
  to <- to*2

  #4. If there is 0 variance in the data, skip window.
 good_windows <- purrr::map_dbl(unique(df$window_vector),
      .f = ~ ifelse(var(dplyr::pull(dplyr::filter(df, window_vector == .), values), na.rm = TRUE) == 0,
                    NA,
                    .
                    )
      )


 #5. If no window has more than 0 variance, return empty:

if ( all(is.na(good_windows)) ) {
 results <- tibble::tibble(window = unique(df$window_vector),
                           period = NA,
                           period_hours = NA,
                           autocorrelation_power = NA,
                           peak_lags = NA,
                           peaks = NA,
                           rythm_strength = NA
 )
 results <- dplyr::arrange(results, window)

 return(results)

}

 good_windows <- na.omit(good_windows)

########## Autocorrelation for a moving window of values ########

# Get the autocorrelation values
autocorrelations <- purrr::map(good_windows, # Iterate over the windows
                               .f =  ~ as.numeric(acf(x = dplyr::pull(dplyr::filter(df, window_vector == .), values), #Select the values in the window
                                                      y = dplyr::pull(dplyr::filter(df, window_vector == .), values),
                                                      na.action = na.pass,
                                                      type = "correlation",
                                                      plot = FALSE,
                                                      lag.max = length(dplyr::pull(dplyr::filter(df, window_vector == .), values)))$acf)) #$acf is to keep only the acf values. We can infer lag position from those

autocor_lags_lengths <- map(c(1:length(autocorrelations)),
    ~ length(autocorrelations[[.]])
    )
autocor_iterator <- 1:length(autocorrelations)
# Case when we are interested in looking at a specific range of lags/periods.
if (!is.null(to) &  !is.null(from) ) {
start <- round(((as.numeric(lubridate::duration(from, "hours"), sampling_rate) + 1)/sampling_bin_size))
end <- round((as.numeric(lubridate::duration(to, "hours"), sampling_rate) + 1)/sampling_bin_size)

autocorrelations <- purrr::map(autocor_iterator,
~ autocorrelations[[.]][seq(start, end)])
} else if (!is.null(to) & is.null(from)) {
 autocorrelations <- purrr::map(autocor_iterator,
~ autocorrelations[[.]][1:round(as.numeric(lubridate::duration(to, "hours"), sampling_rate + 1)/sampling_bin_size)]
)
} else if (is.null(to) & !is.null(from)) {
autocorrelations <-  purrr::map(autocor_iterator,
 ~  autocorrelations[[.]][round((as.numeric(lubridate::duration(from, "hours"), sampling_rate) + 1)/sampling_bin_size):length(autocorrelations[[.]])]
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
mean_peak_index <- purrr::map(autocor_iterator, ~ which(positive_peaks[[.]] == mean_peaks[[.]]))


############### Find lags in the autocorrelation #####
if (is.null(from) & is.null(to)) {

# Get the lag that corresponds to each peak
lags <- purrr::map(autocor_iterator, # Iterate over the windows
                   .f =  ~ seq(from = 0,
                               to = length(dplyr::pull(dplyr::filter(df, window_vector == .), values)) - 1,
                               by = 1))
} else if (!is.null(to) &  !is.null(from) ) {
  start <- round(((as.numeric(lubridate::duration(from, "hours"), sampling_rate) + 1)/sampling_bin_size))
  end <- round((as.numeric(lubridate::duration(to, "hours"), sampling_rate) + 1)/sampling_bin_size)

  lags <- purrr::map(autocor_iterator,
                     .f =  ~ seq(start, end)
                     )

} else if (!is.null(to) & is.null(from)) {
  start <- round(((as.numeric(lubridate::duration(from, "hours"), sampling_rate) + 1)/sampling_bin_size))
  end <- round((as.numeric(lubridate::duration(to, "hours"), sampling_rate) + 1)/sampling_bin_size)

  lags <- purrr::map(autocor_iterator,
                     .f =  ~ seq(start, end))

} else if (is.null(to) & !is.null(from)) {
  lags <- purrr::map(good_windows,
                     .f =  ~ seq(start, end))
}

peak_index <- purrr::map(autocorrelations_no_na, .f = ~ pracma::findpeaks(.)[,2])
peak_lags <- purrr::map2(lags, peak_index, .f = ~ .x[.y])



# Positive Peak Lags
pos_peak_lag_index <- purrr::map2(peak_index, positive_peak_index, .f = ~ .x[.y])
pos_peak_lags <- purrr::map2(lags, pos_peak_lag_index, .f = ~ .x[.y])



#### The period ####
# First, find the windows with null and empty values by turning mean_peaks into a tibble. This will eliminate all the null and is.numeric(0) values

names(mean_peaks) <- seq_along(mean_peaks)



usable_windows <- mean_peaks %>% names() %>% as.numeric()



# Now calculate the period.

period <- purrr::map_if(usable_windows,
                        .p = ~ !(is.null(mean_peaks[[.]])), # If the mean_peaks (autopower) is doesn't exist, give out an NA
                        .else =  ~ NA,
                        .f = ~  (pos_peak_lags[[.]][mean_peak_index[[.]]] - 1) /2
)






# Transform the period into hours
period_hours <- map_if(1:length(period),
                    .p = ~ !is.na(period[[.]]), # If the period is NA, don't turn it into hours
                    .else =  ~ NA,
                    .f = ~ lubridate::duration(period[[.]] * sampling_bin_size, sampling_rate) %>% as.numeric("hours") %>% paste("hours")
    )


###### Arranging Data #####
autocorrelation_power <- map(1:length(mean_peaks), ~ ifelse(is.null(mean_peaks[[.]]), NA, mean_peaks[[.]]))

usable_peak_lags <- map(usable_windows, ~ (pos_peak_lags[[.]][mean_peak_index[[.]]] - 1))
usable_peaks <- map(usable_windows, ~ positive_peaks[[.]][mean_peak_index[[.]]])

rythm_strength <- map(usable_windows, ~ usable_peaks[[.]]/(1.965/sqrt(autocor_lags_lengths[[.]])))



##### Results ####
results <- tibble::tibble(window = good_windows,
                          period = unlist(period),
                          period_hours = unlist(period_hours),
                          autocorrelation_power = unlist(autocorrelation_power),
                          peak_lags = usable_peak_lags,
                          peaks = usable_peaks,
                          rythm_strength = rythm_strength,
                          from_acf = start,
                          to_acf = end
                          )
results <- tidyr::unnest(results, cols = c(peak_lags, peaks, rythm_strength), keep_empty = TRUE)

results <- dplyr::bind_rows(results,
          tibble::tibble(window = unique(df$window_vector)[!(unique(df$window_vector) %in% good_windows)]))


#add the input data.frame to the results
input_values <- tidyr::nest(df, acf_input_values = 2)

results <- dplyr::left_join(results, input_values, by = c("window" = "window_vector"))

#order the data by windows
results <- dplyr::arrange(results, window)

return(results)
}
