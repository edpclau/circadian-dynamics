#' Auto Correlation with a moving window
#'
#' @description Iterated autocorrelation over windowed (grouped) timeseries data.
#'
#' @usage acf_window(df = df_with_windows, multipeak_period = TRUE)
#'
#' @param df A data.frame with 2 columns. Column 1 must contain the windows to iterate over.
#' Column 2 must supply the values. This parameter is optional if window_vector and values are supplied.
#' df must not have gaps in the dates, acf asumes data is evenly spaced.
#'
#' @param multipeak_period TRUE, use all positive peaks to find the period.
#' FALSE (default) use peak_of_interest to find period.
#'
#' @param peak_of_interest Positive peak on which we want to base the period calculation.
#' If no peak is supplied it will use the peak next to the middle peak. The default is the second (2) peak.
#'
#' @param sampling_unit A charater string which indicates the sampling unit. For example: "seconds", "minutes", "hours" (default),
#' "days", "months".
#'
#' @param window_vector A vector containing the windows to iterate over and to label the group
#' to which each value belongs. Usually this will be the output from make_time_windows.
#'
#' @param values The data to which we want to find the period.
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
#' @import dplyr
#' @import purrr
#' @importFrom tibble tibble
#' @importFrom pracma findpeaks
#' @importFrom pracma movavg
#' @importFrom rlang is_empty
#'
acf_window <- function(df = NULL,  multipeak_period = FALSE, peak_of_interest = 2,
                       sampling_unit = "hours", window_vector = NULL, values = NULL){

##### Flow Control Parameters #####
  #1. Either a df or two lists with the time_series and values must be supplied. If a df is supplied,
  # Change the names of the df columns so we can work with it inside the function. If a df is not supplied, turn the
  # two lists into a df.
  if (!is.null(df)) {
    names(df) <- c("window_vector", "values")
  } else if (is.null(df) & (is.null(window_vector) | is.null(values))) {
    stop("If a data.frame is not supplied. Must include both window_vector and values.")
  } else if (is.null(df)) { df = tibble::tibble(window_vector, values) }

# Autocorrelation for a moving window of values

# Get the autocorrelation values
autocorrelations <- purrr::map(unique(df$window_vector), # Iterate over the windows
                               .f =  ~ as.numeric(acf(x = dplyr::pull(dplyr::filter(df, window_vector == .), values), #Select the values in the window
                                                      y = dplyr::pull(dplyr::filter(df, window_vector == .), values),
                                                      na.action = na.pass,
                                                      type = "correlation",
                                                      plot = FALSE,
                                                      lag.max = length(dplyr::pull(dplyr::filter(df, window_vector == .), values)))$acf)) #$acf is to keep only the acf values. We can infer lag position from those

# Change change all NA autocorrelations to 0
# The function that looks for peaks doesn't allow NA, 0 would be ignored if we put a threshold, therefoere it won't affect
# the results
autocorrelations_no_na <- purrr::map(autocorrelations,  .f =  ~ dplyr::if_else(is.na(.) == TRUE, 0, .))

# Find the peaks
peaks <- purrr::map(autocorrelations_no_na, .f = ~ pracma::findpeaks(., nups = 0, ndowns = 1)[,1])
positive_peak_index <- map(1:length(peaks),
                      .f = ~ which(peaks[[.]] > 0))

# Keep only the positive peaks
positive_peaks <-  map2(peaks, positive_peak_index,
     .f = ~ .x[.y])
# Find the average peak size (autopower)
mean_peaks <- purrr::map(positive_peaks, .f = ~ if(!(rlang::is_empty(.) | length(.) <= 1)) { mean(., na.rm = TRUE)} ) # We use map_if so that we don't process the NULL values


############### Find lags in the autocorrelation #####
# Get the lag that corresponds to each peak
lags <- purrr::map(unique(df$window_vector), # Iterate over the windows
                   .f =  ~ seq(from = 0,
                               to = length(dplyr::pull(dplyr::filter(df, window_vector == .), values)) - 1,
                               by = 1))

peak_index <- purrr::map(autocorrelations_no_na, .f = ~ pracma::findpeaks(., nups = 0, ndowns = 1)[,2])
peak_lags <- purrr::map2(lags, peak_index, .f = ~ .x[.y])

# calculate lag diff so that we can find the period
diff_lags <- purrr::map_if(peak_lags, .p = ~ !is.null(.), .f = ~ if(length(.) == 1){.} else {diff(.)})


# Positive Peak Lags
pos_peak_lag_index <- purrr::map2(peak_index, positive_peak_index, .f = ~ .x[.y])
pos_peak_lags <- purrr::map2(lags, pos_peak_lag_index, .f = ~ .x[.y])
# calculate lag diff so that we can find the period of positive peaks
pos_diff_lags <- purrr::map_if(peak_lags, .p = ~ !is.null(.), .f = ~ if(length(.) == 1){.} else {diff(.)})



#### The period ####
# First, find the windows with null and empty values by turning mean_peaks into a tibble. This will eliminate all the null and is.numeric(0) values

names(mean_peaks) <- seq_along(mean_peaks)



usable_windows <- purrr::discard(mean_peaks, rlang::is_empty) %>% names() %>% as.numeric()



# Now calculate the period.

period <- purrr::map_if(usable_windows,
                        .p = ~ !(mean_peaks[[.]] < 0), # If the mean_peaks (autopower) is negative, give out an NA
                        .else =  ~ NA,
                        .f = ~ if (multipeak_period == TRUE) { mean(pos_diff_lags[[.]]) #If we want the multipeak period, print out the mean of lags, I think it should be the median
                        } else if (multipeak_period == FALSE) {
                          if (length(pos_diff_lags[[.]]) == 1) { .
                          } else if (length(pos_diff_lags[[.]]) >= peak_of_interest){pos_diff_lags[[.]][peak_of_interest] #print the lag of the peak of interest
                          } else {pos_diff_lags[[.]][length(pos_diff_lags[[.]])/2] #We're trying to get the inner most lags which tend to be the most stable
                            warning(paste("peak_of_interest is out of bounds. Will use peak =", length(pos_diff_lags[[.]])/2, "instead"))
                          }
                        }
)

# Transform the period into hours
period <- map(1:length(period),
    .f = ~ lubridate::duration(period[[.]], sampling_unit) %>% as.numeric("hours") %>% paste("hours")
    )

autocorrelation_power <- purrr::discard(mean_peaks, is.null)

usable_peak_lags <- pos_peak_lags[usable_windows]
usable_peaks <- positive_peaks[usable_windows]
results <- tibble::tibble(window = usable_windows,
                          period = unlist(period),
                          autocorrelation_power = unlist(autocorrelation_power),
                          peak_lags = usable_peak_lags,
                          peaks = usable_peaks
                          )

return(results)
}
