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
#'
#' @return A data.frame with the autocorrelation results for each window which include: period, peaks,
#' power, lags for the peaks.
#'
#' @seealso [stats::acf()] which this functions uses to run the autocorrelation.
#'
#' @export acf_window_2
#'
#' @examples
#' autocorrelations_multipeak <- acf_window(df = df_with_windows,
#' multipeak_period = FALSE, peak_of_interest = 2,
#' sampling_unit = "hours")
#'
#' @importFrom dplyr pull filter mutate left_join select
#' @importFrom stringr str_extract str_remove
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map_if furrr_options
#' @importFrom lubridate duration as.duration as.interval
#' @importFrom tibble tibble
#' @importFrom pracma findpeaks movavg
#' @import magrittr
#'
acf_window_2 <- function(df = NULL,  from = 18, to = 30,
                       sampling_rate = "1 hour", big_data = FALSE) {

  ##### Flow Control Parameters #####

  #1. Sampling_rate must exist
  if (is.null(sampling_rate)){
    stop("must provide a sampling_rate")
  } else {

    sampling_bin_size = as.numeric(str_extract(sampling_rate, "\\d*"))
    sampling_rate = str_remove(sampling_rate, "\\d* *")
  }

  #2. the period must be calculated from the data on the second day.
  from <- from*2
  to <- to*2
  start <- duration(from, "hours")
  end <- duration(to, "hours")


  #3. plan for paralelization
  if (big_data){
  plan(multisession)
  } else {plan(sequential)}

  df = {future_map_if(
    # .options = furrr_options(lazy = TRUE),
    .x = df,
    .p = ~ var(.x[['butterworth']]) != 0,
    .f = ~ {
      #Carry out the autocorrelation with the butterworth data
      butter = pull(.x, butterworth)
      autocorrelation = as.numeric(acf(x = butter, #Select the values in the window
                     y = butter,
                     na.action = na.pass,
                     type = "correlation",
                     plot = FALSE,
                     lag.max = length(butter))$acf) #$acf is to keep only the acf values. We can infer lag position from those
      # Change change all NA autocorrelations to 0
      # The function that looks for peaks doesn't allow NA, 0 would be ignored if we put a threshold, therefoere it won't affect
      # the results
      autocorrelation = ifelse( is.na(autocorrelation), 0, autocorrelation)
      .x$autocorrelation = autocorrelation
      len_autocor = length(autocorrelation)


      #Find the peaks
      peaks = tibble(as.data.frame(findpeaks(autocorrelation)[,c(1,2)]))
      names(peaks) = c('auto_power', 'datetime')

      #Keep only the positive peaks
      peaks = filter(peaks, auto_power >= 0)

      #Locate the peaks in time
      datetimes = pull(.x, datetime)
      min_datetime = min(datetimes)
      peaks$datetime = duration(peaks$datetime, sampling_rate)
      peaks$datetime = peaks$datetime + min_datetime

      #Find the maximum peak within the scope
      peaks$of_int =   ifelse( (peaks$datetime >= min_datetime + start) & (peaks$datetime <= min_datetime + end), TRUE, FALSE)
      max_peak_of_int = filter(peaks, of_int == TRUE) %>% pull(auto_power) %>% max()
      peaks = mutate(peaks, max = ifelse(auto_power == max_peak_of_int, TRUE, FALSE))

      #Find the period of the maximum peak
      #We divide by 2 to to get the period since we're looking for peaks after the second day.
      peaks = mutate(peaks, auto_period = ifelse(max, as.numeric(as.duration(as.interval(min_datetime, datetime))/2, sampling_rate), NA))

      #Calculate Rythm_strength

      peaks = mutate(peaks, auto_rythm_strength = ifelse(max, auto_power/(1.965/sqrt(len_autocor)), NA))

      peaks = select(peaks, -c(of_int, max))

      #return the analysis
      left_join(.x, peaks, by = 'datetime')
    },
    .else =
      #return NA
      ~ mutate(.x,
               auto_power = NA,
               auto_period = NA,
               auto_rythm_strength = NA)




  )
  }

  #Return results
  return(df)
}
