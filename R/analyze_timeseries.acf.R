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
#' @export analyze_timeseries.acf
#'
#' @examples
#' autocorrelations_multipeak <- acf_window(df = df_with_windows,
#' multipeak_period = FALSE, peak_of_interest = 2,
#' sampling_unit = "hours")
#'
#' @importFrom dplyr pull filter mutate left_join select
#' @importFrom stringr str_extract str_remove
#' @importFrom lubridate duration as.duration as.interval
#' @importFrom tibble tibble
#' @importFrom pracma findpeaks movavg
#'
analyze_timeseries.acf <- function(df = NULL,  from = 18, to = 30,
                       sampling_rate = "1 hour") {

  #Create results list
  results = list()
  #First check if the function can be skipped (var = 0)
  #the data for the acf will be the last col outputter by the processing functions
  values = pull(df, ncol(df))
  #Check the variance
  if (var(values) == 0 | length(values) <= 3) {
  results$datetime = NA
  results$autocorrelation = NA
  results$power = NA
  results$period = NA
  results$rythm_strength = NA
  results$max_peak_of_int = NA
  results$start = NA
  results$end = NA
  results$from = from
  results$to = to
  return(results)

  }

  ##### Flow Control Parameters #####

  #1. Sampling_rate must exist
  if (is.null(sampling_rate)){
    stop("must provide a sampling_rate")
  } else {

    sampling_bin_size = as.numeric(str_extract(sampling_rate, "\\d*"))
    sampling_rate = str_remove(sampling_rate, "\\d* *")
  }

  #2. the period must be calculated from the data on the second day.
  start <- duration(from+24, 'hours')
  end <- duration(to+24, 'hours')


##### ACF ######
  #4. Autocorrelation
  #Carry out the autocorrelation with the  data
  autocorrelation = as.numeric(acf(x = values, #Select the values in the window
                     y = values,
                     na.action = na.pass,
                     type = "correlation",
                     plot = FALSE,
                     lag.max = length(values))$acf) #$acf is to keep only the acf values. We can infer lag position from those

   # Change change all NA autocorrelations to 0
      # The function that looks for peaks doesn't allow NA, 0 would be ignored if we put a threshold, therefore it won't affect
      # the results
  autocorrelation = ifelse( is.na(autocorrelation), 0, autocorrelation)
  len_autocor = length(autocorrelation)

  #Find the peaks
  peaks = findpeaks(autocorrelation, sortstr = TRUE)
  peaks = tibble(auto_power = peaks[,1],
                 lags = diff(c(0, peaks[,2])/sampling_bin_size),
                 datetime = duration(peaks[,2], sampling_rate))

  #Keep only the positive peaks
  peaks = dplyr::filter(peaks, auto_power > 0.2)


  #If there are no Peaks, return NA
  if (nrow(peaks) == 0) {
    results$datetime = NA
    results$autocorrelation = NA
    results$power = NA
    results$period = NA
    results$rythm_strength = NA
    results$max_peak_of_int = NA
    results$start = NA
    results$end = NA
    results$from = from
    results$to = to
    return(results)
  }



  #Locate the peaks in time
  #Translate the lags into the sampling_rate
  peaks$lags = duration(peaks$lags, sampling_rate)

  #Find the maximum peak within the scope
  peaks_of_int = filter(peaks, datetime >= start, datetime <= end)

  #Make sure the peaks_of_int is not empty, otherwise return NA
  if (!all(is_empty(peaks_of_int$auto_power))) {

    #Get the maximum peak
    max_peak_of_int = max(peaks_of_int$auto_power)

    #Get the lag of the maximum peak
    max_lag = filter(peaks_of_int, auto_power == max_peak_of_int)$lags
    #Translate the max_lag into the correct time
    max_lag = abs(as.numeric(max_lag, 'hours'))

    #Get datetime of the maximum peak
    max_date = filter(peaks_of_int, auto_power == max_peak_of_int)$datetime
    max_date = as.numeric( duration(max_date, sampling_rate), 'hours')

    #Get the Rhythm Strength
    rhythm_strength = max_peak_of_int / (1.965/sqrt(nrow(df)))


  # } else if (nrow(peaks) != 0) {
  #
  #   #Get the maximum peak
  #   max_peak_of_int = max(peaks$auto_power)
  #
  #   #Get the lag of the maximum peak
  #   max_lag = filter(peaks, auto_power == max_peak_of_int)$lags
  #   #Translate the max_lag into the correct time
  #   max_lag = abs(as.numeric(max_lag, 'hours'))
  #
  #   #Get datetime of the maximum peak
  #   max_date = filter(peaks, auto_power == max_peak_of_int)$datetime
  #   max_date = as.numeric( duration(max_date, sampling_rate), 'hours')
  #
  #   #Get the Rhythm Strength
  #   rhythm_strength = max_peak_of_int / (1.965/sqrt(nrow(df)))




  } else {
    results$datetime = NA
    results$autocorrelation = NA
    results$power = NA
    results$period = NA
    results$rythm_strength = NA
    results$max_peak_of_int = NA
    results$start = NA
    results$end = NA
    results$from = from
    results$to = to
    results$start = start
    results$end = end
    return(results)
  }






  results$datetime = max_date
  results$autocorrelation = autocorrelation
  results$power = peaks$auto_power
  results$period = max_lag
  results$rythm_strength = rhythm_strength
  results$max_peak_of_int = max_peak_of_int
  results$start = start
  results$end = end
  results$from = from
  results$to = to

  #Return results
  return(results)
}
