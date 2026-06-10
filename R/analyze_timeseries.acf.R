#' Auto Correlation with a moving window
#'
#' @description Uses autocorrelation to find a circadian period for a given timeseries
#'
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
#' @details The reported `rhythm_strength` is the maximum autocorrelation peak within the
#' search band divided by the 95% white-noise confidence bound \code{1.965 / sqrt(n)}, where \code{n} is the number of observations in \code{df}. Values > 1
#' indicate the peak exceeds what white noise would produce. The Lomb-Scargle path
#' reports a different quantity, `relative_power` (in [analyze_lomb]), which is on a
#' different scale and is not directly comparable.
#'
#' @return A data.frame with the autocorrelation results for each window which include: period, peaks,
#' power, lags for the peaks.
#'
#' @seealso [stats::acf()] which this functions uses to run the autocorrelation.
#'
#' @export analyze_acf
#'
#' @examples
#' \dontrun{
#' res <- analyze_acf(df, from = 18, to = 30, sampling_rate = "1 hour")
#' }
#'
#' @importFrom dplyr pull filter
#' @importFrom lubridate duration
#' @importFrom tibble tibble
#' @importFrom pracma findpeaks movavg
#'
analyze_acf <- function(df = NULL,  from = 18, to = 30,
                       sampling_rate = "1 hour") {

  # Empty result, returned from every branch where no period can be determined.
  # All keys present and consistent (start/end are NA in every NA branch).
  na_result <- function() list(
    datetime = NA, autocorrelation = NA, power = NA, period = NA,
    rhythm_strength = NA, max_peak_of_int = NA, start = NA, end = NA,
    from = from, to = to
  )

  # ACF runs on the last column produced by the processing steps.
  values = pull(df, ncol(df))
  # Skip flat or too-short windows. var(values) is NA (not 0) when values contains
  # NAs, so guard with isTRUE() and na.rm to avoid an `if(NA)` crash.
  if (isTRUE(var(values, na.rm = TRUE) == 0) || length(values) <= 3) return(na_result())

  ##### Flow Control Parameters #####

  #1. Sampling_rate must exist
  if (is.null(sampling_rate)){
    stop("must provide a sampling_rate")
  } else {

    sr <- .parse_sampling_rate(sampling_rate)
    sampling_bin_size <- sr$bin
    sampling_rate <- sr$unit
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
  autocorrelation = ifelse(is.na(autocorrelation), 0, autocorrelation)

  #Find the peaks
  peaks = findpeaks(autocorrelation, sortstr = TRUE)

  #If there are no peaks, return NA
  if (rlang::is_empty(peaks)) return(na_result())



  # peaks[,2] are 1-based ACF-vector indices; index i corresponds to lag i-1
  # (lag 0 is index 1). Subtract 1 to get the true lag in samples before scaling
  # by the sampling bin size, otherwise every period is one bin too large.
  peaks = tibble(auto_power = peaks[,1],
                 datetime = duration((peaks[,2] - 1L) * sampling_bin_size, sampling_rate))

  #Keep only the positive peaks
  peaks = dplyr::filter(peaks, auto_power >= 0.2)



  #Find the maximum peak within the scope
  peaks_of_int = filter(peaks, datetime >= start, datetime <= end)

  # Peaks exist but none fall in the [start, end] band: no period (consistent NA result).
  if (nrow(peaks_of_int) == 0) return(na_result())

  max_peak_of_int = max(peaks_of_int$auto_power)
  # top$datetime is already a Duration; convert straight to hours. Re-wrapping it in
  # duration() would reinterpret its seconds as 'sampling_rate' units and inflate it.
  top = filter(peaks_of_int, auto_power == max_peak_of_int)
  period = as.numeric(top$datetime, 'hours') - 24
  max_date = as.numeric(top$datetime, 'hours')
  rhythm_strength = max_peak_of_int / (1.965/sqrt(nrow(df)))






  list(
    datetime = max_date,
    autocorrelation = autocorrelation,
    power = peaks$auto_power,
    period = period,
    rhythm_strength = rhythm_strength,
    max_peak_of_int = max_peak_of_int,
    start = start,
    end = end,
    from = from,
    to = to
  )
}
