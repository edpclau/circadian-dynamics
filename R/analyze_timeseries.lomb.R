#' Lomb Scargle Periodogram by window
#'
#' @description Iteratively computes the Lomb-Scargle periodogram for a time series with irregular (or regular) sampling intervals.
#' @usage
#' lsp_by_window(df = NULL, windows = NULL, values = NULL,
#' times = NULL, sampling_rate = NULL,
#' from = 18, to = 30, type = c("period", "frequency"),
#' ofac = 60, alpha = 0.01, plot = FALSE)
#'
#' @param df
#' optional data.frame with 2 or 3 columns. If the data.frame has 2 columns, column 1 contains the windows, column 2
#' contains the measurement values. If the data.frame has 3 columns, column 1 contains windows, column 2 is POSIXct object,
#' and column 3 is a measurement values.
#'
#' @param windows
#' A vector containing the windows to iterate over and to label the group to which each value belongs.
#' Usually this will be the output from make_time_windows. Optional if df is provided.
#'
#' @param values The data to which we want to find the period. Optional if df is provided.
#' @param times A POSIXct vector. Optional if df is provided.
#' @param sampling_rate
#' A character string indicating the sampling rate of the data.
#' Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @param from
#' An optional numeric indicating from which period or frequency to start looking for peaks.
#' Must be in hours. Default = 18.
#'
#' @param to
#' An optional numeric indicating up to which period or frequency to start looking for peaks.
#' Must be in hours. Default = 30.
#'
#' @param type
#' Either “frequency” or “period” (default). Determines the type of the periodogram x-axis [lomb::lsp()].
#'
#' @param ofac
#' [lomb::lsp()] The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
#'
#' @return
#' A data.frame with the following components:
#'
#' window           A vector containing the window to which the analysis corresponds.
#'
#' period           The period of the timeseries. Outputted in the same sampling rate as the data.
#'
#' power            The maximum power in the frequency/period interval inspected.
#'
#' lsp_p_value      The probability that the maximum peak occurred by chance.
#'
#' scanned          A vector containing the frequencies/period scanned.
#'
#' normalized_power A vector containing the normalized power corresponding to scanned frequencies/periods.
#'
#' sig_level        Powers > sig.level can considered significant peaks.
#'
#' @export analyze_timeseries.lomb
#'
#' @examples
#' lsp_analysis <- lsp_by_window(df = processed_data, sampling_rate = "30 min")
#'
#'
#' @importFrom furrr future_map_if furrr_options
#' @importFrom tibble tibble
#' @importFrom lubridate  duration
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove str_extract
#' @importFrom future plan multisession
#'
analyze_timeseries.lomb<- function (df = NULL, sampling_rate = NULL, from = 18, to = 30,
                            ofac = 60, alpha = 0.01) {

  ###### Flow control parameters######
  #1.must have sampling_rate
  if (is.null(sampling_rate)) {
    stop("Must include sampling_rate. ex. '30 min', '1 hour', '4 seconds', '100 days'.")
  }

  #2. You can choose between period or frequency for "Type". Period is the default.
  type <- 'period'


  #3. Sampling_rate
  sampling_bin_size = as.numeric(str_extract(sampling_rate, "\\d*"))
  sampling_rate = str_remove(sampling_rate, "\\d* *")

  #4. Extract values
  values = df$value

  #4. Convert from-to to sampling rate.
  if (!is.null(from)) {
    from <- as.numeric(duration(from, units = "hours"), sampling_rate)/ sampling_bin_size
  }

  if (!is.null(to)) {
    to  <- as.numeric(duration(to, units = "hours"), sampling_rate) / sampling_bin_size
  }

  #If there is 0 variance in the data or less than 2 values that are not NA, skip window.
  if (var(values) == 0 | sum(!is.na(values)) <= 2) {
    results = list(
      period = NA ,
      power = NA,
      p_value = NA,
      sig_level = NA,
      scanned = NA,
      power = NA
    )
    return(results)
  }

#### Lomb Scargle periodogram#####

#lsp of interest
lsp_of_int = lsp_mod(x = values, from = from, to = to,  ofac = ofac, type = type, alpha = alpha, plot = FALSE)
#the full lsp for plotting (may have to delete this later if I end up not using it)
lsp = lsp_mod(x = values, ofac = ofac, type = type, alpha = alpha, plot = FALSE)

results = list(
       period = as.numeric(duration(lsp_of_int$peak.at[1] * sampling_bin_size, sampling_rate), "hours"),
       peak = lsp_of_int$peak,
       p_value = lsp_of_int$p.value,
       sig_level = lsp_of_int$sig.level,
       scanned = lsp$scanned,
       power = lsp$power)



  return(results)
}
