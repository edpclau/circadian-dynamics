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
#' @export lsp_by_window_2
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
lsp_by_window_2 <- function (df = NULL, sampling_rate = NULL, from = 18, to = 30,
                            ofac = 60, alpha = 0.01, big_data = FALSE) {

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

  #4. Convert from-to to sampling rate.
  if (!is.null(from)) {
    from <- as.numeric(duration(from, units = "hours"), sampling_rate)/ sampling_bin_size
  }

  if (!is.null(to)) {
    to  <- as.numeric(duration(to, units = "hours"), sampling_rate) / sampling_bin_size
  }

  #5. Plan for paralellization
  if (big_data) {
  plan(multisession)
  } else {plan(sequential)}

  #### Lomb Scargle periodogram by time windows #####

 df  = future_map_if(
   .options = furrr_options(lazy = TRUE),
    .x = df,
    #If there is 0 variance in the data or less than 2 values that are not NA, skip window.
    .p = ~ var(.x$value) != 0 | sum(!is.na(.x$value)) >= 2,
    #If it fails, return NA
    .else = ~ {
      mutate(.x,
                        lsp_period = NA ,
                        lsp_power = NA,
                        lsp_p_value = NA,
                        lsp_sig_level = NA
                      )
    },
    .f = ~ {
      value = .x$value
      #lsp of interest
      lsp_of_int = lsp_mod(x = value, from = from, to = to,  ofac = ofac, type = type, alpha = alpha, plot = FALSE)
      #the full lsp for plotting (may have to delete this later if I end up not using it)
      lsp = lsp_mod(x = value, ofac = ofac, type = type, alpha = alpha, plot = FALSE)


      mutate(.x,
             lsp_period = as.numeric(duration(lsp_of_int$peak.at[1] * sampling_bin_size, sampling_rate), "hours"),
             lsp_power = lsp_of_int$peak,
             lsp_p_value = lsp_of_int$p.value,
             lsp_sig_level = lsp_of_int$sig.level)
    }
  )

  return(df)
}
