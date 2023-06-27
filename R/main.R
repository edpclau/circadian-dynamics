#' Helper function to prepare raw data for analysis
#'
#' @description Processes raw data in such a way that it can be directly inputted to the rythm_analysis_by_window function.
#'
#' @usage process_timeseries(df = NULL, sampling_rate = NULL, window_size_in_days = 3, window_step_in_days = 1,
#' movavg = TRUE, detrend_data = TRUE, butterworth = TRUE,
#' f_low = 1/4, f_high = 1/73, plot = TRUE,
#' smoothing_n = 4, datetime = NULL, values = NULL)
#'
#' @param df A data.frame where the first column is a POSIXct object and the rest are independent measurement values.
#'
#' @param sampling_rate A character string indicating the sampling rate of the data. Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @param window_size_in_days a numeric indicating the width of the window size in day units.
#'
#' @param window_step_in_days a numeric indicating the amount of day by which to move the window in day units.
#'
#' @param movavg Logical. If TRUE (default) will smooth the measurement values useing a moving average. If FALSE measurement values won't be smoothed.
#'
#' @param detrend_data Logical. If TRUE (default) will detrend the data. If FALSE measurement values won't be detrended. If both, detrend_data and smooth_data are TRUE, the detrending will run over the smoothed data.
#'
#' @param butterworth Logical. If TRUE (default) will apply a buttwerworth filter to the measurement values using a moving average. If FALSE measurement values won't be filtered.
#'
#' @param datetime Optional if a data.frame is supplied. A POSIXct vector.
#'
#' @param values  Optional if a data.frame is supplied. A vector of values from a mesurement.
#'
#' @param smoothing_n A numeric which indicated the amount of bins over which to run the smoothing average. Default = 4.
#'
#' @param order filter order. Default = 2.
#' @param f_low Frequency for the low pass filter. Default = 1/4.
#' @param f_high Frequency for the high pass filter. Default = 1/72.
#'
#' @param plot logical. If TRUE (default) plots the filtered data over the raw data. If FALSE, does not plot.
#'
#' @return A named list of data.frames containing the output of [circadiandynamics::butterworth], [find_gaps()], [makes_time_windows()], and [smooth_detrend_by_windows()] for each measurement value.
#'
#' @export process_timeseries.rmv_gaps
#' @export process_timeseries.na_to_zero
#' @export process_timeseries.waveform
#' @export process_timeseries.main
#' @export process_timeseries.core
#'
#' @examples
#' processed_data <- process_timeseries(df = raw_data, sampling_rate = "30 min")
#'
#' @importFrom dplyr select right_join bind_rows filter
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom lubridate hour ceiling_date
#' @importFrom future plan multisession sequential
#' @importFrom furrr future_map furrr_options
#'
#'
#'
#'



# Here we create a moving window
# The window size will be dynamic as well as steps to slide the window
# df <- make_time_windows_2(df,
#                           window_size_in_days = window_size_in_days,
#                           window_step_in_days = window_step_in_days
# )

process_timeseries.rmv_gaps <- function(df = NULL, sampling_rate = NULL) {

  df <- right_join(df,
                   find_gaps(times = df$datetime, sampling_rate = sampling_rate),
                                by = "datetime")
  return(df)

}


process_timeseries.na_to_zero <- function(df = NULL) {

#Remove NA for missing data points, this is necessary for the autocorrelation
#Turn NA's into 0
df$value <- ifelse(is.na(df$value), 0, df$value)
return(df)

}



process_timeseries.waveform <- function(df = NULL,
                                        detrend_data = TRUE,
                                        smooth_data = FALSE,
                                        butterworth = TRUE,
                                        f_low = 1/4,
                                        f_high = 1/73,
                                        order = 2) {

if (smooth_data) { butterworth = FALSE }

df = smooth_and_detrend(df, smooth_data = smooth_data, detrend_data = detrend_data, binning_n = binning_n)

if (butterworth)
df = butterworth_filter_2(df, order = order, f_low = f_low, f_high = f_high, plot = FALSE)


return(df)

}







process_timeseries.core <- function(df = NULL,

                                    make_windows = FALSE,
                                    window_size_in_days = 3,
                                    window_step_in_days = 1,
                                    sampling_rate = '1 hour',
                                    detrend_data = TRUE,
                                    smooth_data = FALSE,
                                    butterworth = TRUE,
                                    from = 18,
                                    to = 30,
                                    f_low = 1/4,
                                    f_high = 1/73,
                                    order = 2,
                                    causal_order = 1,
                                    big_data = FALSE,
                                    ofac = 1,
                                    lomb_pvalue = 0.01) {

if (big_data) {
plan(multisession)
} else {plan(sequential)}

  if (make_windows){
  #Set step and window_size
  window_size <- days(window_size_in_days) #Width of the window
  times <- df$datetime
  step = seq(from = min(times), to = max(times), by = paste(window_step_in_days, "day")) #days to move the window

  df = future_map(
    .options = furrr_options(seed = 42),
    .x = step,
    .f = ~ {

      x = filter(df, (datetime >= .x) & (datetime <= .x + window_size))

      #General Pipeline

      x = process_timeseries.rmv_gaps(x, sampling_rate = sampling_rate)

      x = process_timeseries.na_to_zero(x)

      #ACF Pipeline
      x = process_timeseries.waveform(x,
                                      detrend_data = detrend_data, smooth_data = smooth_data,
                                      butterworth = butterworth, f_low = f_low, f_high = f_high, order = order)

      acf_results = analyze_timeseries.acf(x, from = from, to = to, sampling_rate = sampling_rate)

      acf_cosinor = analyze_timeseries.cosinor(x, sampling_rate = sampling_rate, period = acf_results$period)

      acf_results$grangercausal = analyze_timeseries.grangertest(value = x$value, cos = acf_cosinor$wave, order = causal_order)
      #Lomb-Scargle Pipeline

      lsp_results = analyze_timeseries.lomb(df = x, sampling_rate = sampling_rate, from = from, to = to, ofac = ofac, alpha = lomb_pvalue)

      lsp_cosinor = analyze_timeseries.cosinor(x, sampling_rate = sampling_rate, period = lsp_results$period)

      lsp_results$grangercausal = analyze_timeseries.grangertest(value = x$value, cos = lsp_cosinor$wave,  order = causal_order)

      # Control Pipeline (We set an arbitrary 24 hour cosinor)
      arbitrary_cosinor = analyze_timeseries.cosinor(x, sampling_rate = sampling_rate, period = 24)

       return(list(data = x,
                  acf = list(results = acf_results,
                             cosinor = acf_cosinor),
                  lomb = list(results = lsp_results,
                              cosinor = lsp_cosinor),
                  control = list(cosinor = arbitrary_cosinor)
                  )
             )
      }
  )

  return(df)

  } else {
  #General Pipeline
  df = process_timeseries.rmv_gaps(df, sampling_rate = sampling_rate)
  df = process_timeseries.na_to_zero(df)
  #ACF Pipeline
  df = process_timeseries.waveform(df,
                                  detrend_data = detrend_data, smooth_data = smooth_data,
                                  butterworth = butterworth, f_low = f_low, f_high = f_high, order = order)
  acf_results = analyze_timeseries.acf(df, from = from, to = to, sampling_rate = sampling_rate)
  acf_cosinor = analyze_timeseries.cosinor(df, sampling_rate = sampling_rate, period = acf_results$period)
  acf_results$grangercausal = analyze_timeseries.grangertest(value = df$value, cos = acf_cosinor$wave,  order = causal_order)
  #Lomb-Scargle Pipeline
  lsp_results = analyze_timeseries.lomb(df = df, sampling_rate = sampling_rate, from = from, to = to, ofac = ofac, alpha = lomb_pvalue)
  lsp_cosinor = analyze_timeseries.cosinor(df, sampling_rate = sampling_rate, period = lsp_results$period)
  lsp_results$grangercausal = analyze_timeseries.grangertest(value = df$value, cos = lsp_cosinor$wave,  order = causal_order)

  #Control: arbitrary 24 hour cosinor
  arbitrary_cosinor = analyze_timeseries.cosinor(df, sampling_rate = sampling_rate, period = 24)

  return(list(data = df,
              acf = list(results = acf_results,
                         cosinor = acf_cosinor),
              lomb = list(results = lsp_results,
                          cosinor = lsp_cosinor),
              control = list(cosinor = arbitrary_cosinor)
              )
         )

  }


}

process_timeseries.main <- function(df = NULL,

                                    make_windows = FALSE,
                                    window_size_in_days = 3,
                                    window_step_in_days = 1,
                                    from = 18,
                                    to = 30,
                                    sampling_rate = '1 hour',
                                    detrend_data = TRUE,
                                    movavg = FALSE,
                                    butterworth = TRUE,
                                    f_low = 1/4,
                                    f_high = 1/73,
                                    order = 2,
                                    causal_order = 1,
                                    big_data = FALSE,
                                    ofac = 10,
                                    lomb_pvalue = 0.01) {
  if (big_data) {
    plan(multisession)
  } else {plan(sequential)}

  return(

    future_map(
      .x = df,
      .options = furrr_options(seed = 42),
      .f = ~ {
        process_timeseries.core(df = .x,

                                make_windows = make_windows,
                                window_size_in_days = window_size_in_days,
                                window_step_in_days = window_step_in_days,
                                sampling_rate = sampling_rate,
                                detrend_data = detrend_data,
                                smooth_data = movavg,
                                butterworth = butterworth,
                                f_low = f_low,
                                f_high = f_high,
                                from = from,
                                to = to,
                                order = order,
                                causal_order = causal_order,
                                big_data = big_data,
                                ofac = ofac,
                                lomb_pvalue = lomb_pvalue)
      }
    )


  )

}

