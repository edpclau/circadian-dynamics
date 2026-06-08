#' Helper function to prepare raw data for analysis
#'
#' @description Processes raw data in such a way that it can be directly inputted to the rhythm_analysis_by_window function.
#'
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
#' @param order filter order. Default = 2.
#' @param f_low Frequency for the low pass filter. Default = 1/4.
#' @param f_high Frequency for the high pass filter. Default = 1/72.
#'
#' @param from The period (in hours) from which to start looking for peaks. Default = 18.
#'
#' @param to The period (in hours) up to which to look for peaks. Default = 30.
#'
#' @param smooth_data Logical. If TRUE will smooth the measurement values using a moving average. Default = FALSE.
#'
#' @param make_windows Logical. If TRUE, splits the timeseries into moving windows before analysis. Default = FALSE.
#'
#' @param big_data Logical. If TRUE, sets a multisession \code{future} plan for parallel processing. Default = FALSE.
#'
#' @param ofac The Lomb-Scargle oversampling factor passed to [analyze_timeseries.lomb]. Default = 10.
#'
#' @param lomb_pvalue The significance level used for the Lomb-Scargle analysis. Default = 0.01.
#'
#' @param binning_n A numeric indicating the amount of bins over which to run the smoothing average. Default = 4.
#'
#' @return A named list of data.frames containing the output of \code{butterworth_filter}, [find_gaps()],
#' [make_time_windows()], [analyze_timeseries.acf()], [analyze_timeseries.lomb()], and
#' [analyze_timeseries.cosinor()] for each measurement value.
#'
#' @name process_timeseries
#' @rdname process_timeseries
#'
#' @export process_timeseries.rmv_gaps
#'
#' @examples
#' \dontrun{
#' processed_data <- process_timeseries.main(list_of_dfs, sampling_rate = "1 hour")
#' }
#'
#' @importFrom dplyr select right_join bind_rows filter
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom lubridate hour ceiling_date
#' @importFrom furrr future_map furrr_options
#'
#'
#'
#'



process_timeseries.rmv_gaps <- function(df = NULL, sampling_rate = NULL) {

  df <- right_join(df,
                   find_gaps(times = df$datetime, sampling_rate = sampling_rate),
                                by = "datetime")
  return(df)

}


#' @rdname process_timeseries
#' @export
process_timeseries.na_to_zero <- function(df = NULL) {

#Remove NA for missing data points, this is necessary for the autocorrelation
#Turn NA's into 0
df$value <- ifelse(is.na(df$value), 0, df$value)
return(df)

}



#' @rdname process_timeseries
#' @export
process_timeseries.waveform <- function(df = NULL,
                                        detrend_data = TRUE,
                                        smooth_data = FALSE,
                                        butterworth = TRUE,
                                        f_low = 1/4,
                                        f_high = 1/73,
                                        order = 2,
                                        binning_n = 4) {

if (smooth_data) { butterworth = FALSE }

df = smooth_and_detrend(df, smooth_data = smooth_data, detrend_data = detrend_data, binning_n = binning_n)

if (butterworth)
df = butterworth_filter(df, order = order, f_low = f_low, f_high = f_high, plot = FALSE)


return(df)

}







#' @rdname process_timeseries
#' @export
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
                                    big_data = FALSE,
                                    ofac = 1,
                                    lomb_pvalue = 0.01) {

  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  if (big_data) future::plan(future::multisession)

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

      #Lomb-Scargle Pipeline

      lsp_results = analyze_timeseries.lomb(df = x, sampling_rate = sampling_rate, from = from, to = to, ofac = ofac, alpha = lomb_pvalue)

      lsp_cosinor = analyze_timeseries.cosinor(x, sampling_rate = sampling_rate, period = lsp_results$period)

      return(list(data = x,
                  acf = list(results = acf_results,
                             cosinor = acf_cosinor),
                  lomb = list(results = lsp_results,
                              cosinor = lsp_cosinor)
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
  #Lomb-Scargle Pipeline
  lsp_results = analyze_timeseries.lomb(df = df, sampling_rate = sampling_rate, from = from, to = to, ofac = ofac, alpha = lomb_pvalue)
  lsp_cosinor = analyze_timeseries.cosinor(df, sampling_rate = sampling_rate, period = lsp_results$period)


  return(list(data = df,
              acf = list(results = acf_results,
                         cosinor = acf_cosinor),
              lomb = list(results = lsp_results,
                          cosinor = lsp_cosinor)
              )
         )

  }


}

#' @rdname process_timeseries
#' @export
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
                                    big_data = FALSE,
                                    ofac = 10,
                                    lomb_pvalue = 0.01) {
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  if (big_data) future::plan(future::multisession)

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
                                big_data = big_data,
                                ofac = ofac,
                                lomb_pvalue = lomb_pvalue)
      }
    )


  )

}

