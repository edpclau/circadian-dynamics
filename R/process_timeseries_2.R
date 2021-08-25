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
#' @export
#'
#' @examples
#' processed_data <- process_timeseries(df = raw_data, sampling_rate = "30 min")
#'
#' @importFrom dplyr select right_join bind_rows filter
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom lubridate hour ceiling_date
#' @importFrom future plan multisession availableWorkers
#' @importFrom furrr future_map
#'
process_timeseries_2 <- function(df = NULL, sampling_rate = NULL, window_size_in_days = 3, window_step_in_days = 1,
                               detrend_data = TRUE, butterworth = TRUE,
                               f_low = 1/4, f_high = 1/73, order = 2, plot = TRUE,
                               binning_n = 4, big_data = FALSE) {



  ###### Flow control parameters######
  if (big_data){
    workers = length(availableWorkers()) - 2
  } else {workers = 1}

  #Plan for paralellization
  plan(multisession, workers = workers)

  #Filter out dates before the first midnight.
  if (hour(min(df$datetime)) != 0) {

    df <- filter(df, datetime >= ceiling_date(min(df$datetime), unit = "1 day"))
  }

  df <- right_join(df,
                                find_gaps(times = df$datetime, sampling_rate = sampling_rate),
                                by = "datetime")

  df$value <- ifelse(is.na(df$value), 0, df$value)

  # Insert NA for missing data points, this is necessary for the autocorrelation
  # Consider making this into an if statetment type thing that is only turned ON when "autocorrelation" == TRUE

  # Here we create a moving window
  # The window size will be dynamic as well as steps to slide the window
  df <- make_time_windows_2(df,
                            window_size_in_days = window_size_in_days,
                            window_step_in_days = window_step_in_days,
                            workers = workers
                            )



  ##### Smooth or Detrend Data #####

  df <- future_map(df,
                   .f = smooth_and_detrend,
                   smooth_data = FALSE,
                   detrend_data = detrend_data,
                   binning_n = binning_n,
                   .options = furrr_options(seed = TRUE)
                   )


  #butterworth by window
  if (butterworth) {
  df <- future_map(df,
              .f = ~ butterworth_filter_2(.x, order = order, f_low = f_low, f_high = f_high, plot = FALSE),
              .options = furrr_options(seed = TRUE)
    )
  }


  return(df)
}
