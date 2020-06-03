#' Helper function to prepare raw data for analysis
#'
#' @description Processes raw data in such a way that it can be directly inputted to the rythm_analysis_by_window function.
#'
#' @usage process_timeseries(df = NULL, sampling_rate = NULL, window_size_in_days = 3, window_step_in_days = 1, smooth_data = TRUE, detrend_data = FALSE, datetime = NULL, values = NULL)
#'
#' @param df A data.frame where the first column is a POSIXct object and the rest are independent measurement values.
#'
#' @param sampling_rate A character string indicating the sampling rate of the data. Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @param window_size_in_days a numeric indicating the width of the window size in day units.
#'
#' @param window_step_in_days a numeric indicating the amount of day by which to move the window in day units.
#'
#' @param smooth_data Logical. If TRUE (default) will smooth the measurement values useing a moving average. If FALSE measurement values won't be smoothed.
#'
#' @param detrend_data Logical. If TRUE (default) will detrend the data. If FALSE measurement values won't be detrended. If both, detrend_data and smooth_data are TRUE, the detrending will run over the smoothed data.
#'
#' @param butterworth Logical. If TRUE (default) will apply a buttwerworth filter to the measurement values using a moving average. If FALSE measurement values won't be filtered.
#'
#' @param datetime Optional if a data.frame is supplied. A POSIXct vector.
#'
#' @param values  Optional if a data.frame is supplied. A vector of values from a mesurement.
#'
#' @param binning_n A numeric which indicated the amount of bins over which to run the smoothing average. Default = 4.
#'
#' @param period The period on which we want to draw the threshold (default = 24).
#'
#' @param type A string indicating if a low or high pass filter will be used. options: "low" (default), "high".
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
process_timeseries <- function(df = NULL, sampling_rate = NULL, window_size_in_days = 3, window_step_in_days = 1,
                               smooth_data = TRUE, detrend_data = TRUE, butterworth = TRUE,
                               period = 24, type = "low", plot = TRUE,
                               binning_n = 4, datetime = NULL, values = NULL) {


  ###### Flow control parameters######
  #1. Either a df or two lists with the time_series and values must be supplied. If a df is not supplied, turn the
  # two lists into a df.
  if (is.null(df) & (is.null(datetime) | is.null(values))) {
    stop("If a data.frame is not supplied. Must include both timeseries and values.")
  } else if (is.null(df)) { df = tibble::tibble(datetime, values) }
  #2.must have sampling rate and period
  if (is.null(window_size_in_days) | is.null(window_step_in_days)) {stop("Must include window_size and window_step. These are measured in days.")}
  if (is.null(sampling_rate)) {stop("Must include sampling_rate. ex. '30 minutes', '1 hour', '4 seconds', '100 days'.")}
  #3. Change the names of the df columns so we can work with it inside the function
  if (!is.null(df)) {
    original_names <- names(df)
    names(df) <- c("datetime", "values")
  }


if(butterworth){
  df <- butterworth_filter(df, period = period, type = type, plot = plot)
}

completed_dates <- dplyr::right_join(df, find_gaps(times = df$datetime, sampling_rate = sampling_rate), by = "datetime")
# Insert NA for missing data points, this is necessary for the autocorrelation
# Consider making this into an if statetment type thing that is only turned ON when "autocorrelation" == TRUE

# Here we create a moving window
# The window size will be dynamic as well as steps to slide the window
windowed_data <- make_time_windows(completed_dates,
                                   window_size_in_days = window_size_in_days,
                                   window_step_in_days = window_step_in_days)


##### Smooth or Detrend Data #####
windowed_data <- dplyr::bind_cols(datetime = windowed_data$datetime,
                                  smooth_detrend_by_windows(dplyr::select(windowed_data, -datetime) ,
                                                            smooth_data = smooth_data, detrend_data = detrend_data,
                                                            binning_n = binning_n))

windowed_data <- windowed_data %>% dplyr::select(window, datetime, values, everything())
names(windowed_data)[names(windowed_data) %in% names(df)] <- original_names
windowed_data$window <- as.numeric(windowed_data$window)
return(windowed_data)
}
