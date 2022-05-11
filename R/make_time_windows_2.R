# Takes a time series data.frame and returns iterable time windows of any size
#' Create iterable time windows
#' @description Creates iterable time windows for a data.frame
#' @usage make_time_windows(data = NULL, window_size_in_days = 3, window_step_in_days = 1)
#' @param data a data.frame with 2 columns. Column 1 must be a POSIXct object and column 2 must be the measurement values.
#' @param window_size_in_days a numeric indicating the width of the window size in day units.
#' @param window_step_in_days a numeric indicating the amount of day by which to move the window in day units.
#'
#' @return
#' A data.frame cotaining:
#'
#' window     iterator by which to subset the data.frame
#' values     raw measurement data
#' @export
#'
#' @examples
#' windowed_data <- make_time_windows_2(data, window_size_in_days = 3, window_step_in_days = 1)
#'
#' @importFrom lubridate days
#'
make_time_windows_2 <- function(df = NULL, window_size_in_days = 3, window_step_in_days = 1){

  # Set parameters
  window_size <- days(window_size_in_days) #Width of the window


  # Finding dates where the window does not exceed the last time point in the data
  times <- data$datetime

  step = seq(from = min(times), to = max(times), by = paste(window_step_in_days, "day")) #days to move the window

  return(step)


}


