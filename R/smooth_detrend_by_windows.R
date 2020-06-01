#' Smooth and/or Detrend Data by Windows
#' @usage smooth_detrend_by_windows(df = NULL, smooth_data = TRUE, detrend_data = TRUE, windows = NULL, values = NULL)
#' @param df a data.frame with 2 columns. The first column must be the windows. The second column the values to process.
#' @param smooth_data Logical. If TRUE (default) will smooth the measurement values useing a moving average. If FALSE measurement values won't be smoothed.
#' @param binning_n A numeric which indicated the amount of bins over which to run the smoothing average. Default = 4.
#' @param detrend_data Logical. If TRUE (default) will detrend the data. If FALSE measurement values won't be detrended. If both, detrend_data and smooth_data are TRUE, the detrending will run over the smoothed data.
#' @param windows Optional if a data.frame is supplied. A vector with the windows.
#' @param values  Optional if a data.frame is supplied. A vector of values from a mesurement.
#' @return
#'A data.frame conatining: /n
#'
#' window                 A vector with the windows. /n
#' values                 The raw measurement values. /n
#' smoothed               A column with the smoothed data. /n
#' detrended              A column with the detrended data. /n
#' smoothed_and_detrended A column with data that's been both smoothed and detrended.
#'
#' @export
#'
#' @examples
#' smoothed_data <- smooth_detrend_by_windows(df = windowed_data, smooth_data = TRUE, detrend_data = FALSE)
#'
smooth_detrend_by_windows <- function(df = NULL, smooth_data = TRUE, binning_n = 4,
                                      detrend_data = TRUE, windows = NULL, values = NULL) {

  ###### Flow control parameters######
  #1. Either a df or two lists with the time_series and values must be supplied. If a df is not supplied, turn the
  # two lists into a df. Datetime is optional and will not be used.
  if (is.null(df) & (is.null(windows) | is.null(values))) {
    stop("If a data.frame is not supplied. Must include both timeseries and values.")
  } else if (is.null(df)) { windowed_data = tibble::tibble(windows, values) }

  #2. Change the names of the df columns so we can work with it inside the function
  if (!is.null(df)) {
    windowed_data <- df
    names(windowed_data) <- c("window", "values")
  }

####### Data Smoothing or Detrending ######
if (smooth_data == TRUE & detrend_data == TRUE) {
  windowed_data <- windowed_data %>%
    dplyr::group_by(window) %>%
    dplyr::mutate(
      smoothed =  pracma::movavg(values, n = binning_n, type = "s"),
      smoothed_and_detrended = pracma::detrend(smoothed)) %>%
    dplyr::ungroup()

} else if (smooth_data == TRUE & detrend_data == FALSE) {
  windowed_data <- windowed_data %>%
    dplyr::group_by(window) %>%
    dplyr::mutate(
      smoothed =  pracma::movavg(values, n = binning_n, type = "s")) %>%
    dplyr::ungroup()

} else if (smooth_data == FALSE & detrend_data == TRUE) {
  windowed_data <- windowed_data %>%
    dplyr::group_by(window) %>%
    dplyr::mutate(
      detrended =  pracma::detrend(values)) %>%
    dplyr::ungroup()

} else {
  # If smooth and detrend are false, return the unchanged data.frame
  return(windowed_data)
}
return(windowed_data)
}
