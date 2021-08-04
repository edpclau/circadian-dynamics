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

#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom pracma movavg detrend
#'
smooth_and_detrend <- function(df = NULL, smooth_data = TRUE, binning_n = 4,
                                      detrend_data = TRUE) {


  ####### Data Smoothing or Detrending ######
  if (smooth_data & detrend_data) {
      df =  df %>%
          dplyr::mutate(
            smoothed =  pracma::movavg(value, n = binning_n, type = "s"),
            smoothed_and_detrended = c(pracma::detrend(smoothed)))



  } else if (smooth_data) {

    df =  df %>%
      dplyr::mutate(
        smoothed =  pracma::movavg(value, n = binning_n, type = "s"))


  } else if (detrend_data) {

    df =  df %>%
      dplyr::mutate(
        detrended =  c(pracma::detrend(value)))

  }

  return(df)
}
