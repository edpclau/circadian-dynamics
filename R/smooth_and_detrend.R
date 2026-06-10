#' Smooth and/or Detrend Data by Windows
#'
#' @param df a data.frame with 2 columns. The first column must be the windows. The second column the values to process.
#' @param smooth_data Logical. If TRUE (default) will smooth the measurement values useing a moving average. If FALSE measurement values won't be smoothed.
#' @param binning_n A numeric which indicated the amount of bins over which to run the smoothing average. Default = 4.
#' @param detrend_data Logical. If TRUE (default) will detrend the data. If FALSE measurement values won't be detrended. If both, detrend_data and smooth_data are TRUE, the detrending will run over the smoothed data.
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
#' \dontrun{
#' smoothed_data <- smooth_and_detrend(df = windowed_data, smooth_data = TRUE, detrend_data = FALSE)
#' }
#'

#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom pracma movavg detrend
#'
smooth_and_detrend <- function(df = NULL, smooth_data = TRUE, binning_n = 4,
                                      detrend_data = TRUE) {


  ####### Catch ERRORS (too little data) #######
  if (nrow(df) <= 1) {
    return(
      mutate(df, detrended =  NA)
      )
  }

  # pracma::movavg() errors with "Window length n must be greater than length of
  # time series" when the series is no longer than the smoothing window. Short
  # trailing windows (or animals that die early) can have <= binning_n rows; fall
  # back to no smoothing instead of crashing the furrr worker.
  if (smooth_data && nrow(df) <= binning_n) {
    warning("Window has <= binning_n rows; skipping moving-average smoothing.")
    smooth_data <- FALSE
  }


  ####### Data Smoothing or Detrending ######
  # Smooth first (if requested); detrend then runs on the smoothed signal when both
  # are TRUE (column `smoothed_and_detrended`), otherwise on the raw values (`detrended`).
  if (smooth_data) df <- mutate(df, smoothed = movavg(value, n = binning_n, type = "s"))
  if (detrend_data) {
    src <- if (smooth_data) df$smoothed else df$value
    out_col <- if (smooth_data) "smoothed_and_detrended" else "detrended"
    df[[out_col]] <- c(detrend(src))
  }

  return(df)
}
