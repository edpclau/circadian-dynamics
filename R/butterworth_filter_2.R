#' Butterworth Filter Helper
#' @description
#' A helper function which wraps around the [signal::butter()] and [signal::filfilt()] functions.
#' It can be either a low or high pass filter for a given period. (The function can take the period argument
#' because it does the conversion to frequency automatically. (frequency = 1/period).
#'
#' @usage
#' butterworth_filter(df = NULL, order = 2,
#' f_low= 1/4, f_high = 1/72, plot = TRUE)
#'
#' @param df required. A data.frame object where column 1 is a POSIXct object and the other columns are measurement values.
#' @param order filter order. Default = 2.
#' @param f_low Frequency for the low pass filter. Default = 1/4.
#' @param f_high Frequency for the high pass filter. Default = 1/72.
#' @param plot logical. If TRUE (default) plots the filtered data over the raw data. Red line is the low pass filter. Blue is the high pass filter. If FALSE, does not plot.
#' @export butterworth_filter_2
#' @examples
#'
#' butter <- butterworth_filter(df = data, f_low = 1/4, f_high = 1/72)
#' print(butter)
#'
#' @importFrom signal butter filtfilt


butterworth_filter_2 <- function(df = NULL, order = 2, f_low= 1/4, f_high = 1/72, plot = TRUE, ...) {


  ##### Flow Control #####
  #Get last column
  last_col = ncol(df)


  ##### Butterworth #####
  # Define parameters
  # Low Pass
  suppressMessages({

    bf <- butter(order, f_low, type = "low")
    b1 <- filtfilt(bf, df[[last_col]])

    # High Pass
    bf <- butter(order, f_high, type = "high")
    b2 <- filtfilt(bf, b1)

  })

  # Plots
  if (plot) {
      plot(df[['datetime']], df[[last_col]], type = "l")
      lines(df[['datetime']], b1, col = "red", lwd = 1.5)
      lines(df[['datetime']], b2, col = "blue", lwd = 1.5)

    }


  # return the filtered data
  df$butterworth <- b2

  return(df)

}

