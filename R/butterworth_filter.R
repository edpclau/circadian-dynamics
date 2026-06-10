#' Butterworth Filter Helper
#' @description
#' A helper that wraps [signal::butter()] and [signal::filtfilt()] to apply a
#' two-pass (zero-phase) band-pass filter: a low-pass stage followed by a high-pass
#' stage.
#'
#' \strong{The cut-offs are normalised frequencies}, given as a fraction of the
#' Nyquist frequency (half the sampling rate), exactly as [signal::butter()]
#' expects. They are NOT derived from a period and are NOT rescaled by the sampling
#' rate inside this function. The caller must convert a physical cut-off to a
#' normalised frequency for the data's own sampling rate before calling (see the
#' package example script, which scales \code{f_low}/\code{f_high} by the sampling
#' rate).
#'
#'
#' @param df required. A data.frame object where column 1 is a POSIXct object and the other columns are measurement values.
#' @param order filter order. Default = 2.
#' @param f_low Normalised cut-off (fraction of Nyquist) for the low-pass stage. Default = 1/4.
#' @param f_high Normalised cut-off (fraction of Nyquist) for the high-pass stage. Default = 1/72.
#' @param plot logical. If TRUE (default) plots the filtered data over the raw data. Red line is the low pass filter. Blue is the high pass filter. If FALSE, does not plot.
#' @param ... Additional arguments (currently unused), reserved for future use.
#' @return The input data.frame \code{df} with an additional column \code{butterworth} containing the filtered values.
#' @export butterworth_filter
#' @examples
#' \dontrun{
#' butter <- butterworth_filter(df = data, f_low = 1/4, f_high = 1/72)
#' print(butter)
#' }
#'
#' @importFrom signal butter filtfilt


butterworth_filter <- function(df = NULL, order = 2, f_low= 1/4, f_high = 1/72, plot = TRUE, ...) {


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
      cols <- okabe_ito(8)  # colourblind-safe; blue = low-pass, vermillion = band-pass
      plot(df[['datetime']], df[[last_col]], type = "l", col = "grey50",
           xlab = "Datetime", ylab = "Activity", main = "Butterworth band-pass filter")
      lines(df[['datetime']], b1, col = cols[5], lwd = 1.5)
      lines(df[['datetime']], b2, col = cols[6], lwd = 1.5)
      legend("topright", legend = c("raw", "low-pass", "band-pass (final)"),
             col = c("grey50", cols[5], cols[6]), lwd = c(1, 1.5, 1.5), bty = "n", cex = 0.8)
    }


  # return the filtered data
  df$butterworth <- b2

  return(df)

}

