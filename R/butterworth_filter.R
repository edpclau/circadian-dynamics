#' Butterworth Filter Helper
#' @description
#' A helper function which wraps around the [signal::butter] and [signal::filfilt] functions.
#' It can be either a low or high pass filter for a given period. (The function can take the period argument
#' because it does the conversion to frequency automatically. (frequency = 1/period).
#'
#' @usage
#' butterworth_filter(df = NULL, period = 24,
#'      type = c("low", "high"), plot = TRUE)
#'
#' @param df required. A data.frame object where column 1 is a POSIXct object and column 2 is the measurement values.
#' @param period The period on which we want to draw the threshold (default = 24).
#' @param type A string indicating if a low or high pass filter will be used. options: "low" (default), "high".
#' @param plot logical. If TRUE (default) plots the filtered data over the raw data. If FALSE, does not plot.
#' @export
#' @examples
#'
#' butter <- butterworth_filter(df = data,
#'            period = 24, type = "low", plot = FALSE)
#' print(butter)
#'

butterworth_filter <- function(df = NULL, period = 24, type = c("low", "high"), plot = TRUE) {

##### Flow Control #####
  if (is.null(df)) { stop("must include a data.frame") }
  type <- match.arg(type, choices = c("low", "high"))


##### Butterworth #####
# Define parameters
bf <- signal::butter(1, 1/period , type=type)
# Filter signal
bf <- signal::filtfilt(bf, df[[2]])


# Plots
if (plot) {
plot(df[[1]], df[[2]], type = "l")
lines(df[[1]], b1, col = "red", lwd = 1.5)
}

# return the filtered data
df[[2]] <- b1
return(df)

}

