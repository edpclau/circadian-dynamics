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
#' @export
#' @examples
#'
#' butter <- butterworth_filter(df = data, f_low = 1/4, f_high = 1/72)
#' print(butter)
#'
#' @importFrom signal butter filtfilt
#' @importFrom dplyr select everything
#' @importFrom purrr map_dfc

butterworth_filter <- function(df = NULL, order = 2, f_low= 1/4, f_high = 1/72, plot = TRUE) {

##### Flow Control #####
  if (is.null(df)) { stop("must include a data.frame") }
  # type <- match.arg(type, choices = c("low", "high"))



##### Butterworth #####
# Define parameters
# Low Pass
suppressMessages({

bf <- butter(order, f_low, type = "low")
b1 <- map_dfc(2:ncol(df),
       .f = ~ filtfilt(bf, df[[.]]))
# High Pass
bf <- butter(order, f_high, type = "high")
b2 <-  map_dfc(1:ncol(b1),
               .f = ~ filtfilt(bf, b1[[.]])
)
})
# Plots
if (plot) {
for (i in 2:ncol(df)) {
plot(df[[1]], df[[i]], type = "l")
lines(df[[1]], b1[[i-1]], col = "red", lwd = 1.5)
lines(df[[1]], (b2[[i-1]]+mean(df[[i]])), col = "blue", lwd = 1.5)


answer <- readline("Press 'Enter' to plot the next figure. Write 'done' to finish.")
if (answer != "") {break()}
}
}

# return the filtered data
names(b2) <- names(df[,c(2:ncol(df))])
b2$datetime <- df[[1]]
b2 <- select(b2, datetime, everything())
return(invisible(b2))

}

