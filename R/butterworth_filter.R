#' Butterworth Filter Helper
#' @description
#' A helper function which wraps around the [signal::butter()] and [signal::filfilt()] functions.
#' It can be either a low or high pass filter for a given period. (The function can take the period argument
#' because it does the conversion to frequency automatically. (frequency = 1/period).
#'
#' @usage
#' butterworth_filter(df = NULL, period = 24,
#'      type = c("low", "high"), plot = TRUE)
#'
#' @param df required. A data.frame object where column 1 is a POSIXct object and the other columns are measurement values.
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
#' @importFrom signal butter
#' @importFrom signal filtfilt
#' @importFrom dplyr select
#' @importFrom dplyr everything

butterworth_filter <- function(df = NULL, period = 24, type = c("low", "high"), plot = TRUE) {

##### Flow Control #####
  if (is.null(df)) { stop("must include a data.frame") }
  type <- match.arg(type, choices = c("low", "high"))



##### Butterworth #####
# Define parameters
bf <- butter(1, 1/period , type=type)


b1 <-  map_dfc(2:ncol(df),
       .f = ~ filtfilt(bf, df[[.]])
       )


# Plots
if (plot) {
for (i in 2:ncol(df)) {
plot(df[[1]], df[[i]], type = "l")
lines(df[[1]], b1[[i-1]], col = "red", lwd = 1.5)
mtext(paste("IND", i))
answer <- readline("Press 'Enter' to plot the next figure. Write 'done' to finish.")
if (answer != "") {break()}
}
}

# return the filtered data
names(b1) <- names(df[,c(2:ncol(df))])
b1$datetime <- df[[1]]
b1 <- select(b1, datetime, everything())
return(b1)

}

