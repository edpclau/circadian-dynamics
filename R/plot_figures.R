#' Draw and Export plots from the analysis
#'
#' @usage export_plots(filename = "processed_and_analyzed_data.pdf", processed_data = NULL,
#' rythm_analysis_data = NULL, autocorrelation = TRUE,
#' lomb_scargle = TRUE, cosinor_fit = c("lomb_scargle", "autocorrelation"),
#' dir_choose_gui = TRUE)
#' @param filename  A charater string to name the ".pdf" file outputted. Default = "processed_and_analyzed_data".
#' @param processed_data data.frame returned from the process_timeseries or multivariate_process_timeseries functions.
#' @param rythm_analysis_data data.frame returned from the rythm_analysis_by_window and multivariate_rythm_analysis functions.
#' @param autocorrelation If TRUE (default) plots autocorrelation. FALSE does not plot autocorrelation.
#' @param lomb_scargle If TRUE (default) plots lomb scargle periodogram. FALSE does not plot periodogram.
#' @param cosinor_fit Method to fit the COSINOR to the data. Either "lomb_scargle" (default) or "autocorrelation".
#' @param dir_choose_gui If TRUE (default) a GUI will help select the folder in which to save the data and plots. If FALSE,
#' everything will be saved in the current directory.
#'
#' @export
#'
#' @examples
#' export_plots(processed_data = df_processed, rythm_analysis_data = df_analysis)
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom tidyr unnest
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
export_plots <- function(filename = "processed_and_analyzed_data.pdf", processed_data = NULL, rythm_analysis_data = NULL,
           autocorrelation = TRUE, lomb_scargle = TRUE,
           cosinor_fit = c("lomb_scargle", "autocorrelation"),
           dir_choose_gui = TRUE) {
##### Flow Control ####)
if (is.null(processed_data)) { stop("Must provide the output from 'process_timeseries'.")}
if (is.null(rythm_analysis_data)) { stop("Must provide the output from 'rythm_analysis_by_window'.")}

# You can choose between lomb_scargle or autocorrelation for "method". Period is the default.
cosinor_fit <- match.arg(cosinor_fit, choices = c("lomb_scargle", "autocorrelation"))

##### Select Where to save the figures #####
if (dir_choose_gui == TRUE) {
dir <- selectDirectory()
setwd(dir)
}

##### publish the figures #####
pdf(file = filename)
#### Parameters for how the plots will look on the page
n_plots = 3 + lomb_scargle + autocorrelation + (ncol(processed_data) > 4)
par(mar=c(2,4,1.75,2), mfrow = c(n_plots,1))


# Iterate over the windows

for (windows in unique(rythm_analysis_data$window)) {

cosinor <- rythm_analysis_data %>% filter(method == cosinor_fit, window == windows) %>%
    select(wave_x, wave_y, amplitude, phase_in_seconds, adj_r_squared, period) %>%
    unnest(c(wave_x, wave_y))

raw_data <- processed_data %>% filter(window == windows) %>% select(dates = 2, last_col(), 3)


# Raw Data
plot(raw_data[,c(1,3)], type="l", xlab="")

# Headers for the pade. Window number and dates in those windows
mtext(text = paste("Window number ", windows ,sep = ""), side=3, adj = 1, line = .5, cex = .5)
mtext(text=paste(min(raw_data$dates),max(raw_data$dates),sep=" --- "),side=3,outer=TRUE,padj=1.5)


# Show the next to last column of the data when we do multiple processes on the data
if (ncol(processed_data) > 4) {
# If the data is both smoothed and detrended, first show the smooth and then the smooth and detrended
plot(
  filter(processed_data, window == windows) %>% select(2,last_col(1)),
    type="l",
    xlab="")

  }



# Show the last column of the data, detrended, smooth, smooth and detrended
plot(
    raw_data[1:2],
    type="l",
    xlab="")



# Plot autocorrelation
if (autocorrelation == TRUE) {
ccf(x = raw_data[2], #Select the values in the window
    y = raw_data[2],
    na.action = na.pass,
    type = "correlation",
    plot = TRUE,
    lag.max = length(pull(select(filter(processed_data, window == windows), last_col()))),
    main = "")


points(
  x= filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>% pull(peak_lags) %>% unlist(),
  y= filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>% pull(peaks) %>% unlist(),
  pch=4,
  cex=3,
  col = 'red')


legend("topright",
       legend = c(paste("Period = ",
                        filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>% pull(period_hours)),
                  paste("C.C. =",
                        filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>% pull(autocorrelation_power) %>% round(digits = 3))),
       bty = "n",
       cex = 1)

}





# Plot Lomb-Scargle
if (lomb_scargle == TRUE) {
plot(
  x = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(scanned) %>% unlist(),
  y = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(normalized_power) %>% unlist(),
  log = "x",
  type = "l",
  xlab = "Period",
  ylab = "Power (lomb)")

abline(
  h = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(sig_level),
  lty = "dashed")

legend("topright",
       legend = c(paste("Period = ",
                        filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(period) %>% round(digits = 3)),
                  paste("Power =",
                        filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(power) %>% round(digits = 3))),
       bty = "n",
       cex = 1)
}




# Cosinor Fit

plot(x = raw_data[[1]], y = raw_data[[2]], type = "l", xlab = "Dates", ylab = "Values")
lines(x = cosinor$wave_x, y = cosinor$wave_y, type = "l", col = "blue")
legend("topright",
       legend = c(paste("Amplitude = ", unique(round(cosinor$amplitude, digits = 3))),
                  paste("Phase =", unique(round(cosinor$phase_in_seconds, digits = 3))),
                  paste("PR = ", unique(round(cosinor$adj_r_squared, digits = 3))),
                  paste("Period = ", unique(round(cosinor$period, digits = 3)))),
       bty = "n",
       cex = 1)




}


dev.off()

}
