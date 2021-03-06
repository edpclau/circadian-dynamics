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
#' @importFrom tidyr unnest drop_na
#' @importFrom magrittr '%>%'
#' @importFrom dplyr pull filter select last_col mutate
#' @importFrom lubridate hour dhours
#'
export_plots <- function(path = getwd(), filename = "processed_and_analyzed_data.pdf", processed_data = NULL,
                         rythm_analysis_data = NULL, autocorrelation = TRUE, lomb_scargle = TRUE,
           cosinor_fit = c("lomb_scargle", "autocorrelation"), dir_choose_gui = TRUE) {
##### Flow Control ####)
if (is.null(processed_data)) { stop("Must provide the output from 'process_timeseries'.")}
if (is.null(rythm_analysis_data)) { stop("Must provide the output from 'rythm_analysis_by_window'.")}

# You can choose between lomb_scargle or autocorrelation for "method". Period is the default.
cosinor_fit <- match.arg(cosinor_fit, choices = c("lomb_scargle", "autocorrelation"))

##### Select Where to save the figures #####
if (dir_choose_gui == TRUE) {
dir <- selectDirectory()
path = dir
}

##### publish the figures #####
pdf(file = paste0(path,"/",filename))
#### Parameters for how the plots will look on the page
n_plots = 2 + lomb_scargle + autocorrelation + (ncol(processed_data) > 4) + (ncol(processed_data) > 3)
par(mar=c(2,4,1.75,2), mfrow = c(n_plots,1))


# Iterate over the windows

for (windows in unique(rythm_analysis_data$window)) {

cosinor <- rythm_analysis_data %>% dplyr::filter(method == cosinor_fit, window == windows) %>%
    dplyr::select(wave_x, wave_y, amplitude, phase_in_seconds, adj_r_squared, period, cosinor_p_value) %>%
    tidyr::unnest(c(wave_x, wave_y))

raw_data <- processed_data %>% filter(window == windows) %>% select(dates = 2, 3)

# Raw Data
plot(raw_data, type="l", xlab="", xaxt = "n")
axis(1, at = seq(min(raw_data$dates), max(raw_data$dates), by = lubridate::dhours(4)),
     labels = lubridate::hour(seq(min(raw_data$dates), max(raw_data$dates), by = lubridate::dhours(4))))

# Headers for the pade. Window number and dates in those windows
mtext(text = paste("Window number ", windows ,sep = ""), side=3, adj = 1, line = .5, cex = .5)
mtext(text=paste(min(raw_data$dates),max(raw_data$dates),sep=" --- "),side=3,outer=TRUE,padj=1.5)


# Show the next to last column of the data when we do multiple processes on the data
if (ncol(processed_data) > 4) {
# If the data is both smoothed and detrended, first show the smooth and then the smooth and detrended
x_axis <- filter(processed_data, window == windows) %>% pull(2)
plot(
  filter(processed_data, window == windows) %>% select(2,last_col(1)),
    type="l",
    xlab="", xaxt = "n")
  axis(1, at = seq(min(x_axis), max(x_axis), by = lubridate::dhours(4)),
       labels = lubridate::hour(seq(min(x_axis), max(x_axis), by = lubridate::dhours(4))))

  }



# Show the last column of the data, detrended, smooth, smooth and detrended
if (ncol(processed_data) > 3) {
plot(
    filter(processed_data, window == windows) %>% select(2,last_col()),
    type="l",
    xlab="", xaxt = "n")
axis(1, at = seq(min(raw_data$dates), max(raw_data$dates), by = lubridate::dhours(4)),
     labels = lubridate::hour(seq(min(raw_data$dates), max(raw_data$dates), by = lubridate::dhours(4))))

}

# Plot autocorrelation
if (autocorrelation == TRUE) {

if(rle(c(drop_na(raw_data)[[2]]))$length[1] != length(drop_na(raw_data)[[2]])) {


autocor <- ccf(x = raw_data[2], #Select the values in the window
    y = raw_data[2],
    na.action = na.pass,
    type = "correlation",
    lag.max = length(pull(select(drop_na(filter(processed_data, window == windows)), last_col()))),
    plot = FALSE)

plot(x = autocor$lag, y = autocor$acf, type = "l", col = "blue", main = " ", xlab = "Lag", ylab = "ACF", xaxt = "n")
abline(v = (rythm_analysis_data %>% pull(from) %>% unique())*2)
abline(v = (rythm_analysis_data %>% pull(to) %>% unique())*2)
axis(1, at = seq(min(autocor$lag), max(autocor$lag), by = 4), labels = FALSE)
axis(1, at = seq(min(autocor$lag), max(autocor$lag), by = 12), labels = seq(min(autocor$lag), max(autocor$lag), by = 12))

#Calculate and add confidence intervals
error <- 2/sqrt(length(autocor$acf))
abline(h = mean(autocor$acf) - error, lty=2, col = "red")
abline(h = mean(autocor$acf) + error, lty=2, col = "red")

plot_points <- TRUE

} else {
  plot(x = seq(-length(raw_data[[2]]), length(raw_data[[2]]), by = 1),
       y = rep(0, (length(raw_data[[2]])*2) + 1),
       type = "l",
       col = "blue",
       main = " ",
       xlab = "Lag",
       ylab = "ACF")
  abline(v = (rythm_analysis_data %>% pull(from) %>% unique())*2)
  abline(v = (rythm_analysis_data %>% pull(to) %>% unique())*2)
  plot_points <- FALSE
}

if (plot_points) {
points(
  x= filter(rythm_analysis_data, window == windows, method == "autocorrelation")  %>% pull(peak_lags) %>% unlist(),
  y= filter(rythm_analysis_data, window == windows, method == "autocorrelation")  %>% pull(peaks) %>% unlist(),
  pch=4,
  cex=3,
  col = 'red')

}

if (!(filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>%
      pull(period_hours) %>%
      unlist() %>%
      is.na())) {

legend("topright",
       legend = c(paste("Period = ",
                        filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>% pull(period_hours)),
                  paste("C.C. =",
                        filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>% pull(autocorrelation_power) %>% round(digits = 3))),
       bty = "n",
       cex = 1)

} else {
legend("topright",
       legend = c(paste("Period = ",
                        filter(rythm_analysis_data, window == windows, method == "autocorrelation") %>% pull(period_hours)),
                  paste("C.C. =", "NA")),
       bty = "n",
       cex = 1)
}
}





# Plot Lomb-Scargle
if (filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>%
    pull(normalized_power) %>%
    unlist() %>% is.na() %>%
    all()) {
  lomb_scargle = FALSE
}

if (lomb_scargle == TRUE) {
  x = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(scanned) %>% unlist()
  y = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(normalized_power) %>% unlist()
  y_max = max(y, na.rm = TRUE)
  y_min = min(y, na.rm = TRUE)
  sig_level = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(sig_level)

plot(
  x = x,
  y = y,
  log = "x",
  type = "l",
  xlab = "Period",
  ylab = "Power (lomb)",
  xaxt = "n",
  ylim = if (sig_level > y_max) {
    c(y_min,sig_level)
  } else if (sig_level < y_min) {
    c(sig_level, y_max)
  } else { c(y_min, y_max)})
  minor_ticks <- seq(0, round(max(x)), by = 4)
  major_ticks <- seq(0, round(max(x)), by = 12)
  axis(1, at = minor_ticks, labels = FALSE)
  axis(1, at = major_ticks, labels = major_ticks)

abline(v = (rythm_analysis_data %>% pull(from) %>% unique()))
abline(v = (rythm_analysis_data %>% pull(to) %>% unique()))

if (filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(sig_level) <= max(y)) {
abline(
  h = sig_level,
  lty = "dashed",
  col = "red")
} else {
  abline(
    h = max(y) + 1,
    lty = "dashed",
    col = "red")
}


points(
  x= filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(period) %>% unlist(),
  y= filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(power) %>% unlist(),
  pch=4,
  cex=3,
  col = 'red')


power = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(power)
sig_level = filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(sig_level)
legend("topright",
       legend = c(
         if (!is.na(power)){
           if (power >= sig_level) {
           paste("Period = ",
                 filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(period) %>% round(digits = 3)) %>% paste("*")
           } else {
             paste("Period = ",
                   filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(period) %>% round(digits = 3))
           }
             } else {
           paste("Period = ",
                 filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(period) %>% round(digits = 3))
         },
         paste("Power =",
                filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(power) %>% round(digits = 3)),

         paste("p.value =",
                filter(rythm_analysis_data, window == windows, method == "lomb_scargle") %>% pull(lsp_p_value))),
       bty = "n",
       cex = 1)
}




# Cosinor Fit
data_for_cos_fit <- filter(processed_data, window == windows) %>% select(dates = 2,last_col())

plot(x = data_for_cos_fit[[1]], y = data_for_cos_fit[[2]], type = "l", xlab = "Dates", ylab = "Values", xaxt = "n")
axis(1, at = seq(min(data_for_cos_fit$dates), max(data_for_cos_fit$dates), by = lubridate::dhours(4)),
     labels = lubridate::hour(seq(min(data_for_cos_fit$dates), max(data_for_cos_fit$dates), by = lubridate::dhours(4))))

lines(x = cosinor$wave_x, y = cosinor$wave_y, type = "l", col = "blue")
abline(v = unique(cosinor$phase_in_seconds), lty = 1, col = "black")

if(all(is.na(cosinor$cosinor_p_value))){
legend("topright",
       legend = c(paste("Amplitude = ", unique(round(cosinor$amplitude, digits = 3))),
                  paste("Phase =", "NA"),
                  paste("PR = ", "NA"),
                  paste("Period = ", unique(round(cosinor$period, digits = 3))),
                    paste("p.value = ", "NA")
                  ),
       bty = "n",
       cex = 0.9)
} else {

legend("topright",
       legend = c(paste("Amplitude = ", unique(round(cosinor$amplitude, digits = 3))),
                  paste("Phase =", unique(round(cosinor$phase_in_seconds, digits = 3))),
                  paste("PR = ", unique(round(cosinor$adj_r_squared, digits = 3))),
                  paste("Period = ", unique(round(cosinor$period, digits = 3))),
                  paste("p.value = ", unique(signif(cosinor$cosinor_p_value, digits = 3)))
       ),
       bty = "n",
       cex = 0.9)

}


}


dev.off()

}
