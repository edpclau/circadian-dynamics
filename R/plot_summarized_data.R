#' Plot a sumarrized version of the Rythm data found by Rythm_analysis
#' @usage plot_summarized_data(raw_data = NULL, analyzed_data = NULL,
#' dir_choose_gui = TRUE)
#'
#' @param raw_data data.frame or tibble containing the raw data. The first column must be a datetime.
#' @param analyzed_data data.frame returned from the rythm_analysis_by_window and multivariate_rythm_analysis functions.
#' @param dir_choose_gui If TRUE (default) a GUI will help select the folder in which to save the data and plots. If FALSE,
#' everything will be saved in the current directory.
#'
#' @return
#' @export
#'
#' @examples
#' plot_summarized_data(raw_data = monitor_downsampled, analyzed_data = monitor_analysis, dir_choose_gui = TRUE)
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom purrr map
#' @importFrom dplyr select mutate filter pull
#' @importFrom magrittr '%>%'
#'
plot_summarized_data <- function(raw_data = NULL, analyzed_data = NULL, dir_choose_gui = TRUE) {

##### Flow Control ####
  if (is.null(raw_data)) { stop("Must provide the raw data. The first column must be the datetime.")}
  if (is.null(analyzed_data)) { stop("Must provide the output from 'multivariate_rythm_analysis' or 'rythm_analysis'.")}
  df1 <- raw_data
  df2 <- analyzed_data

  if (dir_choose_gui) {
    current_dir <- getwd()
    directory <- rstudioapi::selectDirectory()
    new_dir1 <- paste0(directory,"/", "Summary_plots")
    dir.create(new_dir1)
    setwd(new_dir1)

  }


###### Plotting #####
purrr::map(names(df2),
           ~ {
pdf(paste( .,"Summarized Data.pdf", sep = " "), width = 9, height = 8)

lay <- layout(rbind(c(1,1,1,1,1,1,1,1,1,1,1,1),
                    c(2,2,2,2,3,3,3,3,4,4,4,4),
                    c(5,5,5,6,6,6,7,7,7,8,8,8)))



# plot raw data
plot(dplyr::pull(df1,datetime), dplyr::pull(df1,.), type="l",
     ylab = "", xlab = "", main = paste(., "Summary", sep = " "))

# plot Cosinor amplitudes gotten from lomb_scargle
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,amplitude)

plot(to_plot,type="s", main = "Cosinor Amplitudes", ylab = "Amplitude", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)


#plot cosinor acrophase calculated from lomb_scargle
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,phase_in_seconds)

plot(to_plot,type="s", main ="Cosinor Phases", ylab = "Phase in hours", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)


# plot Cosinor Percent Rythm
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,adj_r_squared)

plot(to_plot,type="s", main ="Percent Rythm", ylab = " Cosinor Adj. R-squared", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)


# plot lomb_scargle periods
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,period)

plot(to_plot, type="s", main ="Lomb-Scargle Periods" , ylab = "Period in hours", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)


#plot period found by autocorrelation
to_plot = df2[[.]] %>%
  dplyr::filter(method == "autocorrelation") %>%
  dplyr::select(window,period_hours) %>%
  dplyr::mutate(period_hours = as.numeric(stringr::str_remove(period_hours, "hours" )))

plot(to_plot,type="s", main="Periods Autocorrelation", ylab = "Period in hours", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)


# plot lomb_scargle p.value
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,lsp_p_value)

plot(to_plot, type="s", main ="Lomb-Scargle P-value", ylab = "P.value", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)


# plot aucocorrelation coefficients
to_plot = df2[[.]] %>%
  dplyr::filter(method == "autocorrelation") %>%
  dplyr::select(window,autocorrelation_power)

plot(to_plot,type="s", main ="Autocorrelation Coefficients" , ylab = "Correlation Coefficient",
     xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)

dev.off()

}

)

}



