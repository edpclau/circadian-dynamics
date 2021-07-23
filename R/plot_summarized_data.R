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
plot_summarized_data <- function(raw_data = NULL, analyzed_data = NULL, path = getwd()) {

##### Flow Control ####
  if (is.null(raw_data)) { stop("Must provide the raw data. The first column must be the datetime.")}
  if (is.null(analyzed_data)) { stop("Must provide the output from 'multivariate_rythm_analysis' or 'rythm_analysis'.")}
  df1 <- raw_data
  df2 <- analyzed_data
  #Plan for paralellization
  future::plan(future::multisession)

###### Plotting #####
furrr::future_map(names(df2),
           ~ {
pdf(paste0(path,"/", paste( .,"Summarized Data.pdf", sep = " ")), width = 9, height = 8)

lay <- layout(rbind(c(1,1,1,1,1,1,1,1,1,1,1,1),
                    c(2,2,2,2,3,3,3,3,4,4,4,4),
                    c(5,5,5,6,6,6,7,7,7,8,8,8)))



###### plot raw data #####

#axis
raw_x = dplyr::pull(df1,datetime)
raw_x_min = lubridate::floor_date(min(raw_x, na.rm = TRUE), unit = "day")
raw_x_max = lubridate::floor_date(max(raw_x, na.rm = TRUE), unit = "day")
raw_y = dplyr::pull(df1,.)

# plot
plot(raw_x, raw_y, type="l",
     ylab = "", xlab = "", main = paste(., "Summary", sep = " "), xaxt = "n")

# axis breaks and labels
axis(1, at = seq(raw_x_min, raw_x_max, by = lubridate::ddays(1)), labels = NA)
axis(1, at = lubridate::round_date(seq(raw_x_min, raw_x_max,
                 by = lubridate::ddays(lubridate::as.interval(raw_x_min, raw_x_max) / lubridate::ddays(15))),
                 unit = "day"),
     labels = format(
       lubridate::round_date(seq(raw_x_min, raw_x_max,
                                 by = lubridate::ddays(lubridate::as.interval(raw_x_min, raw_x_max) / lubridate::ddays(15))),
                             unit = "day"),
               "%b %d"))



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

if ( !all(is.na(to_plot[[2]]))) {
plot(to_plot,type="s", main ="Cosinor Phases", ylab = "Phase in hours", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)

} else {
  plot.new()
  title("Cosinor Phases = NA")
}


# plot Cosinor Percent Rythm
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,adj_r_squared)

plot(to_plot,type="s", main ="Percent Rythm", ylab = " Cosinor Adj. R-squared", xaxt = "n",
     ylim = c(0,1))
points(to_plot)
axis(1, at = to_plot$window)
abline(h = 0.5, col = "red", lty = 2)



# plot lomb_scargle periods
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,period)

if ( !all(is.na(to_plot[[2]]))) {
plot(to_plot, type="s", main ="Lomb-Scargle Periods" , ylab = "Period in hours", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)

} else {
  plot.new()
  title("Lom-Scargle Periods = NA")
}

#plot period found by autocorrelation
to_plot = df2[[.]] %>%
  dplyr::filter(method == "autocorrelation") %>%
  dplyr::select(window,period_hours) %>%
  dplyr::mutate(period_hours = as.numeric(stringr::str_remove(period_hours, "hours" )))

if (!all(is.na(to_plot[[2]]))) {
plot(to_plot,type="s", main="Autocorrelation Periods", ylab = "Period in hours", xlab = "Window", xaxt = "n")
points(to_plot)
axis(1, at = to_plot$window)

} else {
  plot.new()
  title("Autocorrelation Periods = NA")
}

# plot lomb_scargle p.value
to_plot = df2[[.]] %>%
  dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window,lsp_p_value) %>%
  dplyr::mutate(lsp_p_value = log(lsp_p_value/unique(df2[[1]]$alpha)))

if ( !all(is.na(to_plot[[2]])) ) {

#max y value:
  max_value = max(to_plot$lsp_p_value, na.rm = TRUE)
  min_value = min(to_plot$lsp_p_value, na.rm = TRUE)

plot(to_plot, type="s", main ="Lomb-Scargle Rythm Strength", ylab = "Log (P.value / Alpha)", xlab = "Window", xaxt = "n",
     ylim = if (min_value < 1 & 1 < max_value) {
       c(min_value, max_value)
     } else if (1 > max_value) {
       c(min_value, 1)
     } else if (1 < min_value) {
       c(1, max_value)
     } )
points(to_plot)
axis(1, at = to_plot$window)
abline(h = 1, lty = 2, col = "red")


} else {
  plot.new()
  title("Lomb-Scarg. Rythm Strength = NA")
}

# plot Rythm Strength
to_plot = df2[[.]] %>%
  dplyr::filter(method == "autocorrelation") %>%
  dplyr::select(window,rythm_strength)

if ( !all(is.na(to_plot[[2]]))) {
  #max min y value:
  max_value = max(to_plot$rythm_strength, na.rm = TRUE)
  min_value = min(to_plot$rythm_strength, na.rm = TRUE)

plot(to_plot,type="s", main ="Autocor Rythm Strength" , ylab = "C.C. / 95% C.I.",
     xlab = "Window", xaxt = "n",
     ylim = if (min_value < 1 & 1 < max_value) {
       c(min_value, max_value)
     } else if (1 < min_value) {
       c(1, max_value)
     } else if (1 > max_value) {
       c(min_value, 1)
     })
points(to_plot)
axis(1, at = to_plot$window)
abline(h = 1, lty = 2, col = "red")

} else {
  plot.new()
  title("Autocorrelation Rythm Strength = NA")
}

 dev.off()

}

)

}



