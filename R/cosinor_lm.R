#' Cosinor Analysis
#'
#' @usage
#' cosinor_lm(df = data, timeseries_datetime = NULL, values = NULL, sampling_rate = "30 min", period = 48, na.action = na.omit)
#'
#' @description COSINOR analysis of a timeseries.
#'
#' @param df
#' A data.frame which contains the dates of a time series in column 1 and the values in column 2.
#'
#' @param timeseries_datetime
#' A vector of class POSICXct which contains the dates over which to run a COSINOR analysis.
#'
#' @param values
#' The data to which we want to find the period.
#'
#' @param sampling_rate
#' A character string indicating the sampling rate of the data. Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @param period
#' A numeric indicating the period to analyse. Must be in the same units as the sampling rate.
#' Examples: If the goal is to evaluate a 24 "hour" period but the sampling rate is "30 minutes",
#' the period to use is a  48 "30 minutes" period.
#'
#' @param na.action
#' Default is na.omit which excludes NA values from the analysis. See [stats::lm()] for a more detailed description.
#'
#' @return
#' A data.frame with:
#' MESOR: The intercept of the regression. ie. Mean of the COSINOR fit.
#' Amplitude: Amplitude of the COSINOR fit.
#' Amplitude_se
#' Acrophase: Phase shifting of the fit in radians.
#' Acrophase_se
#' Phase_in_seconds: Acrophase in seconds.
#' Phase_se_seconds
#' Adj_r_squared: The PR or Percent Rythm of the fit.
#' Cosinor_p_value: p.value for the regression. Significance indicates the period matches the data.
#' Wave_y = MESOR + amplitude * cosw : The use is to plot the fit.
#' Wave_x = timeseries_datetime + phase_in_seconds : The use is to plot the fit.
#'
#' @seealso
#' Barnett, A. G., & Dobson, A. J. (2010).
#' Analysing Seasonal Health Data. Statistics for Biology and Health.
#' doi:10.1007/978-3-642-10748-1
#'
#'Tong, Y. L. (1976).
#'Parameter Estimation in Studying Circadian Rhythms.
#'Biometrics, 32(1), 85. doi:10.2307/2529340
#'
#' @export
#'
#' @examples
#' cosinor <- cosinor_lm(df = data, sampling_rate = "30 min", period = 48)
#'
#'
cosinor_lm <- function(df = NULL, timeseries_datetime = NULL, values = NULL, sampling_rate = NULL, period = NULL, na.action = na.omit) {

###### Flow control parameters######
#1. Either a df or two lists with the time_series and values must be supplied. If a df is not supplied, turn the
# two lists into a df.
if (is.null(df) & (is.null(timeseries_datetime) | is.null(values))) {
  stop("If a data.frame is not supplied. Must include both timeseries and values.")
  } else if (is.null(df)) { df = tibble::tibble(timeseries_datetime, values) }
#2.must have sampling rate and period
if (is.null(period)) {stop("Must include period. Period must be in the same units as the sampling rate")}
if (is.null(sampling_rate)) {stop("Must include sampling_rate. ex. '30 minutes', '1 hour', '4 seconds', '100 days'.")}
#3. Change the names of the df columns so we can work with it inside the function
if (!is.null(df)) {
  names(df) <- c("timeseries_datetime", "values")
}

##### Format the data so we can run the cosinor ####
# Insert a sample number for every timepoint

df <- df %>% dplyr::mutate(sample = 1:n())

# Calculate sin and cos widths
# Period and time_value must be in the same units
sinw <- sin(2*pi*df$sample/period)

cosw <- cos(2*pi*df$sample/period)

# Regression model for Cosinor
model <- lm(df$values ~  sinw + cosw)

# Coefficients of the model. The intercept = MESOR.
MESOR <- as.numeric(model$coefficients[1])
sin_coeff <- as.numeric(model$coefficients[2])
cos_coeff <- as.numeric(model$coefficients[3])
sin_se <- broom::tidy(model)$std.error[2]
cos_se <- broom::tidy(model)$std.error[3]

# Calculating Amplitude and phase
# Amplitude of the function = square root of (sin_coeff^2 + cos_coeff^2)
amplitude <- sqrt(sin_coeff^2 + cos_coeff^2)
amplitude_se <- sqrt((sin_coeff^2*sin_se^2) + (cos_coeff^2 * cos_se^2))/ amplitude^2
#Phase equals arctan(- cos_coeff / sin_coeff)
acrophase <- atan( sin_coeff / cos_coeff )
acrophase_se <- ((cos_se^2 * sin_coeff^-2) + (cos_coeff^2 / sin_coeff^3 * sin_se^2)) / (1 + (cos_coeff/sin_coeff)^2)^2

if (cos_coeff < 0 & sin_coeff < 0) {
  acrophase <- acrophase + pi
}
# if (cos_coeff < 0 & sin_coeff > 0) {
#   acrophase <- - acrophase
# }
if (cos_coeff < 0 & sin_coeff >= 0) {
  acrophase <- pi - acrophase
}


time_offset <- acrophase * period / (2*pi) # We translate the phase into time units
time_offset_se <- acrophase_se * period / (2*pi)
phase_in_seconds <- lubridate::period(sampling_rate) %>% lubridate::period_to_seconds() * time_offset
phase_se_seconds <- lubridate::period(sampling_rate) %>% lubridate::period_to_seconds() * time_offset_se

# Model fit variables
# R-squared, how well the model matches the data
adj_r_squared <- broom::glance(model)$adj.r.squared
# p.value if that r-squared is significant
model_p.value <- broom::glance(model)$p.value


results <- tibble::tibble_row(MESOR, amplitude, amplitude_se, acrophase, acrophase_se, phase_in_seconds, phase_se_seconds,
                              adj_r_squared, cosinor_p_value = model_p.value, wave_y = list(MESOR + amplitude * cosw),
                              wave_x = list(df$timeseries_datetime + phase_in_seconds))

return(results)
}
