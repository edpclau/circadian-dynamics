#' Cosinor Analysis
#'
#' @usage
#' analyze_timeseries.cosinor(df = data, timeseries_datetime = NULL, values = NULL, sampling_rate = "30 min", period = 48, na.action = na.omit)
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
#' @importFrom tibble tibble_row tibble
#' @importFrom dplyr mutate n
#' @importFrom broom tidy glance
#' @importFrom rlang is_empty
#' @import magrittr
#' @importFrom lubridate duration
#' @importFrom stringr str_extract str_remove
analyze_timeseries.cosinor <- function(df = NULL, sampling_rate = NULL, period = NULL, na.action = na.omit) {



#### ERROR CHECK (too few timepoints) ######
  if (nrow(df) < 3) {
   return(
     list(
      mesor = NA,
      amplitude = NA,
      amplitude_se = NA,
      acrophase = NA,
      acrophase_se = NA,
      phase = NA,
      phase_se = NA,
      adj_r_squared = NA,
      p_value = NA,
      wave = NA,
      cos_coeff = NA,
      sin_coeff = NA
    )
   )
  }
##### Base Cases #####
  if (is_empty(period)) {
    period = 24
  } else if (is.na(period)){
    period = 24
  }


  ###### Flow control parameters######
  #1. Must have sampling rate
  if (is.null(sampling_rate)) {stop("Must include sampling_rate. ex. '30 minutes', '1 hour', '4 seconds', '100 days'.")}

  #2. Sampling Rate
  sampling_bin_size = as.numeric(str_extract(sampling_rate, "\\d*"))
  sampling_rate = str_remove(sampling_rate, "\\d* *")

  #3. Period must be in the correct sampling_rate
  period = as.numeric(lubridate::duration(period, 'hours'), sampling_rate)


  ##### Format the data so we can run the cosinor ####
  # Insert a sample number for every timepoint
  samples = seq(1, nrow(df))

  # Calculate sin and cos widths
  # Period and time_value must be in the same units
  sinw <- sin(2*pi*samples/period)

  cosw <- cos(2*pi*samples/period)

  # Regression model for Cosinor
  model <- lm(df[[ncol(df)]] ~  sinw + cosw)

  # Coefficients of the model. The intercept = MESOR.
  MESOR <- as.numeric(model$coefficients[1])
  sin_coeff <- as.numeric(model$coefficients[2])
  cos_coeff <- as.numeric(model$coefficients[3])
  sin_se <- tidy(model)$std.error[2]
  cos_se <- tidy(model)$std.error[3]

  # Calculating Amplitude and phase
  # Amplitude of the function = square root of (sin_coeff^2 + cos_coeff^2)
  amplitude <- sqrt(sin_coeff^2 + cos_coeff^2)
  amplitude_se <- sqrt((sin_coeff^2*sin_se^2) + (cos_coeff^2 * cos_se^2))/ amplitude^2
  #Phase equals arctan(- cos_coeff / sin_coeff)
  acrophase <- atan( sin_coeff / cos_coeff )
  acrophase_se <- ((cos_se^2 * sin_coeff^-2) + (cos_coeff^2 / sin_coeff^3 * sin_se^2)) / (1 + (cos_coeff/sin_coeff)^2)^2


  if (cos_coeff < 0 & sin_coeff >= 0) {
    acrophase <-   acrophase + pi
  }
  if (cos_coeff < 0 & sin_coeff < 0) {
    acrophase <- pi + acrophase
  }

  if (cos_coeff >= 0 & sin_coeff < 0) {
    acrophase <- 2*pi + acrophase
  }

  # if (cos_coeff >= 0 & sin_coeff >= 0) {
  #   acrophase <- acrophase + pi
  # }


  #Calculating the phase and translating it into the same units as the sampling_rate
  time_offset <- acrophase * period / (2*pi) # We translate the phase into time units

  time_offset_se <- acrophase_se * period / (2*pi)

  phase <- as.numeric(duration(paste(sampling_bin_size, sampling_rate, sep = " ")) * time_offset, sampling_rate)

  phase_se <- as.numeric(duration(paste(sampling_bin_size, sampling_rate, sep = " "))  * time_offset_se, sampling_rate)

  # Model fit variables
  # R-squared, how well the model matches the data
  adj_r_squared <- glance(model)$adj.r.squared
  # p.value if that r-squared is significant
  model_p.value <- glance(model)$p.value


  results <- list(
    mesor = MESOR,
    amplitude = amplitude,
    amplitude_se = amplitude_se,
    acrophase = acrophase,
    acrophase_se = acrophase_se,
    phase = phase,
    phase_se = phase_se,
    adj_r_squared = adj_r_squared,
    p_value = model_p.value,
    wave = MESOR + (sin_coeff * sinw) + (cos_coeff * cosw),
    cos_coeff = cos_coeff,
    sin_coeff = sin_coeff
  )

  return(results)
}
