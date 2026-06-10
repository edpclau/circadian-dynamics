#' Cosinor Analysis
#'
#'
#' @description COSINOR analysis of a timeseries.
#'
#' @param df
#' A data.frame which contains the dates of a time series in column 1 and the values in column 2.
#'
#' @param sampling_rate
#' A character string indicating the sampling rate of the data. Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @param period
#' A numeric indicating the period to analyse, given in HOURS regardless of the
#' sampling rate (e.g. \code{period = 24} for a 24-hour rhythm). The function
#' converts it internally to the number of samples per cycle using \code{sampling_rate}.
#'
#' @return
#' A named list with:
#' \code{period}: The period (in hours) that was fitted. Equals the supplied
#' \code{period}, or 24 when \code{period} is NA/empty (e.g. when neither ACF nor
#' Lomb-Scargle found an in-band peak), which lets callers tell the ACF-period and
#' Lomb-period cosinor fits apart.
#' \code{mesor}: Intercept of the regression, i.e. the mean of the cosinor fit.
#' \code{amplitude}, \code{amplitude_se}: Amplitude of the fit and its standard error.
#' \code{acrophase}, \code{acrophase_se}: Phase of the fit in radians and its standard error.
#' \code{phase}, \code{phase_se}: Acrophase in time units of \code{sampling_rate}, and its standard error.
#' \code{adj_r_squared}: Adjusted R-squared (percent rhythm) of the fit.
#' \code{p_value}: p-value of the regression; significance indicates the period matches the data.
#' \code{wave}: Fitted values (\code{mesor + sin_coeff * sin + cos_coeff * cos}) for plotting.
#' \code{cos_coeff}, \code{sin_coeff}: The fitted cosine and sine coefficients.
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
#' \dontrun{
#' res <- analyze_cosinor(df, sampling_rate = "1 hour", period = 24)
#' }
#'
#' @importFrom tibble tibble_row tibble
#' @importFrom dplyr mutate n
#' @importFrom broom glance
#' @importFrom stats vcov
#' @importFrom rlang is_empty
#' @import magrittr
#' @importFrom lubridate duration
analyze_cosinor <- function(df = NULL, sampling_rate = NULL, period = NULL) {



#### ERROR CHECK (too few timepoints) ######
  if (nrow(df) < 3) {
   return(
     list(
      period = NA,
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
      sin_coeff = NA,
      var_sin = NA,
      var_cos = NA,
      cov_sincos = NA,
      n_obs = nrow(df)
    )
   )
  }
##### Base Cases #####
  if (is_empty(period) || is.na(period)) period = 24


  ###### Flow control parameters######
  #1. Must have sampling rate
  if (is.null(sampling_rate)) {stop("Must include sampling_rate. ex. '30 minutes', '1 hour', '4 seconds', '100 days'.")}

  #2. Sampling Rate
  sr <- .parse_sampling_rate(sampling_rate)
  sampling_bin_size <- sr$bin
  sampling_rate <- sr$unit

  #3. Period must be in the correct sampling_rate (in number of samples per cycle).
  #   Keep the fitted period in hours (after the NA/empty -> 24 fallback above) so it
  #   can be reported back: this is what makes the ACF- vs Lomb-period fits distinguishable.
  period_hours = period
  period = as.numeric(lubridate::duration(period, 'hours'), sampling_rate) / sampling_bin_size


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
  # Amplitude = sqrt(sin_coeff^2 + cos_coeff^2)
  amplitude <- sqrt(sin_coeff^2 + cos_coeff^2)

  # Delta-method SE for amplitude using the model covariance matrix.
  # A = sqrt(b_s^2 + b_c^2);  Var(A) = (1/A^2) * [b_s^2 Vss + b_c^2 Vcc + 2 b_s b_c Vsc]
  V   <- vcov(model)
  # Term names come from the lm formula 'value ~ sinw + cosw'; keep these in sync if renamed.
  Vss <- V["sinw", "sinw"]; Vcc <- V["cosw", "cosw"]; Vsc <- V["sinw", "cosw"]
  amplitude_se <- sqrt(sin_coeff^2 * Vss + cos_coeff^2 * Vcc +
                         2 * sin_coeff * cos_coeff * Vsc) / amplitude

  # Acrophase via atan2 (correct quadrant), mapped to [0, 2*pi).
  acrophase <- atan2(sin_coeff, cos_coeff)
  if (acrophase < 0) acrophase <- acrophase + 2 * pi

  # Delta-method SE for acrophase.
  # phi = atan2(b_s, b_c); Var(phi) = (1/A^4)[b_c^2 Vss + b_s^2 Vcc - 2 b_s b_c Vsc]
  acrophase_se <- sqrt(cos_coeff^2 * Vss + sin_coeff^2 * Vcc -
                         2 * sin_coeff * cos_coeff * Vsc) / amplitude^2


  #Calculating the phase and translating it into the same units as the sampling_rate
  time_offset <- acrophase * period / (2*pi) # We translate the phase into time units

  time_offset_se <- acrophase_se * period / (2*pi)

  bin_dur <- duration(paste(sampling_bin_size, sampling_rate, sep = " "))
  phase <- as.numeric(bin_dur * time_offset, sampling_rate)
  phase_se <- as.numeric(bin_dur * time_offset_se, sampling_rate)

  # Model fit: adjusted R-squared and its significance, from a single glance() call.
  g <- glance(model)
  adj_r_squared <- g$adj.r.squared
  model_p.value <- g$p.value


  results <- list(
    period = period_hours,
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
    sin_coeff = sin_coeff,
    # Sine/cosine coefficient (co)variances and N, exposed so callers can draw the
    # Bingham (1982) joint 95% confidence ellipse for the acrophase/amplitude.
    var_sin = Vss,
    var_cos = Vcc,
    cov_sincos = Vsc,
    n_obs = nrow(df)
  )

  return(results)
}
