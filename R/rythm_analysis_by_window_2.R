#' Helper function to analyze the period of a time series
#' @description
#' Helper function which takes the output from process_timeseries and runs: autocorrelation,
#' lomb-scargle periodogram, and COSINOR analysis.
#' @usage multivariate_rythm_analysis(df = NULL, sampling_rate = NULL, auto_correlation = TRUE, lomb_scargle = TRUE,
#' from = NULL, to = NULL, ofac = 1, multipeak_period = TRUE, peak_of_interest = Inf,
#' datetime = NULL, window = NULL, values = NULL)
#' @param df The output from multivariate_process_timeseries
#' @param sampling_rate A character string indicating the sampling rate of the data. Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#' @param autocorrelation Logical. If TRUE (default) runs an autocorrelation on each window of the data. If FALSE, does not run.
#' @param lomb_scargle Logical. If TRUE (default) runs the Lomb-Scargle Periodogram on each window on the data. If FALSE, does not run.
#' @param from
#' An optional numeric indicating from which period or frequency to start looking for peaks.
#' Must be in hours. Default = 18.
#'
#' @param to
#' An optional numeric indicating up to which period or frequency to start looking for peaks.
#' Must be in hours. Default = 30.
#'
#' @param ofac
#' [lomb::lsp()] The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
#'
#' @param datetime optional if a data.frame is supplied. A list of multiple POSIXct vectors corresponding to each mearument variable.
#' @param window optional if a data.frame is supplied. A list of multiple window vectors corresponding to each mearument variable.
#' @param values optional if a data.frame is supplied. A list of multiple value vectors corresponding to each mearument variable.
#'
#' @return
#' A names list of data.frames with the combined output of [acf_by_window()], [lsp_by_window()], and [cosinor_lm()].
#' @export
#'
#' @examples
#' analysis <- rythm_analysis(df = processed_data, sampling_rate = "30 min", auto_correlation = TRUE,
#' lomb_scargle = TRUE, from = 36, to = 48, multipeak_period = TRUE)
#'
#' @importFrom rlang is_empty
#' @importFrom dplyr rename
#' @importFrom tidyr drop_na
#' @importFrom furrr future_map_if furrr_options
#' @import magrittr
#' @importFrom future plan multisession sequential
#' @importFrom stringr str_remove str_extract
#' @importFrom lubridate duration
#'
#'

rythm_analysis_by_window_2 <- function(df = NULL,
                                       sampling_rate = NULL,
                                       from = 18,
                                       to = 30,
                                       ofac = 60,
                                       alpha = 0.01,
                                       big_data = FALSE) {
  ###### Flow control parameters######
  #1. Must have sampling rate and period
  if (is.null(sampling_rate)) {
    stop("Must include sampling_rate. ex. '30 minutes', '1 hour', '4 seconds', '100 days'.")
  }
  sampling_bin_size = as.numeric(str_extract(sampling_rate, "\\d*"))
  sampling_unit = str_remove(sampling_rate, "\\d* *")

  #2. In case we have windows without a period
  placeholder_period = as.numeric(duration(24, 'hour'), sampling_unit) /
    sampling_bin_size

  #2. Plan for paralellization
  if (big_data) {
  plan(multisession)
    } else {plan(sequential)}

  ########### #Auto Correlation ######
  df = acf_window_2(
    df = df ,
    from = from,
    to = to,
    sampling_rate = sampling_rate,
    big_data = big_data
  )
  # Fit Cosinor to autocorrelation
  df = {future_map_if(
      # .options = furrr_options(lazy = TRUE),
      .x = df,
      .p = ~ !is_empty(drop_na(.x)$auto_period),
      #if there is no period, skip
      .f = ~ cosinor_lm_2(
        df = .x,
        period = drop_na(.x)$auto_period,
        sampling_rate = sampling_rate,
        na.action = na.pass
      ) %>% rename(
        auto_mesor = mesor,
        auto_amplitude = amplitude,
        auto_amplitude_se = amplitude_se,
        auto_acrophase = acrophase,
        auto_acrophase_se = acrophase_se,
        auto_phase = phase,
        auto_phase_se = phase_se,
        auto_adj_r_squared = adj_r_squared,
        auto_cosinor_p_value = cosinor_p_value,
        auto_cosinor_wave = cosinor_wave,
        auto_cos_coeff = cos_coeff,
        auto_sin_coeff = sin_coeff
      ),
      .else = ~ cosinor_lm_2(
        df = .x,
        period = placeholder_period,
        sampling_rate = sampling_rate,
        na.action = na.pass
      ) %>% rename(
        auto_mesor = mesor,
        auto_amplitude = amplitude,
        auto_amplitude_se = amplitude_se,
        auto_acrophase = acrophase,
        auto_acrophase_se = acrophase_se,
        auto_phase = phase,
        auto_phase_se = phase_se,
        auto_adj_r_squared = adj_r_squared,
        auto_cosinor_p_value = cosinor_p_value,
        auto_cosinor_wave = cosinor_wave,
        auto_cos_coeff = cos_coeff,
        auto_sin_coeff = sin_coeff
      )

    )
  }

  ######## Lomb-Scargle Periodogram by windows #######{
  df = lsp_by_window_2(
    df,
    from = from,
    to = to,
    sampling_rate = sampling_rate,
    ofac = ofac,
    alpha = alpha,
    big_data = big_data
  )
  #Fit Cosinor LSP
  df = {
    future_map_if(
      # .options = furrr_options(lazy = TRUE),
      .x = df,
      .p = ~ !is_empty(drop_na(.x)$lsp_period),
      #if there is no period, skip
      .f = ~ cosinor_lm_2(
        df = .x,
        period = drop_na(.x)$lsp_period,
        sampling_rate = sampling_rate,
        na.action = na.pass
      ) %>% rename(
        lsp_mesor = mesor,
        lsp_amplitude = amplitude,
        lsp_amplitude_se = amplitude_se,
        lsp_acrophase = acrophase,
        lsp_acrophase_se = acrophase_se,
        lsp_phase = phase,
        lsp_phase_se = phase_se,
        lsp_adj_r_squared = adj_r_squared,
        lsp_cosinor_p_value = cosinor_p_value,
        lsp_cosinor_wave = cosinor_wave,
        lsp_cos_coeff = cos_coeff,
        lsp_sin_coeff = sin_coeff
      ),
      .else = ~ cosinor_lm_2(
        df = .x,
        period = placeholder_period,
        sampling_rate = sampling_rate,
        na.action = na.pass
      ) %>% rename(
        lsp_mesor = mesor,
        lsp_amplitude = amplitude,
        lsp_amplitude_se = amplitude_se,
        lsp_acrophase = acrophase,
        lsp_acrophase_se = acrophase_se,
        lsp_phase = phase,
        lsp_phase_se = phase_se,
        lsp_adj_r_squared = adj_r_squared,
        lsp_cosinor_p_value = cosinor_p_value,
        lsp_cosinor_wave = cosinor_wave,
        lsp_cos_coeff = cos_coeff,
        lsp_sin_coeff = sin_coeff
      )

    )
  }

  return(df)
}
