#' Simplify pipeline output into tidy data frames
#'
#' @description Arranges the nested output of [process_timeseries.main] into four
#'   tidy tibbles (`data`, `autocorrelation`, `lombscargle`, `utils`) suitable for
#'   export and plotting. Works for both windowed and non-windowed output; the
#'   windowed case adds a `window` column.
#' @param df Output from [process_timeseries.main].
#' @param big_data Logical; if TRUE use a multisession plan for large datasets. Default FALSE.
#' @return A named list of four tibbles: `data`, `autocorrelation`, `lombscargle`, `utils`.
#' @export
#' @importFrom future multisession plan
#' @importFrom furrr future_map_dfr
#' @importFrom rlang is_empty
simplify_data <- function(df, big_data = FALSE) {
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  if (big_data) future::plan(future::multisession)

  windowed <- any(class(df[[1]][[1]]) == "list")

  # Flatten one analysis unit (an individual, or an individual's window) with a
  # builder, keyed by individual (and window, when windowed).
  collect <- function(builder) {
    if (windowed) {
      future_map_dfr(df, function(ind) future_map_dfr(ind, builder, .id = "window"), .id = "data")
    } else {
      future_map_dfr(df, builder, .id = "data")
    }
  }

  list(
    data = collect(.unit_data),
    autocorrelation = collect(.unit_acf),
    lombscargle = collect(.unit_lomb),
    utils = collect(.unit_utils)
  )
}

# --- per-unit builders (one definition each; shared by both branches) ---

.unit_data <- function(u) {
  data <- u$data
  data$lomb_cosinor <- u$lomb$cosinor$wave
  data$autocorr_cosinor <- u$acf$cosinor$wave
  names(data)[names(data) == "value"] <- "raw_values"
  data
}

.unit_acf <- function(u) {
  tibble::tibble(
    peak_datetime = u$acf$results$datetime,
    period = u$acf$results$period,
    rhythm_strength = u$acf$results$rhythm_strength,
    acf_peak = u$acf$results$max_peak_of_int,
    mesor = u$acf$cosinor$mesor,
    amplitude = u$acf$cosinor$amplitude,
    amp_se = u$acf$cosinor$amplitude_se,
    acrophase = u$acf$cosinor$acrophase,
    acro_se = u$acf$cosinor$acrophase_se,
    phase = u$acf$cosinor$phase,
    phase_se = u$acf$cosinor$phase_se,
    adj_r_squared = u$acf$cosinor$adj_r_squared,
    cosinor_p_value = u$acf$cosinor$p_value
  )
}

.unit_lomb <- function(u) {
  tibble::tibble(
    peak_datetime = u$lomb$results$datetime,
    period = if (is_empty(u$lomb$results$period)) NA else u$lomb$results$period,
    rhythm_strength = if (is_empty(u$lomb$results$rhythm_strength)) NA else u$lomb$results$rhythm_strength,
    lsp_peak = u$lomb$results$peak,
    mesor = u$lomb$cosinor$mesor,
    amplitude = u$lomb$cosinor$amplitude,
    amp_se = u$lomb$cosinor$amplitude_se,
    acrophase = u$lomb$cosinor$acrophase,
    acro_se = u$lomb$cosinor$acrophase_se,
    phase = u$lomb$cosinor$phase,
    phase_se = u$lomb$cosinor$phase_se,
    adj_r_squared = u$lomb$cosinor$adj_r_squared,
    cosinor_p_value = u$lomb$cosinor$p_value
  )
}

.unit_utils <- function(u) {
  tibble::tibble(
    datetime = u$data$datetime,
    acf = u$acf$results$autocorrelation,
    acf_period = u$acf$results$period,
    acf_rs = u$acf$results$rhythm_strength,
    acf_peak = u$acf$results$max_peak_of_int,
    acf_peak_time = u$acf$results$datetime,
    lsp_period = if (is_empty(u$lomb$results$period)) NA else u$lomb$results$period,
    lsp_peak = u$lomb$results$peak,
    lsp_sig_level = u$lomb$results$sig_level,
    lsp_p_value = u$lomb$results$p_value,
    lsp_scanned = list(u$lomb$results$scanned),
    lsp_power = list(u$lomb$results$power),
    lsp_rs = if (is_empty(u$lomb$results$rhythm_strength)) NA else u$lomb$results$rhythm_strength,
    acf_start = u$acf$results$start,
    acf_end = u$acf$results$end,
    acf_from = u$acf$results$from,
    acf_to = u$acf$results$to,
    lsp_phase = u$lomb$cosinor$phase,
    acf_phase = u$acf$cosinor$phase,
    acf_amp = u$acf$cosinor$amplitude,
    acf_pr = u$acf$cosinor$adj_r_squared,
    lsp_amp = u$lomb$cosinor$amplitude,
    lsp_pr = u$lomb$cosinor$adj_r_squared
  )
}
