#' Lomb Scargle Periodogram by window
#'
#' @description Iteratively computes the Lomb-Scargle periodogram for a time series with irregular (or regular) sampling intervals.
#'
#' @param df
#' optional data.frame with 2 or 3 columns. If the data.frame has 2 columns, column 1 contains the windows, column 2
#' contains the measurement values. If the data.frame has 3 columns, column 1 contains windows, column 2 is POSIXct object,
#' and column 3 is a measurement values.
#'
#' @param sampling_rate
#' A character string indicating the sampling rate of the data.
#' Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @param from
#' An optional numeric indicating from which period or frequency to start looking for peaks.
#' Must be in hours. Default = 18.
#'
#' @param to
#' An optional numeric indicating up to which period or frequency to start looking for peaks.
#' Must be in hours. Default = 30.
#'
#' @param ofac
#' The Lomb-Scargle oversampling factor. Must be an integer >= 1. Larger values give
#' finer frequency scanning but cost more time on large datasets or wide (from...to)
#' ranges. Default = 1. Typically 1-10; values above 20 are capped to 20 with a warning.
#'
#' @param alpha
#' The significance level used by the Lomb-Scargle periodogram. Powers exceeding the
#' corresponding significance threshold are considered significant. Default = 0.01.
#'
#' @return
#' A data.frame with the following components:
#'
#' window           A vector containing the window to which the analysis corresponds.
#'
#' period           The period of the timeseries. Outputted in the same sampling rate as the data.
#'
#' power            The maximum power in the frequency/period interval inspected.
#'
#' lsp_p_value      The probability that the maximum peak occurred by chance.
#'
#' scanned          A vector containing the frequencies/period scanned.
#'
#' normalized_power A vector containing the normalized power corresponding to scanned frequencies/periods.
#'
#' sig_level        Powers > sig.level can considered significant peaks.
#'
#' relative_power   The fraction of total Lomb-Scargle spectral power carried by the
#'                  dominant in-band peak, i.e. \code{peak / sum(power)}. Bounded in
#'                  (0, 1]; a normalised, length-robust index of how concentrated the
#'                  series' power is at the detected period. Higher values indicate a
#'                  more dominant, robust rhythm. This replaces the former
#'                  \code{rhythm_strength} (peak / significance threshold), which was a
#'                  significance ratio rather than a measure of rhythm strength and
#'                  grew with recording length.
#'
#' @references
#' Ruf, T. (1999) The Lomb-Scargle Periodogram in Biological Rhythm Research: Analysis
#' of Incomplete and Unequally Spaced Time-Series. Biological Rhythm Research 30(2):
#' 178-201. \doi{10.1076/brhm.30.2.178.1422}
#'
#' Refinetti, R., Cornelissen, G. & Halberg, F. (2007) Procedures for numerical
#' analysis of circadian rhythms. Biological Rhythm Research 38(4): 275-325.
#' \doi{10.1080/09291010600903692}
#'
#' @export analyze_lomb
#'
#' @examples
#' \dontrun{
#' res <- analyze_lomb(df, sampling_rate = "1 hour", from = 18, to = 30)
#' }
#'
#'
#' @importFrom lubridate  duration
#'
analyze_lomb<- function (df = NULL, sampling_rate = NULL, from = 18, to = 30,
                            ofac = 1, alpha = 0.01) {

  ###### Flow control parameters######
  #1.must have sampling_rate
  if (is.null(sampling_rate)) {
    stop("Must include sampling_rate. ex. '30 min', '1 hour', '4 seconds', '100 days'.")
  }

  #2. You can choose between period or frequency for "Type". Period is the default.
  type <- 'period'

  #ofac is an integer oversampling factor (typically 1-10), NOT the sampling rate.
  if (ofac > 20) {
    warning("ofac = ", ofac, " is implausibly large for an oversampling factor; ",
            "capping at 20. ofac is the Lomb oversampling factor, not the sampling rate.")
    ofac <- 20
  }

  #3. Sampling_rate
  sr <- .parse_sampling_rate(sampling_rate)
  sampling_bin_size <- sr$bin
  sampling_rate <- sr$unit

  #4. Extract values. Use the LAST column, i.e. the most-processed signal
  #   (detrended / smoothed / butterworth) appended by the waveform step, to match
  #   analyze_acf() and analyze_cosinor(). Reading df$value here would silently run
  #   the periodogram on the raw, unfiltered counts and ignore the requested
  #   detrending/filtering.
  values = df[[ncol(df)]]

  #4. Convert from-to to sampling rate.
  if (!is.null(from)) {
    from <- (as.numeric(duration(from, units = "hours"), sampling_rate)/ sampling_bin_size)
  }

  if (!is.null(to)) {
    to  <- (as.numeric(duration(to, units = "hours"), sampling_rate) / sampling_bin_size)
  }

  # Empty result returned when no periodogram can be computed (flat/too-short window
  # or a failed Lomb-Scargle fit). Same key set as the success path.
  na_result <- list(period = NA, peak = NA, p_value = NA, sig_level = NA,
                    scanned = NA, power = NA, relative_power = NA)

  # Skip flat windows or windows with too few non-NA values. var(values) is NA
  # (not 0) when values contains NAs, so guard with isTRUE() and na.rm to avoid
  # an `if(NA)` error that would crash the furrr worker.
  if (isTRUE(var(values, na.rm = TRUE) == 0) || sum(!is.na(values)) <= 2) return(na_result)

  #### Lomb-Scargle periodogram ####
  lsp_of_int <- lsp_mod(x = values, ofac = ofac, type = type, alpha = alpha, plot = FALSE)
  if (is_empty(lsp_of_int)) return(na_result)

  ## Peak of interest within the [from, to] band ##
  peaks <- lsp_peaks(lsp_of_int)
  position <- from <= peaks$time & to >= peaks$time
  # Significance is calibrated to the SEARCH BAND, not the full Nyquist grid.
  # lsp_mod()'s own $sig.level/$p.value describe the GLOBAL periodogram maximum,
  # which is frequently a different (out-of-band) peak than the one reported here
  # (e.g. a 12 h harmonic when searching 18-30 h). Reporting the global p-value
  # next to an in-band period is misleading, and a full-grid significance
  # threshold over-penalises and deflates rhythm_strength. We therefore mirror
  # lsp_mod()'s formulas (Horne & Baliunas effective number of independent
  # frequencies) but count only the frequencies scanned inside [from, to].
  n_band <- sum(lsp_of_int$scanned >= from & lsp_of_int$scanned <= to)
  effm <- 2 * n_band / ofac
  if (any(position)) {
    peak <- peaks$peaks[position][1]
    period <- peaks$time[position][1]
    sig_level <- -log(1 - (1 - alpha)^(1 / effm))
    exPN <- exp(-peak)
    p_value <- effm * exPN
    if (p_value > 0.01) p_value <- 1 - (1 - exPN)^effm
    # Relative spectral power: the fraction of total Lomb-Scargle spectral power
    # carried by the dominant in-band peak. Bounded in (0, 1]; higher means the
    # series' power is more concentrated at the rhythmic period (a more robust
    # rhythm). See Ruf (1999); Refinetti, Cornelissen & Halberg (2007).
    relative_power <- peak / sum(lsp_of_int$power, na.rm = TRUE)
  } else {
    # Periodogram computed but no peak inside [from, to]: no rhythm detected in the
    # search band. Report NA rather than leaking the GLOBAL out-of-band maximum's
    # p-value, which would otherwise flag a dead/arrhythmic channel as highly
    # significant while period is NA. sig_level is kept band-calibrated so the
    # returned periodogram's threshold line stays consistent with the success path.
    peak <- NA
    period <- NA
    p_value <- NA
    relative_power <- NA
    sig_level <- if (n_band > 0) -log(1 - (1 - alpha)^(1 / effm)) else lsp_of_int$sig.level
  }

  list(
    period = as.numeric(duration(period * sampling_bin_size, sampling_rate), "hours"),
    peak = peak,
    p_value = p_value,
    sig_level = sig_level,
    scanned = as.numeric(duration(lsp_of_int$scanned * sampling_bin_size, sampling_rate), "hours"),
    power = lsp_of_int$power,
    relative_power = relative_power
  )
}
