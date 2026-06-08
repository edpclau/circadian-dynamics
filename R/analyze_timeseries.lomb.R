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
#' \code{lomb::lsp()} The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
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
#' @export analyze_lomb
#'
#' @examples
#' \dontrun{
#' res <- analyze_lomb(df, sampling_rate = "1 hour", from = 18, to = 30)
#' }
#'
#'
#' @importFrom lubridate  duration
#' @importFrom stringr str_remove str_extract
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
  sampling_bin_size = as.numeric(str_extract(sampling_rate, "\\d*"))
  sampling_rate = str_remove(sampling_rate, "\\d* *")

  #4. Extract values
  values = df$value

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
                    scanned = NA, power = NA, rhythm_strength = NA)

  # Skip flat windows or windows with too few non-NA values.
  if (var(values) == 0 | sum(!is.na(values)) <= 2) return(na_result)

  #### Lomb-Scargle periodogram ####
  lsp_of_int <- lsp_mod(x = values, ofac = ofac, type = type, alpha = alpha, plot = FALSE)
  if (is_empty(lsp_of_int)) return(na_result)

  ## Peak of interest within the [from, to] band ##
  peaks <- lsp_peaks(lsp_of_int)
  position <- from <= peaks$time & to >= peaks$time
  if (any(position)) {
    peak <- peaks$peaks[position][1]
    period <- peaks$time[position][1]
    # Rhythm strength = peak power relative to the significance threshold. Anchored
    # to alpha, so it is comparable across windows regardless of periodogram grid size.
    rhythm_strength <- peak / lsp_of_int$sig.level
  } else {
    # Periodogram computed but no peak in band: period/peak undefined, but the
    # periodogram values (p_value, sig_level, scanned, power) are still meaningful.
    peak <- NA
    period <- NA
    rhythm_strength <- NA
  }

  list(
    period = as.numeric(duration(period * sampling_bin_size, sampling_rate), "hours"),
    peak = peak,
    p_value = lsp_of_int$p.value,
    sig_level = lsp_of_int$sig.level,
    scanned = as.numeric(duration(lsp_of_int$scanned * sampling_bin_size, sampling_rate), "hours"),
    power = lsp_of_int$power,
    rhythm_strength = rhythm_strength
  )
}
