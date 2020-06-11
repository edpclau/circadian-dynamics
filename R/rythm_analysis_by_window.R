#' Helper function to analyze the period of a time series
#' @description
#' Helper function which takes the output from process_timeseries and runs: autocorrelation,
#' lomb-scargle periodogram, and COSINOR analysis.
#' @usage multivariate_rythm_analysis(df = NULL, sampling_rate = NULL, auto_correlation = TRUE, lomb_scargle = TRUE,
#' from = NULL, to = NULL, ofac = 1, multipeak_period = TRUE, peak_of_interest = Inf,
#' datetime = NULL, window = NULL, values = NULL)
#' @param df The output from multivariate_process_timeseries
#' @param sampling_rate A character string indicating the sampling rate of the data. Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#' @param auto_correlation Logical. If TRUE (default) runs an autocorrelation on each window of the data. If FALSE, does not run.
#' @param lomb_scargle Logical. If TRUE (default) runs the Lomb-Scargle Periodogram on each window on the data. If FALSE, does not run.
#' @param from
#' An optional numeric indicating from which period or frequency to start looking for peaks.
#' Must be in the same units as the sampling rate.
#' Examples: If the goal is to evaluate a 18 "hour" period or frequency but the sampling rate is "30 minutes",
#' the period to use is a  36 "30 minutes" period or frequency.
#' @param to
#' An optional numeric indicating up to which period or frequency to start looking for peaks.
#' Must be in the same units as the sampling rate.
#' Examples: If the goal is to evaluate a 28 "hour" period or frequency but the sampling rate is "30 minutes",
#' the period to use is a  56 "30 minutes" period or frequency.
#' @param ofac
#' [lomb::lsp()] The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
#' @param multipeak_period TRUE (default) use all positive peaks to find the period.
#' FALSE use peak_of_interest to find period.
#'
#' @param peak_of_interest Positive peak on which we want to base the period calculation.
#' If not peak is supplied it will use the peak next to the middle peak.
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
#'

rythm_analysis_by_window <- function(df = NULL, sampling_rate = NULL, auto_correlation = TRUE, lomb_scargle = TRUE,
                                     from = NULL, to = NULL, ofac = 60, multipeak_period = TRUE, peak_of_interest = Inf,
                                     datetime = NULL, window = NULL, values = NULL) {
  ###### Flow control parameters######
  #1. Either a df or three lists with the datetimes, values, and windows must be supplied. If a df is not supplied, turn the
  # three lists into a df.
  if (is.null(df) & (is.null(datetime) | is.null(values) | is.null(window))) {
    stop("If a data.frame is not supplied. Must include windows, datetime, and values.")
  } else if (is.null(df)) { df = tibble::tibble(window, datetime, values) }
  #2.must have sampling rate and period
  if (is.null(sampling_rate)) {stop("Must include sampling_rate. ex. '30 minutes', '1 hour', '4 seconds', '100 days'.")}
  #3. Change the names of the df columns so we can work with it inside the function
  if (!is.null(df)) {
    original_names <- names(df)
    names(df) <- c("window", "datetime", "values")
  }





########### #Auto Correlation ######
if (auto_correlation) {
  acf_results <- acf_window(df = df %>% select(window, values),
                            multipeak_period = multipeak_period, peak_of_interest = peak_of_interest)
  acf_results <-  tidyr::drop_na(acf_results)
  # Fit Cosinor to autocorrelation
  cosinor_fits_auto_corr <- purrr::map2_df(.x = unique(acf_results$window), .y = acf_results$period,
                                           .f = ~ cosinor_lm(dplyr::filter(df, window == .x) %>%
                                                               dplyr::select(datetime, values),
                                                             sampling_rate = sampling_rate,
                                                             period = .y,
                                                             na.action = na.pass))

}


######## Lomb-Scargle Periodogram by windows #######
if (lomb_scargle) {
  lsp_results <- lsp_by_window(df %>% dplyr::select(window, datetime, values),
                               from = from, to = to, sampling_rate = sampling_rate, ofac = ofac)
 lsp_results <- mutate(lsp_results,
                       period = ifelse(is.na(period), 24, period))
  # Fit Cosinor to lsp
  cosinor_fits_lsp <- purrr::map2_df(.x = unique(lsp_results$window), .y = lsp_results$period,
                                     .f = ~ cosinor_lm(dplyr::filter(df, window == .x) %>% dplyr::select(datetime, values),
                                                       sampling_rate = sampling_rate,
                                                       period = .y,
                                                       na.action = na.pass))

}

###### Prepare Results #######

#1. auto and lomb results
if (auto_correlation & lomb_scargle) {
  lsp <- dplyr::bind_cols(lsp_results,cosinor_fits_lsp)
  auto <- dplyr::bind_cols(acf_results,cosinor_fits_auto_corr)
  results <-  dplyr::bind_rows(lomb_scargle = lsp, autocorrelation = auto, .id = "method")

  #2. Only auto
} else if (auto_correlation == TRUE & lomb_scargle == FALSE) {
  results <- dplyr::bind_cols(acf_results,cosinor_fits_auto_corr)
  results$method <- "autocorrelation"
  results <- dplyr::select(results, method, dplyr::everything())
  #3. Only lomb
} else {
  results <- dplyr::bind_cols(lsp_results,cosinor_fits_lsp)
  results$method <- "lomb_scargle"
  results <- dplyr::select(results, method, dplyr::everything())
}

return(results)
}
