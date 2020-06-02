#' Helper function to analyze the period of a time series
#' @description
#' Helper function which takes the output from multivariate_process_timeseries and runs: autocorrelation,
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
#' analysis <- multivariate_rythm_analysis(df = processed_data, sampling_rate = "30 min", auto_correlation = TRUE,
#' lomb_scargle = TRUE, from = 36, to = 48, multipeak_period = TRUE)
#'
#'
multivariate_rythm_analysis <- function(df = NULL, sampling_rate = NULL, auto_correlation = TRUE, lomb_scargle = TRUE,
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






df_short <- purrr::map(1:length(df),
           .f = ~ dplyr::select(df[[.]], 1:2, dplyr::last_col())
)




results <- purrr::map(1:length(df_short),
           .f = ~
             rythm_analysis_by_window(df = df_short[[.]] , sampling_rate = sampling_rate, auto_correlation = auto_correlation, lomb_scargle = lomb_scargle,
                         from = from, to = to, ofac = ofac, multipeak_period = multipeak_period, peak_of_interest = peak_of_interest)
)


if (!is.null(names(df))) {
  names(results) <- names(df)
}

return(results)

}
