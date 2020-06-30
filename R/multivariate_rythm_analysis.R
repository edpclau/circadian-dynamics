#' Helper function to analyze the period of a time series
#' @description
#' Helper function which takes the output from multivariate_process_timeseries and runs: autocorrelation,
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
#' analysis <- multivariate_rythm_analysis(df = processed_data, sampling_rate = "30 min", autocorrelation = TRUE,
#' lomb_scargle = TRUE, from = 18, to = 30, multipeak_period = TRUE)
#'
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr select last_col
#'
#'
multivariate_rythm_analysis <- function(df = NULL, sampling_rate = NULL, autocorrelation = TRUE, lomb_scargle = TRUE,
         from = 18, to = 30, ofac = 60, datetime = NULL, window = NULL, values = NULL,
         alpha = 0.01) {

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
             rythm_analysis_by_window(df = df_short[[.]] , sampling_rate = sampling_rate,
                                      autocorrelation = autocorrelation, lomb_scargle = lomb_scargle,
                         from = from, to = to, ofac = ofac,
                         alpha = alpha)
)


if (!is.null(names(df))) {
  names(results) <- names(df)
}

return(results)

}
