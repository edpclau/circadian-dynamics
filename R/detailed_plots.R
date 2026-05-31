#' Generate Detailed Plots (Utility function)
#'
#' @param trikinetics_analyzed The output from the rhythm analysis pipeline.
#' @param sampling_rate A character string for the sampling rate (e.g., \code{"minutes"}).
#' @param windows Logical. If \code{TRUE} (default), calls \code{generate_plots_with_windows}; otherwise calls \code{generate_plots_no_windows}.
#'
#' @return Invisibly saves PDF plot files to the working directory.
#' @export
detailed_plots <- function(trikinetics_analyzed, sampling_rate = 'minutes', windows = TRUE) {
  if (windows) {
    generate_plots_with_windows(trikinetics_analyzed, sampling_rate)
  } else {
    generate_plots_no_windows(trikinetics_analyzed, sampling_rate)
  }
}
