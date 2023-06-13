#' Generate Detailed Plots (Utility function)
#'
#' @param trikinetics_analyzed
#' @param sampling_rate
#' @param windows
#'
#' @return
#' @export
#'
#' @examples
detailed_plots <- function(trikinetics_analyzed, sampling_rate = 'minutes', windows = TRUE) {
  if (windows) {
    generate_plots_with_windows(trikinetics_analyzed, sampling_rate)
  } else {
    generate_plots_no_windows(trikinetics_analyzed, sampling_rate)
  }
}
