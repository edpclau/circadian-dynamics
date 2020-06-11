#' Export data
#'
#' @description Saves data outputted from the process_timeseries and rythm_analysis_by_window functions.
#'
#' @usage export_data(processed_data = NULL, rythm_analysis_data = NULL, dir_choose_gui = TRUE)
#'
#' @param processed_data optional data.frame returned from the process_timeseries function.
#' @param rythm_analysis_data optional data.frame returned from the rythm_analysis_by_window function.
#' @param dir_choose_gui If TRUE (default) a GUI will help select the folder in which to save the data and plots. If FALSE,
#' everything will be saved in the current directory.
#'
#' @export
#'
#' @examples
#' export_data(processed_data = processed_data, rythm_analysis_data = analysis_data, dir_choose_gui = TRUE)
export_data <- function(processed_data = NULL, rythm_analysis_data = NULL, dir_choose_gui = TRUE) {


if (dir_choose_gui) {
    directory <- rstudioapi::selectDirectory()
    new_dir1 <- paste0(directory,"/analysis")
    dir.create(new_dir1)
  }

if (!is.null(processed_data))  {
readr::write_csv(processed_data, "processed_data.csv", col_names = TRUE)
}

if (!is.null(rythm_analysis_data)) {
dplyr::select(rythm_analysis_data,
              c(method, window, period, power, lsp_p_value, sig_level, ofac, MESOR,
                amplitude, amplitude_se, adj_r_squared, cosinor_p_value, phase_in_seconds)) %>%
  dplyr::mutate(phase_in_hours = phase_in_seconds) %>%
  readr::write_csv("rythm_analysis.csv", col_names = TRUE)
}

}
