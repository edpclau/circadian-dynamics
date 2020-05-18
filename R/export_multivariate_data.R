#' Export multivariate data
#'
#' @description Saves data outputted from the multivariate process_timeseries and multivariate_rythm_analysis functions.
#' @usage export_multivariate_data(processed_data = NULL, rythm_analysis_data = NULL, dir_choose_gui = TRUE, new_dir_name = "analysis")
#' @param processed_data optional data.frame returned from the process_timeseries function.
#' @param rythm_analysis_data optional data.frame returned from the rythm_analysis_by_window function.
#' @param dir_choose_gui If TRUE (default) a GUI will help select the folder in which to save the data and plots. If FALSE,
#' everything will be saved in the current directory.
#' @param new_dir_name Optional character argument to name the folder in which the data will be saved.
#'
#' @export
#'
#' @examples
#' export_multivariate_data(processed_data = processed_data, rythm_analysis_data = analysis_data,
#' dir_choose_gui = TRUE, new_dir_name = "Sensor_project")
export_multivariate_data <- function(processed_data = NULL, rythm_analysis_data = NULL, dir_choose_gui = TRUE,
                                     new_dir_name = "analysis") {


  ##### Flow Control ####)
if (is.null(processed_data)) { stop("Must provide the output from 'multivariate_process_timeseries'.")}
if (is.null(rythm_analysis_data)) { stop("Must provide the output from 'multivariate_rythm_analysis'.")}


if (dir_choose_gui) {
  directory <- rstudioapi::selectDirectory()
  new_dir1 <- paste0(directory,"/", new_dir_name)
  dir.create(new_dir1)
}

for (name in names(processed_data)) {

  new_dir2 <- paste0(new_dir1,"/",name)
  filename <- paste0("plots_", name, ".pdf")
  dir.create(new_dir2)
  setwd(new_dir2)


  export_data(processed_data = processed_data[[name]], rythm_analysis_data = rythm_analysis_data[[name]], dir_choose_gui = FALSE)

}

}
