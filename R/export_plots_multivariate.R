#' Export plots
#'
#' @description Creates a folder in which all plots resulting from the analysis will be saved.
#' @usage
#' export_plots_multivariate(processed_data = NULL, rythm_analysis_data = NULL,
#' autocorrelation = TRUE, lomb_scargle = TRUE, cosinor_fit = c("lomb_scargle", "autocorrelation"),
#' dir_choose_gui = TRUE, new_dir_name = "analysis")
#' @param processed_data data.frame returned from the process_timeseries or multivariate_process_timeseries functions.
#' @param rythm_analysis_data data.frame returned from the rythm_analysis_by_window and multivariate_rythm_analysis functions.
#' @param autocorrelation If TRUE (default) plots autocorrelation. FALSE does not plot autocorrelation.
#' @param lomb_scargle If TRUE (default) plots lomb scargle periodogram. FALSE does not plot periodogram.
#' @param cosinor_fit Method to fit the COSINOR to the data. Either "lomb_scargle" (default) or "autocorrelation".
#' @param dir_choose_gui If TRUE (default) a GUI will help select the folder in which to save the data and plots. If FALSE,
#' everything will be saved in the current directory.
#' @param new_dir_name Optional character argument to name the folder in which the data will be saved.
#'
#' @export
#'
#' @examples
#' export_plots_multivariate(processed_data = processed_data, rythm_analysis_data = analysis_data,
#' new_dir_name = "sensor_project")
#'
#' @importFrom rstudioapi selectDirectory
export_plots_multivariate <- function(processed_data = NULL, rythm_analysis_data = NULL,
                                      autocorrelation = TRUE, lomb_scargle = TRUE,
                                      cosinor_fit = c("lomb_scargle", "autocorrelation"),
                                      dir_choose_gui = TRUE, new_dir_name = "analysis") {

##### Flow Control ####)
if (is.null(processed_data)) { stop("Must provide the output from 'multivariate_process_timeseries'.")}
if (is.null(rythm_analysis_data)) { stop("Must provide the output from 'multivariate_rythm_analysis'.")}

# You can choose between lomb_scargle or autocorrelation for "method". Period is the default.
cosinor_fit <- base::match.arg(cosinor_fit, choices = c("lomb_scargle", "autocorrelation"))

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

export_plots(filename = filename, processed_data = processed_data[[name]], rythm_analysis_data = rythm_analysis_data[[name]],
           autocorrelation = TRUE, lomb_scargle = TRUE,
           cosinor_fit = cosinor_fit,
           dir_choose_gui = FALSE)

}

}
