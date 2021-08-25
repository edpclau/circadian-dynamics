#' Export plots and data outputed by the package
#'
#' @description Creates a folder in which all the data and plots resulting from the analysis will be saved.
#' @usage
#' export_all(processed_data = NULL, rythm_analysis_data = NULL, autocorrelation = TRUE, lomb_scargle = TRUE,
#' cosinor_fit = c("lomb_scargle", "autocorrelation"), dir_choose_gui = TRUE, new_dir_name = "analysis")
#'
#' @param raw_data data.frame or tibble containing the raw data.
#' @param processed_data data.frame returned from the process_timeseries or multivariate_process_timeseries functions.
#' @param rythm_analysis_data data.frame returned from the rythm_analysis_by_window and multivariate_rythm_analysis functions.
#' @param ld_data A data.frame/Tibble with 2 columns. Column 1 is a datetime object and column 2 is the light/dark indicator. (defult = NULL)
#' @param autocorrelation If TRUE (default) plots autocorrelation. FALSE does not plot autocorrelation.
#' @param lomb_scargle If TRUE (default) plots lomb scargle periodogram. FALSE does not plot periodogram.
#' @param cosinor_fit Method to fit the COSINOR to the data. Either "lomb_scargle" (default) or "autocorrelation".
#' @param dir_choose_gui If TRUE (default) a GUI will help select the folder in which to save the data and plots. If FALSE,
#' everything will be saved in the current directory.
#' @param new_dir_name Optional character argument to name the folder in which the data will be saved.
#'
#'
#'
#' @export
#'
#' @examples
#' export_all(processed_data = processed_data, rythm_analysis_data = analysis_data, new_dir_name = "test")
#'
#' @importFrom rstudioapi selectDirectory
export_all <- function(raw_data = NULL, processed_data = NULL, rythm_analysis_data = NULL, ld_data = NULL,
                       autocorrelation = TRUE, lomb_scargle = TRUE,
                       cosinor_fit = c("lomb_scargle", "autocorrelation"),
                       dir_choose_gui = TRUE, new_dir_name = "analysis",
                       path = getwd()) {

##### Flow Control ####)
if (is.null(processed_data)) { stop("Must provide the output from 'multivariate_process_timeseries'.")}
if (is.null(rythm_analysis_data)) { stop("Must provide the output from 'multivariate_rythm_analysis'.")}
if (is.null(raw_data)) { stop("Must provide the raw data.")}
if (length(processed_data) == 1 & is.null(names(processed_data))) {
  names(processed_data) <- names(processed_data[[1]])[3]
  names(rythm_analysis_data) <- names(processed_data[[1]])[3]
}

#Plan for paralellization
future::plan(future::multisession)

################################################################################################################
#######*******#### Temporary!!!! Drop problematic columns from rythm_analysis_data. In the future, we should
  #incorporate this columns into the pipeline

 rythm_analysis_data <- furrr::future_map(rythm_analysis_data,
             .f = ~ dplyr::select(., -c(from_acf, to_acf, acf_input_values,)))

###############################################################################################################


# You can choose between lomb_scargle or autocorrelation for "method". Period is the default.
cosinor_fit <- base::match.arg(cosinor_fit, choices = c("lomb_scargle", "autocorrelation"))

if (dir_choose_gui) {
  directory <- rstudioapi::selectDirectory()
  new_dir1 <- paste0(directory,"/", new_dir_name)
  dir.create(new_dir1)

} else {
  new_dir1 <- paste0(path,"/", new_dir_name)
  dir.create(new_dir1)
}

#Create a 'raw data'column for the mean
if (!rlang::is_empty(processed_data$mean) ) {
raw_data <- dplyr::left_join(raw_data, dplyr::distinct(processed_data$mean[2:3]), by = "datetime")
raw_data <- dplyr::select(raw_data,1, mean, dplyr::everything())
}

if (!is.null(raw_data)) {

#Function to export actogram
plot_actogram(raw_data, ld_data = ld_data, export = TRUE, autosize = FALSE, width =8, height=11, nrow = 2, ncol = 2, path = new_dir1)
}
print("Actogram Exported")

#Functions to save data
bind_processed(processed_data, export = TRUE, path = new_dir1)
print("bind_processed")
bind_analysis(rythm_analysis_data, export = TRUE, path = new_dir1)
print("bind_analysis")


#Plotting the Phase for all individuals in one PDF

plot_phase(path = new_dir1, analysis = rythm_analysis_data)

print("phase plotted for all inds in one pdf")

print("Making individual plots!")
# Saving Files!
for (name in names(processed_data)) {

  new_dir2 <- paste0(new_dir1,"/",name)
  filename <- paste0("plots_", name, ".pdf")
  dir.create(new_dir2)

  #Here I am changing directory to save in that dir. We need to change it so that it saves directly to this dir even
  # if it's not the working dir.
  # setwd(new_dir2)

  #First Function that we call for saving.

  export_plots(path = new_dir2, filename = filename, processed_data = processed_data[[name]], rythm_analysis_data = rythm_analysis_data[[name]],
               autocorrelation = autocorrelation, lomb_scargle = lomb_scargle,
               cosinor_fit = cosinor_fit,
               dir_choose_gui = FALSE)




  analysis_data_short <- list(rythm_analysis_data[[name]])
  names(analysis_data_short) <- name



  # Second function function that we call for saving
  plot_summarized_data(raw_data, analysis_data_short, path = new_dir2)



  #Third function that we call for saving

  export_data(path = new_dir2, processed_data = processed_data[[name]], rythm_analysis_data = rythm_analysis_data[[name]])

  print(paste(name, "export finished"))

}

# setwd(current_dir)
}
