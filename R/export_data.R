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
#' @importFrom rstudioapi selectDirectory
#' @importFrom readr write_csv
#' @importFrom dplyr select mutate
#' @importFrom magrittr '%>%'
export_data <- function(path = getwd(), processed_data = NULL, rythm_analysis_data = NULL) {


##### Actually saving files ####
if (!is.null(processed_data))  {
  #First time we write a csv
readr::write_csv(processed_data, paste0(path, "/","processed_data.csv"), col_names = TRUE)
}

if (!is.null(rythm_analysis_data)) {
df_select <-  dplyr::select(rythm_analysis_data,
              c(method, window, period, period_hours, power, lsp_p_value, sig_level, ofac, MESOR,
                amplitude, amplitude_se, adj_r_squared, cosinor_p_value, phase_in_seconds)) %>%
  dplyr::rename(phase_in_hours = phase_in_seconds) %>%
  dplyr::rename(PR = adj_r_squared)

df_autocor <- df_select %>% dplyr::filter(method == "autocorrelation") %>%
  dplyr::select(-c(power, lsp_p_value, sig_level, ofac, method))

df_lsp <- df_select %>% dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(-c(period_hours, method))

#Second time we write a csv
write.csv(df_lsp, paste0(path, "/", "rythm_analysis_lsp.csv"), col.names = TRUE, row.names = FALSE)
#Third time we write a csv
write.csv(df_autocor, paste0(path, "/", "rythm_analysis_autocor.csv"), col.names = TRUE, row.names = FALSE)
}

}
