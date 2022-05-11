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
#' @importFrom tidyr unnest
export_data <- function(path = getwd(), processed_data = NULL, rythm_analysis_data = NULL) {


##### Actually saving files ####
if (!is.null(processed_data))  {
  #First time we write a csv
  write.csv(processed_data, paste0(path, "/","processed_data.csv"),  row.names = FALSE)
}


if (!is.null(rythm_analysis_data)) {
df_select <-  dplyr::select(rythm_analysis_data,
              c(method, window, period, period_hours, power, lsp_p_value, sig_level, ofac, MESOR,
                amplitude, amplitude_se, adj_r_squared, cosinor_p_value, phase_in_seconds,
                wave_x, wave_y)) %>%
  dplyr::rename(phase_in_hours = phase_in_seconds) %>%
  dplyr::rename(PR = adj_r_squared)

df_autocor <- df_select %>% dplyr::filter(method == "autocorrelation") %>%
  dplyr::select(-c(power, lsp_p_value, sig_level, ofac, method, wave_x, wave_y))

df_lsp <- df_select %>% dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(-c(period_hours, method, wave_x, wave_y))


#Second time we write a csv
write.csv(df_lsp, paste0(path, "/", "rythm_analysis_lsp.csv"), row.names = FALSE)
#Third time we write a csv
write.csv(df_autocor, paste0(path, "/", "rythm_analysis_autocor.csv"), row.names = FALSE)


##### Export Cosinor Values ######
df_autocor_wave <- df_select %>% dplyr::filter(method == "autocorrelation") %>%
  dplyr::select(window, wave_x, wave_y) %>% unnest(cols = c(wave_x, wave_y))

df_lsp_wave <- df_select %>% dplyr::filter(method == "lomb_scargle") %>%
  dplyr::select(window, wave_x, wave_y) %>% unnest(cols = c(wave_x, wave_y))

#Fourth time we write a csv
write.csv(df_lsp_wave, paste0(path, "/", "lsp_cosinor.csv"), row.names = FALSE)
#Fifth time we write a csv
write.csv(df_autocor_wave, paste0(path, "/", "autocor_cosinor.csv"), row.names = FALSE)

}

}
