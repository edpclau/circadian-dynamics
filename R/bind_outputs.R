#' Bind Rows of Processed and Analyzed data
#'
#' @description Allows for easy export and binding of rows from the outputs of process_data and rythm_analysis.
#' @usage
#' bind_processed(df = NULL, export = FALSE, path = paste0(getwd(),"/",substitute(df),".csv"))
#' bind_analysis(df = NULL, export = FALSE, path = paste0(getwd(),"/",substitute(df),".csv"))
#'
#' @param df the output from either process_data or rythm_analysis
#' @param export If TRUE, will save the data.frame as a .csv in the current directory. If FALSE (default),
#' will return the bound data.frame.
#' @param path Where to save the file if export = TRUE. Defaults to current directory.
#'
#' @export
#'
#' @examples
#' bind_processed(df = monitor_processed)
#' bind_analysis(df = monitor_analysis)
#'
 bind_processed <- function(df = NULL, export = FALSE, path = getwd()) {

  df_bound <-  purrr::map_df(df, ~ dplyr::rename(., raw = 3), .id = "ID")
  filename = paste0(path,"/",substitute(df),".csv")

  if (export) {readr::write_csv(df_bound, filename)
    } else {return(df_bound) }


 }


 bind_analysis <- function(df = NULL, export = FALSE, path = getwd()) {

   df_bound <-  dplyr::bind_rows(df, .id = "ID")
   filename = paste0(path,"/",substitute(df),".csv")


   if (export) {
     df_bound <- dplyr::select(df_bound, -c(scanned, normalized_power, wave_y, wave_x))
     df_bound <- dplyr::rename(df_bound, phase_hours = phase_in_seconds, phase_hours_se = phase_se_seconds)
     df_bound <- dplyr::arrange(df_bound, ID)
     readr::write_csv(df_bound, filename)
     } else {return(df_bound)}


 }
