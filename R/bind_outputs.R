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
   #Plan for paralellization
   future::plan(future::multisession)

  df_bound <-  purrr::map_df(df, ~ dplyr::rename(., raw = 3), .id = "ID")
  filename = paste0(path,"/",substitute(df),".csv")

  if (export) {readr::write_csv(df_bound, filename)
    } else {return(df_bound) }


 }



 bind_analysis <- function(df = NULL, export = FALSE, path = getwd()) {

   df_lomb <-  dplyr::bind_rows(df, .id = "ID") %>%
     dplyr::filter(method == 'lomb_scargle') %>%
     dplyr::select_if(~!(all(is.na(.)) | all(is.list(.)))) %>%
     select(-window_starts, -window_ends, -method, -from, -to) %>%
     tidyr::pivot_longer(-c(ID,window)) %>%
     tidyr::pivot_wider(id_cols = c(ID, name), names_from = window, values_from = value) %>%
     dplyr::arrange(name) %>%
     dplyr::rename(Variables = name)

  df_acf <-  dplyr::bind_rows(df, .id = "ID") %>%
     dplyr::filter(method == 'autocorrelation') %>%
     dplyr::select_if(~!(all(is.na(.)) | all(is.list(.)))) %>%
     select(-window_starts, -window_ends, -method, -period_hours, -from, -to) %>%
     tidyr::pivot_longer(-c(ID,window)) %>%
     tidyr::pivot_wider(id_cols = c(ID, name), names_from = window, values_from = value) %>%
     dplyr::arrange(name) %>%
     dplyr::rename(Variables = name)

  names(df_acf) = stringr::str_replace(names(df_acf), '\\d', paste('Window', names(df_acf)))
  names(df_lomb) = stringr::str_replace(names(df_lomb), '\\d', paste('Window', names(df_lomb)))

  lomb_filename = paste0(path,"/","lomb_scargle.csv")
  acf_filename = paste0(path,"/","autocorrelation.csv")


   if (export) {
     readr::write_csv(df_acf, acf_filename)
     readr::write_csv(df_lomb, lomb_filename)
     } else {return(list(df_lomb, df_acf))}


 }
