#' Bind Rows of Processed data
#'
#' @description Allows for easy export and binding of rows from the output of process_data.
#'
#' @param df the output from process_data
#' @param export If TRUE, will save the data.frame as a .csv in the current directory. If FALSE (default),
#' will return the bound data.frame.
#' @param path Where to save the file if export = TRUE. Defaults to current directory.
#'
#' @return A single data.frame with all individuals row-bound and an additional \code{ID} column, or \code{NULL} (invisibly) when \code{export = TRUE}.
#' @export
#'
#' @importFrom furrr future_map_dfr
#'
#' @examples
#' \dontrun{
#' bind_processed(df = monitor_processed)
#' }
#'
 bind_processed <- function(df = NULL, export = FALSE, path = getwd()) {
  df_bound <-  future_map_dfr(df, ~ dplyr::rename(., raw = 3), .id = "ID")

  filename = paste0(path,"/",substitute(df),".csv")

  if (export) {write.csv(df_bound, filename, row.names = FALSE)
    } else {return(df_bound) }


 }
