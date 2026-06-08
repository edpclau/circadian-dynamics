#' Import a folder of Vital Patch data
#'
#' @description Imports a folder of Vital Patch "Eval Data" (one subfolder per
#'   channel) and concatenates it into a single tibble.
#'
#' @param folder Path to the folder to import. If `NULL` (default) a directory
#'   dialog opens. Parallelism follows the caller's [future::plan()].
#' @return A tibble with parsed `datetime` and the channel columns.
#' @export
#' @examples
#' \dontrun{
#' read_vitalpatch("/path/to/folder")
#' }
#' @importFrom lubridate as_datetime
#' @importFrom dplyr rename
#' @importFrom readr cols
read_vitalpatch <- function(folder = NULL) {
  if (is.null(folder)) folder <- rstudioapi::selectDirectory()
  files <- list.files(folder, full.names = TRUE)
  df <- furrr::future_map_dfr(files, readr::read_csv, col_types = cols(.default = "d"))
  df$Time <- lubridate::as_datetime(df$Time / 1000)
  dplyr::rename(df, datetime = Time)
}
