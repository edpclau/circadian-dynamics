#' @title  Read a Folder of Vital Patch Data
#'
#'
#' @description This function imports a folder of Vital Patch data in the standard format. That is,
#' the format provided in Vital Patch's Eval Data. There should be a folder per channel. read_vitalpatch
#' concatenates all the data into a single tibble (data.frame) so that the user doesn't have to do it manually.
#'
#'
#' @param folder Optional. A path (directory) to the folder we want to analyze.
#'
#' @return Returns a tibble (data.frame) object with parsed dates.
#' @export
#'
#' @examples read_vitalpatch()
#'
#' @importFrom lubridate as_datetime
#' @importFrom dplyr rename
#' @importFrom purrr map_df
#' @importFrom rstudioapi selectDirectory
#'
read_vitalpatch <- function(folder = NULL) {

  ##### Flow Control #####
  #Allow for using a GUI to choose the file, if one is not supplied
  if (is.null(folder)) {
    folder <- rstudioapi::selectDirectory()
  }

files = list.files(folder, full.names = TRUE)

df = purrr::map_df(files, readr::read_csv, col_types = cols(.default = 'd'))

df$Time = lubridate::as_datetime(df$Time/1000)

df = dplyr::rename(df, datetime = Time)

return(df)
}

