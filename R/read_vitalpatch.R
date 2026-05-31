#' @title Read a Folder of Vital Patch Data
#' @description This function imports a folder of Vital Patch data in the standard format. That is,
#' the format provided in Vital Patch's Eval Data. There should be a folder per channel. read_vitalpatch
#' concatenates all the data into a single tibble (data.frame) so that the user doesn't have to do it manually.
#'
#'
#' @param folder A path (directory) to the folder we want to analyze.
#' @param ... Arguments passed on to \code{read_vitalpatch}.
#'
#' @return Returns a tibble (data.frame) object with parsed dates.
#'
#' @details Parallelism follows the caller's \code{future::plan()}; set a plan (e.g. \code{future::plan(future::multisession)}) before calling to parallelize.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read_vitalpatch(folder = "/path/to/folder")
#' }
#'
#' @importFrom lubridate as_datetime
#' @importFrom dplyr rename
#' @importFrom purrr map_df
#'
read_vitalpatch <- function(folder = NULL) {

  ##### Flow Control #####
  if (missing(folder) || is.null(folder)) {
    stop("`folder` is required. Use read_vitalpatch_interactive() to choose one via a dialog.")
  }
files = list.files(folder, full.names = TRUE)

df = furrr::future_map_dfr(files, readr::read_csv, col_types = cols(.default = 'd'))

df$Time = lubridate::as_datetime(df$Time/1000)

df = dplyr::rename(df, datetime = Time)

return(df)
}

#' @rdname read_vitalpatch
#' @export
read_vitalpatch_interactive <- function(...) read_vitalpatch(rstudioapi::selectDirectory(), ...)
