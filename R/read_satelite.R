#' Import Weather data From satellite
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'
read_satellite <- function(path = NULL) {
  if (is.null(path)) path = rstudioapi::selectFile()
  df <- readr::read_csv(path, skip = 9)
  df$time <- lubridate::mdy_hm(df$time)
  df <- dplyr::rename(df, 'datetime'=time)
  return(df)
}
