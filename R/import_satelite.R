#' Import Weather data From satellite
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'
import_satellite <- function(path = NULL) {
  if (is.null(path)) path = rstudioapi::selectFile()
  readr::read_csv(path, skip = 9)
}
