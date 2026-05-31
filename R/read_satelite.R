#' Import Weather data From satellite
#'
#' @param path A file path to the satellite data CSV.
#'
#' @return A tibble with columns \code{datetime} and one column of numeric satellite measurement values.
#' @export
#'
#' @examples read_satellite(path = "/path/to/data.csv")
#'
read_satellite <- function(path = NULL) {
  if (missing(path) || is.null(path)) {
    stop("`path` is required. Use read_satellite_interactive() to choose one via a dialog.")
  }
  df <- readr::read_csv(path, skip = 8, col_types = list(readr::col_datetime(), readr::col_double()))
  # df$time <- lubridate::mdy_hm(df$time)
  df <- dplyr::rename(df, 'datetime'=time)
  return(df)
}

#' @rdname read_satellite
#' @export
read_satellite_interactive <- function(...) read_satellite(rstudioapi::selectFile(), ...)
