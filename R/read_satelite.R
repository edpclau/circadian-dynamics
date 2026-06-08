#' Import weather data from satellite
#'
#' @param file Path to the satellite data CSV. If `NULL` (default) a file dialog opens.
#' @return A tibble with columns `datetime` and one column of numeric satellite values.
#' @export
#' @examples
#' \dontrun{
#' read_satellite("/path/to/data.csv")
#' }
read_satellite <- function(file = NULL) {
  if (is.null(file)) file <- file.choose()
  df <- readr::read_csv(file, skip = 8, col_types = list(readr::col_datetime(), readr::col_double()))
  dplyr::rename(df, "datetime" = time)
}
