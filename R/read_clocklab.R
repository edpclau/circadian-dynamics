#' Import data from Clocklab
#'
#' @param path A `.csv` exported from Clocklab, *or* a directory of them (all
#'   individuals must belong to the same group and be run on the same dates).
#'   If `NULL` (default) a file dialog opens; a directory is detected automatically.
#'
#' @return A tibble with `datetime`, `ld` (light status), and one column of
#'   measurement values per individual.
#' @export
#' @examples
#' \dontrun{
#' df <- read_clocklab("/path/to/file.csv")
#' df <- read_clocklab("/path/to/folder")
#' }
read_clocklab <- function(path = NULL) {
  if (is.null(path)) path <- file.choose()
  if (dir.exists(path)) return(.read_clocklab_folder(path))
  .parse_clocklab(path)
}

.parse_clocklab <- function(file) {
  ind_label <- suppressMessages(readr::read_csv(file, col_names = FALSE, skip = 1, n_max = 1)) %>%
    dplyr::pull(1) %>%
    stringr::str_replace_all(" ", "_")
  start_date <- suppressMessages(readr::read_csv(file, col_names = FALSE, skip = 2, n_max = 1)) %>%
    dplyr::pull(1) %>%
    lubridate::dmy()
  df <- suppressMessages(readr::read_csv(file, skip = 3))
  df <- df %>%
    dplyr::mutate(datetime = start_date + (lubridate::days(df$Day) - lubridate::days(df$Day)[1]) +
                    lubridate::hours(Hr) + lubridate::minutes(Min)) %>%
    dplyr::select(datetime, ld = Lights, `Cnts/min`)
  names(df)[3] <- ind_label
  df
}

.read_clocklab_folder <- function(directory) {
  message("Make sure the experiments were run on the same dates.")
  paths <- list.files(directory, full.names = TRUE)
  df <- purrr::map(paths, .parse_clocklab)
  df <- purrr::list_rbind(lapply(df, function(d) tidyr::pivot_longer(d, -c(1, 2))))
  tidyr::pivot_wider(df, id_cols = c("datetime", "ld"))
}
