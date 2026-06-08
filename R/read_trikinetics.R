#' Import Trikinetics data
#'
#' @description Reads a single Trikinetics `.txt` (tab-separated) file or a whole
#'   folder of them. `layout = "nested"` returns a named list of per-individual
#'   tibbles (the form consumed by [process_timeseries_main]); `layout = "long"`
#'   returns one wide data.frame with a column per individual (handy for [actogram]).
#'
#' @param path Path to a Trikinetics file *or* a directory of them. If `NULL`
#'   (default) a file dialog opens. A directory is detected automatically.
#' @param layout One of `"nested"` (default) or `"long"`.
#'
#' @return For `"nested"`, a named list of tibbles with columns `datetime`, `ld`,
#'   and `value`. For `"long"`, a data.frame with `datetime`, a light/dark column,
#'   and one column per individual.
#'
#' @export
#' @examples
#' \dontrun{
#' trikinetics <- read_trikinetics("/path/to/file.txt")
#' wide <- read_trikinetics("/path/to/folder", layout = "long")
#' }
#' @importFrom readr read_tsv
#' @importFrom tidyr unite pivot_longer pivot_wider nest matches
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr all_of
read_trikinetics <- function(path = NULL, layout = c("nested", "long")) {
  layout <- match.arg(layout)
  if (is.null(path)) path <- file.choose()
  if (dir.exists(path)) return(.read_trikinetics_folder(path, layout))
  .parse_trikinetics(path, layout)
}

# Single-file parser. The first steps are identical for both layouts; they
# diverge only in the second-column label and whether the result is nested.
.parse_trikinetics <- function(file, layout) {
  df <- readr::read_tsv(file, col_names = FALSE, show_col_types = FALSE)
  df <- df[, -c(1, 4:9)]
  df <- tidyr::unite(df, "datetime", c(1, 2), sep = " ")
  df$datetime <- lubridate::parse_date_time(df$datetime, orders = "d m y H:M:S")
  light <- if (layout == "long") "dd" else "ld"
  names(df) <- c("datetime", light, paste0("IND ", seq_len(ncol(df) - 2)))
  if (layout == "long") return(df)
  df <- tidyr::pivot_longer(df, cols = !dplyr::all_of(c("datetime", light)))
  df <- tidyr::nest(df, data = -name)
  stats::setNames(df$data, df$name)
}

.read_trikinetics_folder <- function(directory, layout) {
  files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
  ids <- stringr::str_remove(basename(files), "\\.txt$")
  if (layout == "nested") {
    dfs <- suppressMessages(purrr::map(stats::setNames(files, ids), .parse_trikinetics, layout = "nested"))
    return(do.call(c, dfs))
  }
  message("Make sure all monitors were run on the same dates with the same LD/DD settings.")
  dfs <- suppressMessages(purrr::map(stats::setNames(files, ids), .parse_trikinetics, layout = "long"))
  dfs <- purrr::list_rbind(lapply(dfs, function(d) tidyr::pivot_longer(d, -c(1, 2))), names_to = "monitor")
  dfs <- tidyr::unite(dfs, "name", c("monitor", "name"), sep = " ")
  tidyr::pivot_wider(dfs, id_cols = c("datetime", tidyr::matches("dd|ld")))
}
