#' Import circadian data from a generic CSV
#'
#' @param file Path to a `.csv` whose first column is the datetime and remaining
#'   columns are individuals/signals. If `NULL` (default) a file dialog opens.
#' @return A named list of tibbles, one per individual/column, each with columns
#'   `datetime` and `value`.
#' @export
#' @examples
#' \dontrun{
#' circadian_data <- read_csv_data("/path/to/data.csv")
#' }
#' @importFrom readr read_csv
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer nest
read_csv_data <- function(file = NULL) {
  if (is.null(file)) file <- file.choose()
  df <- read_csv(file, show_col_types = FALSE)
  names(df) <- c("datetime", names(df)[-1])
  df <- df %>% pivot_longer(-datetime) %>% nest(data = -name)
  stats::setNames(df$data, df$name)
}
