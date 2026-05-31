#' Import data from a Trikinetics tsv
#' @param file a path to a csv file.
#' @param ... Arguments passed on to \code{read_csv_data}.
#' @return A named list of tibbles, one per individual/column, each with columns \code{datetime}, \code{ld} (if present), and \code{value}.
#' @export read_csv_data
#'
#' @examples
#' \dontrun{
#' circadian_data <- read_csv_data(file = "/path/to/data.csv")
#' }
#' @importFrom readr read_csv
#' @importFrom magrittr '%>%'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest

read_csv_data <- function(file = NULL){

  ##### Flow Control #####
  if (missing(file) || is.null(file)) {
    stop("`file` is required. Use read_csv_data_interactive() to choose one via a dialog.")
  }

  # Import the file
  df = read_csv(file)
  names(df) = c('datetime', names(df)[-1])
  df = df %>% pivot_longer(-datetime) %>% nest(data = -name)
  names(df$data) <- df$name
  df = df$data
  return(df)
}

#' @rdname read_csv_data
#' @export
read_csv_data_interactive <- function(...) read_csv_data(file.choose(), ...)
