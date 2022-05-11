#' Import data from a Trikinetics tsv
#' @param file a path to a csv file. If no path is supplied, a GUI will open and help with file selection.
#' @export read_csv_data
#'
#' @examples
#' circadian_data <- read_csv_data()
#' @importFrom readr read_csv
#' @importFrom magrittr '%>%'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest

read_csv_data <- function(file = NULL){

  ##### Flow Control #####
  #Allow for using a GUI to choose the file, if one is not supplied
  if (is.null(file)) {
    file = file.choose()
  }

  # Import the file
  df = read_csv(file)
  names(df) = c('datetime', names(df)[-1])
  df = df %>% pivot_longer(-datetime) %>% nest(data = -name)
  names(df$data) <- df$name
  df = df$data
  return(df)
}
