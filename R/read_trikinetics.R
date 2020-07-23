#' Import data from a Trikinetics tsv
#' @param file a path to a Trikinetics file. If no path is supplied, a GUI will open and help with file selection.
#' @export read_trikinetics
#' @export read_trikinetics_folder
#'
#' @examples
#' trikinetics_data <- read_trikinetics()
#' @importFrom readr read_tsv
#' @importFrom magrittr '%>%'
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @importFrom lubridate parse_date_time
read_trikinetics <- function(file = NULL){

##### Flow Control #####
  #Allow for using a GUI to choose the file, if one is not supplied
  if (is.null(file)) {
  file <- file.choose()
  }

# Import the file
  df <- read_tsv(file,col_names = FALSE)
  df <- df[,-c(1,4:9)]
  df <- unite(df, "datetime", c(1:2), sep = " ") %>% mutate(datetime = parse_date_time(datetime, orders = "d m y H:M:S"))
  names(df) <- c("datetime", "dd", paste0("IND ",1:(ncol(df)-2)))
  return(df)
}

read_tikinetics_folder <- function(directory = NULL) {

message("Make sure, all monitors were run on the same dates with the same LD/DD settings.")

  #### Flow Control ####
  #Allow for using a GUI to choose the folder, if one is not supplied
  if (is.null(directory)) {
    directory <- rstudioapi::selectDirectory()
  }

  files <- list.files(directory)
  paths <- paste0(directory, "/", files)
  df <- suppressMessages(purrr::map(paths, read_trikinetics))
  names(df) <- stringr::str_remove(files, "\\.txt")
  df <- purrr::map_df(df, ~ tidyr::pivot_longer(., -c(1,2)), .id = "monitor")
  df <-  tidyr::unite(df, "name", c(monitor, name), sep = " ")
  df <- tidyr::pivot_wider(df, c(datetime, tidyr::matches("dd|ld")))
  return(df)
}
