#' Import data from a Trikinetics tsv
#' @param file a path to a Trikinetics file.
#' @return A data.frame with columns: \code{datetime}, \code{dd}, and one column per individual.
#' @export read_trikinetics_long
#' @export read_trikinetics_folder_long
#'
#' @examples
#' trikinetics_data <- read_trikinetics_long(file = "/path/to/file.txt")
#' @importFrom readr read_tsv
#' @importFrom magrittr '%>%'
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @importFrom lubridate parse_date_time
#'
read_trikinetics_long <- function(file = NULL){

##### Flow Control #####
  if (missing(file) || is.null(file)) {
    stop("`file` is required. Use read_trikinetics_long_interactive() to choose one via a dialog.")
  }

# Import the file
  df <- read_tsv(file,col_names = FALSE)
  df <- df[,-c(1,4:9)]
  df <- unite(df, "datetime", c(1:2), sep = " ") %>% mutate(datetime = parse_date_time(datetime, orders = "d m y H:M:S"))
  names(df) <- c("datetime", "dd", paste0("IND ",1:(ncol(df)-2)))
  return(df)
}




read_trikinetics_folder_long <- function(directory = NULL) {

message("Make sure, all monitors were run on the same dates with the same LD/DD settings.")

  #### Flow Control ####
  if (missing(directory) || is.null(directory)) {
    stop("`directory` is required. Use read_trikinetics_folder_long_interactive() to choose one via a dialog.")
  }

  files <- list.files(directory)
  paths <- paste0(directory, "/", files)
  df <- suppressMessages(purrr::map(paths, read_trikinetics_long))
  names(df) <- stringr::str_remove(files, "\\.txt")
  df <- furrr::future_map_dfr(df, ~ tidyr::pivot_longer(., -c(1,2)), .id = "monitor")
  df <-  tidyr::unite(df, "name", c(monitor, name), sep = " ")
  df <- tidyr::pivot_wider(df, c(datetime, tidyr::matches("dd|ld")))
  return(df)
}

#' @rdname read_trikinetics_long
#' @export
read_trikinetics_long_interactive <- function(...) read_trikinetics_long(file.choose(), ...)

#' @rdname read_trikinetics_long
#' @export
read_trikinetics_folder_long_interactive <- function(...) read_trikinetics_folder_long(rstudioapi::selectDirectory(), ...)
