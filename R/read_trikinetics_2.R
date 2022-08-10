#' Import data from a Trikinetics tsv
#' @param file a path to a Trikinetics file. If no path is supplied, a GUI will open and help with file selection.
#' @export read_trikinetics_2
#' @export read_trikinetics_folder_2
#'
#' @examples
#' trikinetics_data <- read_trikinetics()
#' @importFrom readr read_tsv
#' @importFrom magrittr '%>%'
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @importFrom lubridate parse_date_time
#' @importFrom tidyr pivot_longer
#'
read_trikinetics_2 <- function(file = NULL){

  ##### Flow Control #####
  #Allow for using a GUI to choose the file, if one is not supplied
  if (is.null(file)) {
    file <- file.choose()
  }

  # Import the file
  df <- read_tsv(file,col_names = FALSE, show_col_types = FALSE)
  df <- df[,-c(1,4:9)]
  df <- unite(df, "datetime", c(1:2), sep = " ") %>% mutate(datetime = parse_date_time(datetime, orders = "d m y H:M:S"))
  names(df) <- c("datetime", "ld", paste0("IND ",1:(ncol(df)-2)))
  df <- tidyr::pivot_longer(df, c(-datetime, -ld))
  df <- tidyr::nest(df, data = -name)
  df <- as.list(df)
  names(df$data) <- df$name
  df <- df$data
  return(df)
}

.read_trikinetics <- function(file = NULL){

  ##### Flow Control #####
  #Allow for using a GUI to choose the file, if one is not supplied
  if (is.null(file)) {
    file <- file.choose()
  }

  # Import the file
  df <- read_tsv(file,col_names = FALSE, show_col_types = FALSE)
  df <- df[,-c(1,4:9)]
  df <- unite(df, "datetime", c(1:2), sep = " ") %>% mutate(datetime = parse_date_time(datetime, orders = "d m y H:M:S"))
  names(df) <- c("datetime", "ld", paste0("IND ",1:(ncol(df)-2)))
  df <- tidyr::pivot_longer(df, c(-datetime, -ld))
  return(df)
}





read_trikinetics_folder_2 <- function(directory = NULL) {

  #### Flow Control ####
  #Allow for using a GUI to choose the folder, if one is not supplied
  if (is.null(directory)) {
    directory <- rstudioapi::selectDirectory()
  }
  # Create a plan for parallelization
  future::plan(future::multisession)

  files <- list.files(directory)
  files <- files[stringr::str_detect(files, '\\.txt')]
  paths <- paste0(directory, "/", files)
  names(paths) <- stringr::str_remove(files, "\\.txt")
  df <- suppressMessages(furrr::future_map(paths, read_trikinetics_2, .options = furrr::furrr_options(seed = TRUE)))
  df <- do.call(c, df)
  return(df)
}
