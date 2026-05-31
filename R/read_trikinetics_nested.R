#' Import data from a Trikinetics tsv
#' @param file a path to a Trikinetics file.
#' @param directory a path to a folder containing Trikinetics files.
#' @param ... Arguments passed on to \code{read_trikinetics_nested} or \code{read_trikinetics_folder_nested}.
#' @return A named list of tibbles, one per individual, each with columns \code{datetime}, \code{ld}, and \code{value}.
#' @export read_trikinetics_nested
#'
#' @examples
#' \dontrun{
#' trikinetics_data <- read_trikinetics_nested(file = "/path/to/file.txt")
#' }
#' @importFrom readr read_tsv
#' @importFrom magrittr '%>%'
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @importFrom lubridate parse_date_time
#' @importFrom tidyr pivot_longer
#'
read_trikinetics_nested <- function(file = NULL){

  ##### Flow Control #####
  if (missing(file) || is.null(file)) {
    stop("`file` is required. Use read_trikinetics_nested_interactive() to choose one via a dialog.")
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
  if (missing(file) || is.null(file)) {
    stop("`file` is required.")
  }

  # Import the file
  df <- read_tsv(file,col_names = FALSE, show_col_types = FALSE)
  df <- df[,-c(1,4:9)]
  df <- unite(df, "datetime", c(1:2), sep = " ") %>% mutate(datetime = parse_date_time(datetime, orders = "d m y H:M:S"))
  names(df) <- c("datetime", "ld", paste0("IND ",1:(ncol(df)-2)))
  df <- tidyr::pivot_longer(df, c(-datetime, -ld))
  return(df)
}




#' @rdname read_trikinetics_nested
#' @export
read_trikinetics_folder_nested <- function(directory = NULL) {

  #### Flow Control ####
  if (missing(directory) || is.null(directory)) {
    stop("`directory` is required. Use read_trikinetics_folder_nested_interactive() to choose one via a dialog.")
  }
  # Save and restore plan for parallelization
  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  future::plan(future::multisession)

  files <- list.files(directory)
  files <- files[stringr::str_detect(files, '\\.txt')]
  paths <- paste0(directory, "/", files)
  names(paths) <- stringr::str_remove(files, "\\.txt")
  df <- suppressMessages(furrr::future_map(paths, read_trikinetics_nested, .options = furrr::furrr_options(seed = TRUE)))
  df <- do.call(c, df)
  return(df)
}

#' @rdname read_trikinetics_nested
#' @export
read_trikinetics_nested_interactive <- function(...) read_trikinetics_nested(file.choose(), ...)

#' @rdname read_trikinetics_nested
#' @export
read_trikinetics_folder_nested_interactive <- function(...) read_trikinetics_folder_nested(rstudioapi::selectDirectory(), ...)
