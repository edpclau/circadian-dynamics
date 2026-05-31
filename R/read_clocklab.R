#' Import data from Clocklab
#'
#' @param file A file path for a .csv outputted from the clocklab software.
#' @param directory A folder path containing .csv files outputted from the clocklab software.
#' Make sure all individuals in the folder belong to the same experimental group and that the experiments
#' were run on the same dates.
#' @param ... Arguments passed on to \code{read_clocklab} or \code{read_clocklab_folder}.
#'
#' @return
#' Returns a data.frame/tibble with 3 columns:
#' datatetime of the experiment
#' ld : light switch status
#' IND Name: measurement values
#' @export read_clocklab
#' @export read_clocklab_folder
#'
#' @examples
#' \dontrun{
#' df <- read_clocklab(file = "/path/to/file.csv")
#' df <- read_clocklab_folder(directory = "/path/to/folder")
#' }
#'
read_clocklab <- function(file = NULL) {

##### Flow Control #####
if (missing(file) || is.null(file)) {
  stop("`file` is required. Use read_clocklab_interactive() to choose one via a dialog.")
}

####### Import the file #####
# Extract IND name
ind_label <- suppressMessages(readr::read_csv(file, col_names = FALSE, skip = 1, n_max = 1)) %>%
  dplyr::pull(1) %>%
  stringr::str_replace_all(" ", "_")

#Extract Date
start_date <- suppressMessages(readr::read_csv(file, col_names = FALSE, skip = 2, n_max = 1)) %>%
  dplyr::pull(1) %>%
  lubridate::dmy()

# Extract datetime
df <- suppressMessages(readr::read_csv(file, skip = 3))
df <- df %>% dplyr::mutate(datetime = start_date + suppressMessages(lubridate::days(df$Day) - lubridate::days(df$Day)[1]) + lubridate::hours(Hr) + lubridate::minutes(Min)) %>%
  dplyr::select(datetime, ld = Lights, `Cnts/min`)
names(df)[3] <- ind_label


return(df)
}


#' @rdname read_clocklab
#' @export
read_clocklab_folder <- function(directory = NULL) {

#### Flow Control ####
if (missing(directory) || is.null(directory)) {
  stop("`directory` is required. Use read_clocklab_folder_interactive() to choose one via a dialog.")
}

files <- list.files(directory)
paths <- paste0(directory, "/", files)
df <- furrr::future_map(paths, read_clocklab)
df <- furrr::future_map_dfr(df, ~ tidyr::pivot_longer(., -c(1,2)))
df <- tidyr::pivot_wider(df, c(datetime,ld))

message("Make sure the experiments were run on the same dates")
return(df)
}

#' @rdname read_clocklab
#' @export
read_clocklab_interactive <- function(...) read_clocklab(file.choose(), ...)

#' @rdname read_clocklab
#' @export
read_clocklab_folder_interactive <- function(...) read_clocklab_folder(rstudioapi::selectDirectory(), ...)
