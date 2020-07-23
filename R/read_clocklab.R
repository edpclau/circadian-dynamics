#' Import data from Clocklab
#' @usage
#' read_clocklab(file = NULL)
#'
#'
#' read_clocklab_folder(directory = NULL)
#'
#' @param file optional. A file path for a .csv outputted form the clocklab software.
#' @param directory optional. A folder path containing .csv files outputted from the clocklab software.
#' Make sure all individuals in the folder belong to the same experimental group and that the experiments
#' were run on the same dates.
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
#' df <- read_clocklab()
#' df <- read_clocklab_folder()
#'
read_clocklab <- function(file = NULL) {



##### Flow Control #####
#Allow for using a GUI to choose the file, if one is not supplied
if (is.null(file)) {
  file <- file.choose()
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


read_clocklab_folder <- function(directory = NULL) {

#### Flow Control ####
#Allow for using a GUI to choose the folder, if one is not supplied
if (is.null(directory)) {
  directory <- rstudioapi::selectDirectory()
}

files <- list.files(directory)
paths <- paste0(directory, "/", files)
df <- purrr::map(paths, read_clocklab)
df <- purrr::map_df(df, ~ tidyr::pivot_longer(., -c(1,2)))
df <- tidyr::pivot_wider(df, c(datetime,ld))

message("Make sure the experiments were run on the same dates")
return(df)
}
