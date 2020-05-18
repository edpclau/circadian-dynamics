#' Import data from a Trikinetics tsv
#'
#' @export
#'
#' @examples
#' trikinetics_data <- read_trikinetics()
#'
read_trikinetics <- function(){
  library(tidyverse)
  library(lubridate)
  file <- file.choose()
  df <- read_tsv(file,col_names = FALSE)
  df <- df[,-c(1,4:10)]
  df <- unite(df, "datetime", c(1:2), sep = " ") %>% mutate(datetime = parse_date_time(datetime, orders = "d m y H:M:S"))
  names(df) <- c("datetime", paste0("IND ",1:(ncol(df)-1)))
  return(df)
}
