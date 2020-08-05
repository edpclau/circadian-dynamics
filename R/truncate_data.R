#' Truncate data
#' @details A helper function wrapped around dplyr::filter and lubridate::parse_date_time which helps to truncate datetime
#' data.
#' @param df a data.frame/tibble with a column named datetime which is also a datetime object.
#' @param from ,
#' @param to a string which indicates a date in the ISO 8601 format.
#'
#' @return
#' @export
#'
#' @importFrom magrittr `%>%`
#'
#'
truncate <- function(df = NULL, from = NULL, to = NULL) {
  if (is.null(from) & is.null(to)) {return(df)}

  if (is.null(from)) {from = as.character(min(monitor$datetime, na.rm = TRUE))}

  if (is.null(to)) {to = as.character(max(monitor$datetime, na.rm = TRUE))}


  df <- dplyr::filter(df,
                datetime >= lubridate::parse_date_time(from, orders = "%y%m%d %H%M%S", truncated = 5),
                datetime <= lubridate::parse_date_time(to, orders = "%y%m%d %H%M%S", truncated = 5))
  return(df)
}
