#' Crop data to specific dates
#' @details A helper function wrapped around dplyr::filter and lubridate::parse_date_time which helps to data to
#' datetimes of interest.
#' @param list_df a list of data.frame/tibble with a column named datetime which is also a datetime object.
#' @param from,to a string which indicates a date in the ISO 8601 format.
#'
#' @return A list of data.frames/tibbles with dates within the specified window of time.
#' @export
#'
#' @import magrittr
#' @importFrom future plan sequential
#' @importFrom furrr future_map
#' @importFrom dplyr filter
#' @importFrom lubridate parse_date_time
#'
#'
#'
crop_data <- function(df = NULL, from = NULL, to = NULL) {
  if (is.null(from) & is.null(to)) {return(df)}

  if (is.null(from)) {from = as.character(min(df$datetime, na.rm = TRUE))}

  if (is.null(to)) {to = as.character(max(df$datetime, na.rm = TRUE))}

  names(df)[1] = 'datetime'

  plan(sequential)

  df = future_map(
    .x = df,
    .f = ~ {
      filter(.x,
                    datetime >= parse_date_time(from, orders = "%y%m%d %H%M%S", truncated = 5),
                    datetime <= parse_date_time(to, orders = "%y%m%d %H%M%S", truncated = 5))
    }
  )

  return(df)
}
