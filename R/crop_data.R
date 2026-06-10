#' Crop data to specific dates
#' @details A helper function wrapped around dplyr::filter and lubridate::parse_date_time which helps to data to
#' datetimes of interest.
#' @param df a list of data.frame/tibble with a column named datetime which is also a datetime object.
#' @param from,to a string which indicates a date in the ISO 8601 format.
#'
#' @return A list of data.frames/tibbles with dates within the specified window of time.
#' @export
#'
#' @import magrittr
#' @importFrom furrr future_map
#' @importFrom dplyr filter
#' @importFrom lubridate parse_date_time
#'
#'
#'
crop_data <- function(df = NULL, from = NULL, to = NULL) {
  if (is.null(from) & is.null(to)) {return(df)}

  # df is a *list* of data.frames, so df$datetime is NULL and min()/max() on it
  # would return Inf -> an unparseable bound -> every row silently filtered out.
  # Derive the missing bound from each element's first (datetime) column instead.
  if (is.null(from)) {
    from = as.character(min(do.call(c, lapply(df, function(d) min(d[[1]], na.rm = TRUE)))))
  }

  if (is.null(to)) {
    to = as.character(max(do.call(c, lapply(df, function(d) max(d[[1]], na.rm = TRUE)))))
  }



  # Parse the bounds once rather than re-parsing them for every element below.
  from_dt = parse_date_time(from, orders = "%y%m%d %H%M%S", truncated = 5)
  to_dt   = parse_date_time(to,   orders = "%y%m%d %H%M%S", truncated = 5)
  df = future_map(
    .x = df,
    .f = ~ filter(.x,
                  !!as.symbol(names(.x)[1]) >= from_dt,
                  !!as.symbol(names(.x)[1]) <= to_dt)
  )

  return(df)
}
