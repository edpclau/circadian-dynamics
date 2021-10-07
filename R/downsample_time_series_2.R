#This function will resample time series data (upsample or downsample) to the nearest minute, half-hour, hour, or day.
#It will also allow for inputation when upsampling and takin the mean, median, or sum when downsampling.
#data must be in tidy format for it to work

#data = data frame or tibble
#datetime_column = which column in data has the datetime?
#amount = how many units do you want to downsample for?
#units = which unit do you want to downsample for or with?
#method = which method do you want to use for summarizing the data?

#' Downsample a timeseries with a datetime object
#' @usage
#' downsample_time_series(data = NULL, datetime_column = "datetime",amount = 30,
#'        units = c("minute", "hour", "day", "week"), method = c("mean", "sum", "median"))
#'
#' @description
#' This function will resample time series data ( downsample) to the nearest minute, half-hour, hour, or day.
#'It allow to summarise the measurement values using mean, median, or sum.
#' @param data data frame or tibble with 2 columns. One column must be a POSIXct object.
#' @param datetime_column  column in data that has the datetime
#' @param amount the amount of units to downsample for
#' @param units a character object like:"hour", "minute", "day", "week". Default is "minute"
#' @param method methods by which to summarise the measurement values in the timeseries: "mean", "sum", "median". Default is "mean".
#'
#' @return
#' A data.frame with 2 columns:
#' downsampled_data: summarise measurement values
#' floored_dates: downsampled POSIXct object
#'
#' @export downsample_time_series_2
#'
#' @examples
#' df_downsampled <- downsample_time_series(data = raw_data,
#' datetime_column = "datetime", amount = 30, units = "hour", method = "sum")
#'
#' @importFrom dplyr pull summarise group_by
#' @importFrom lubridate floor_date period
#' @importFrom rlang sym as_function
#' @import magrittr
#'
downsample_time_series_2 <- function(data = NULL,
                                   amount = 1,
                                   units = "hour",
                                   method = c("mean", "sum", "median"))
{
 data = map(
    .x = data,
    .f = ~ downsample_time_series(
      .x,
      amount = amount,
      units = units,
      method = method
    )
  )
  #return the result of downsampling
  return(data)
}
