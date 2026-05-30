#This function will resample time series data (upsample or downsample) to the nearest minute, half-hour, hour, or day.
#It will also allow for inputation when upsampling and takin the mean, median, or sum when downsampling.
#data must be in tidy format for it to work

#data = data frame or tibble
#datetime_column = which column in data has the datetime?
#amount = how many units do you want to downsample for?
#units = which unit do you want to downsample for or with?
#method = which method do you want to use for summarizing the data?

# Internal single-series worker — not exported.
# Called by the public mapper `downsample_time_series` below.
#
#' @importFrom dplyr pull summarise ungroup group_by
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom lubridate floor_date period
#' @importFrom rlang sym as_function
#' @importFrom stringr str_remove
#' @importFrom magrittr '%>%'
#'
.downsample_one <- function(data = NULL,
                                 datetime_column = "datetime",
                                 amount = 1,
                                 units = "hour",
                                 method = c("mean", "sum", "median"))
{



#Managing arguments
  #to work within the tidyverse symtems these chracters must be turned into symbols
datetime_column <- sym(datetime_column)


  #choose a time point to roll back the time by a certain amount
units <- str_remove(units, "\\d")

  #handle the anonymous function naming
method <- match.arg(method, choices = c("mean", "sum", "median"))
method <- as_function(method)
method <- match.fun(method, c(mean, sum, median))


#flooring dates
floored_dates <- floor_date(pull(data, !!datetime_column),
                                       unit = period(amount, units = units))
data[as.character(datetime_column)] <- floored_dates

#pivot data longer for downsampling

data <- data %>%
  pivot_longer(-!!datetime_column, names_to = "id", values_to = "values")


#downsampling
data <- data %>%
  group_by(!!datetime_column, id) %>%
  summarise(downsampled_data =  method(values, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(!!datetime_column, names_from = "id", values_from = "downsampled_data")


  return(data)
}


#' Downsample a timeseries with a datetime object
#' @usage
#' downsample_time_series(data = NULL, amount = 30,
#'        units = c("minute", "hour", "day", "week"), method = c("mean", "sum", "median"))
#'
#' @description
#' This function will resample time series data (downsample) to the nearest minute, half-hour, hour, or day.
#' It allows to summarise the measurement values using mean, median, or sum.
#' @param data a list of data frames or tibbles, each with a POSIXct datetime column.
#' @param amount the amount of units to downsample for
#' @param units a character object like:"hour", "minute", "day", "week". Default is "hour"
#' @param method methods by which to summarise the measurement values in the timeseries: "mean", "sum", "median". Default is "mean".
#'
#' @return
#' A list of data frames with downsampled data.
#'
#' @export downsample_time_series
#'
#' @examples
#' df_downsampled <- downsample_time_series(data = raw_data,
#' amount = 30, units = "hour", method = "sum")
#'
#' @importFrom dplyr pull summarise group_by
#' @importFrom lubridate floor_date period
#' @importFrom rlang sym as_function
#' @import magrittr
#' @importFrom furrr future_map
#'
downsample_time_series <- function(data = NULL,
                                   amount = 1,
                                   units = "hour",
                                   method = c("mean", "sum", "median"))
{
 data = future_map(
    .x = data,
    .f = ~ .downsample_one(
      .x,
      amount = amount,
      units = units,
      method = method
    )
  )
  #return the result of downsampling
  return(data)
}
