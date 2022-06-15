#' Managing Inactive Variables
#' @usage rm_inactive_dates(df,inactivity_period = "1 day", sampling_rate = "1 hour")
#' @usage rm_inactive_variables(df,inactivity_period = "1 day", sampling_rate = "1 hour")
#' @usage report_inactive_variables(df,inactivity_period = "1 day", sampling_rate = "1 hour")
#'
#' @description rm_inactive_dates drops the rows of data during which an variable was "inactive" ie. equal to 0.
#' @description rm_inactive_variables drops the rows of data during which
#' @param df a data.frame where the first column is a datetime object.
#' @param inactivity_period a string indicating what will be considered the period of inactivity
#' necessary to be removed. Default = "1 day".
#' @param sampling_rate a string indicating the sampling rate of the data. Default = "1 hour".
#'
#' @examples  df = downsample_time_series_2(trikinetics, amount = 1, units = 'hour', method = 'sum')
#' cropped_dates_df = rm_inactive_dates(df, inactivity_period = '1 day', sampling_rate = '1 hour')
#' print(cropped_dates_df)
#' cropped_variables_df = rm_inactive_variables(df,inactivity_period = "1 day", sampling_rate = "1 hour")
#' print(cropped_variables_df)
#'
#' @export rm_inactive_dates
#' @export rm_inactive_variables
#' @export report_inactive_variables
#'
#' @import magrittr
#' @importFrom future plan sequential
#' @importFrom lubridate duration
#' @importFrom furrr future_map future_map2
#' @importFrom dplyr filter tibble group_by mutate all_of select
#'
#'
#'
rm_inactive_dates <- function(df,inactivity_period = "1 day", sampling_rate = "1 hour") {

  #Definition for what we will consider an inactive dates
  inactivity_threshold = duration(inactivity_period) / duration(sampling_rate)

  #plan for paralellization
  plan(sequential)

  to_return = future_map(
    .x = df,
    .f = ~ {
      #Identify Sequences of activity
      rles = rle(.x[[3]])
      rles = tibble(lengths = rles$lengths, values = rles$values)

      #Identify perdiods where activity or measurements are 0
      inactive = mutate(rles, inactive = ifelse(values == 0 & lengths >= inactivity_threshold, TRUE, FALSE))$inactive

      #If there is an inactive period, return only the dates before that datetime
      if (any(inactive)) {
        inactive_point = which.max(inactive) - 1
        rows = sum(rles[1:inactive_point,][[1]])
        return(.x[1:rows,])
      } else {
        return(.x)
      }
    }
    )

  return(to_return)


  }


report_inactive_variables <- function(df,inactivity_period = "1 day", sampling_rate = "1 hour") {

  #Definition for what we will consider an inactive dates
  inactivity_threshold = duration(inactivity_period) / duration(sampling_rate)

  plan(sequential)
  to_return = future_map2(
    .x = df,
    .y = names(df),
    .f = ~ {
      #Identify Sequences of activity
      rles = rle(.x[[3]])
      rles = tibble(lengths = rles$lengths, values = rles$values)

      #Identify perdiods where activity or measurements are 0
      inactive = mutate(rles, inactive = ifelse(values == 0 & lengths >= inactivity_threshold, TRUE, FALSE))$inactive

      #If there is an inactive period, return only the active variables
      if (any(inactive)) {
        return(.y)
      }
    }
  )





  return(as.character(to_return))
}


rm_inactive_variables <- function(df,inactivity_period = "1 day", sampling_rate = "1 hour") {

  inactive = report_inactive_variables(df, inactivity_period, sampling_rate)

  return(df[!(names(df) %in% test)])

  }
