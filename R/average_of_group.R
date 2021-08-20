#' Create a timeseries mean based on all the samples in the data
#'
#' @param df a data.frame where the first column is a POSIXct object and every
#' subsequent column is a sample.
#'
#' @return returns the inputted data.frame with a mean column added.
#' @export
#'
#' @examples
#' group_average(monitor_downsampled)
#'
#' @importFrom tidyr pivot_longer everything
#' @importFrom dplyr group_by summarise left_join select
#' @importFrom dplyr summarise
#' @import magrittr
average_of_group <- function(df = NULL) {
df_long <- pivot_longer(df, -1, names_to = "ID", values_to = "value")
df_mean <- group_by(df_long, datetime) %>%
  summarise(mean = mean(value, na.rm = TRUE), .groups = "drop")
df <- left_join(df, df_mean, by = names(df)[1])
df <- select(df, 1, mean, everything())

return(df)
}
