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
average_of_group <- function(df = NULL) {
df_long <- tidyr::pivot_longer(df, -1, names_to = "ID", values_to = "value")
df_mean <- dplyr::group_by(df_long, datetime) %>%
  dplyr::summarise(mean = mean(value, na.rm = TRUE), .groups = "drop")
df <- dplyr::left_join(df, df_mean, by = names(df)[1])
df <- dplyr::select(df, 1, mean, tidyr::everything())

return(df)
}
