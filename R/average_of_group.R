#' Create a timeseries mean based on all the samples in the data
#'
#' @param df a data.frame where the first column is a POSIXct object and every
#' subsequent column is a sample.
#'
#' @return returns the inputted data.frame with a mean column added.
#' @export
#'
#' @examples
#' \dontrun{
#' average_of_group(df = monitor_downsampled)
#' }
#'
#' @importFrom dplyr select everything
average_of_group <- function(df = NULL) {
  # Row-wise mean across all measurement columns (assumes one row per datetime).
  df$mean <- rowMeans(df[-1], na.rm = TRUE)
  select(df, 1, mean, everything())
}
