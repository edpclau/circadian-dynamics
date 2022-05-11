#' Subtract Arbitrarily Chosen Noise Level from the Data
#' @usage arbitrary_noise_reduction(df, noise = 0)
#' @param df a data.frame where the first column is a datetime object.
#' @param noise an integer representing an arbitrarily chosen noise level to subtract from the data.
#' Default = 0.
#'
#' @return
#' @export
#'
#'@importFrom dplyr select everything
#'@importFrom furrr future_map_dfr
#'@importFrom future plan sequential
#'
arbitrary_noise_subtraction <- function(df, noise = 0) {

plan(sequential)

noise_reduced <- future_map_dfr(df[-1], ~ ifelse(. - noise < 0 | is.na(.), 0, . - noise))

noise_reduced$datetime <- df[[1]]

noise_reduced <- select(noise_reduced, datetime, everything())

return(noise_reduced)

}
