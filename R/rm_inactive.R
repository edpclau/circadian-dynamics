#' Remove Inactive Individuals
#' @usage rm_inactive(df,inactivity_period = "1 day", sampling_rate = "1 hour")
#' @param df a data.frame where the first column is a datetime object.
#' @param inactivity_period a string indicating what will be considered the period of inactivity
#' necessary to be removed. Default = "1 day".
#' @param sampling_rate a string indicating the sampling rate of the data. Default = "1 hour".
#'
#' @export
#'
#'
rm_inactive <- function(df,inactivity_period = "1 day", sampling_rate = "1 hour") {
  #Plan for paralellization
future::plan(future::multisession)

    inactivity_threshold <- lubridate::duration(inactivity_period) / lubridate::duration(sampling_rate)
    rles <- furrr::future_map(df[-1], ~ rle( .))
    rles <- furrr::future_map_dfr(rles, ~ tibble(lengths = .[[1]], values = .[[2]]), .id = "IND")
    active_inds <- dplyr::filter(rles, values == 0) %>%
      dplyr::group_by(IND) %>%
      dplyr::summarize(max_zeroes = max(lengths), .groups = "drop") %>%
      dplyr::filter(max_zeroes < inactivity_threshold) %>%
      pull(1)

    df_active <- dplyr::select(df, 1, dplyr::all_of(active_inds))

    return(df_active)
  }
