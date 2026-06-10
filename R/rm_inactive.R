#' Remove Inactive Individuals
#'
#' @param df a data.frame where the first column is a datetime object.
#' @param inactivity_period a string indicating what will be considered the period of inactivity
#' necessary to be removed. Default = "1 day".
#' @param sampling_rate a string indicating the sampling rate of the data. Default = "1 hour".
#' @return A data.frame with only the active individuals (columns) retained.
#'
#' @export
#'
#'
rm_inactive <- function(df,inactivity_period = "1 day", sampling_rate = "1 hour") {
  # Runs on the caller's future plan (sequential by default); the RLE scan is
  # deterministic, so this does not force a multisession plan of its own.
    inactivity_threshold <- .inactivity_threshold(inactivity_period, sampling_rate)
    rles <- furrr::future_map(df[-1], ~ rle( .))
    rles <- furrr::future_map_dfr(rles, ~ tibble(lengths = .[[1]], values = .[[2]]), .id = "IND")
    # Longest zero-run per individual. Grouping the *full* RLE table (not a
    # values == 0 pre-filter) is essential: an individual that never hits zero must
    # still get a row, with max_zeroes = 0, otherwise the most active animals are
    # silently dropped. max(c(0L, ...)) yields 0 for individuals with no zero runs.
    active_inds <- rles %>%
      dplyr::group_by(IND) %>%
      dplyr::summarize(max_zeroes = max(c(0L, lengths[values == 0])), .groups = "drop") %>%
      dplyr::filter(max_zeroes < inactivity_threshold) %>%
      pull(1)

    df_active <- dplyr::select(df, 1, dplyr::all_of(active_inds))

    return(df_active)
  }
