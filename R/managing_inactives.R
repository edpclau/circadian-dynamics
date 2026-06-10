#' Managing Inactive Variables
#'
#' @description rm_inactive_dates drops the rows of data during which an variable was "inactive" ie. equal to 0.
#' @description rm_inactive_variables drops the rows of data during which
#' @param df a data.frame where the first column is a datetime object.
#' @param inactivity_period a string indicating what will be considered the period of inactivity
#' necessary to be removed. Default = "1 day".
#' @param sampling_rate a string indicating the sampling rate of the data. Default = "1 hour".
#'
#' @examples
#' \dontrun{
#' df = downsample_time_series(trikinetics, amount = 1, units = 'hour', method = 'sum')
#' cropped_dates_df = rm_inactive_dates(df, inactivity_period = '1 day', sampling_rate = '1 hour')
#' print(cropped_dates_df)
#' cropped_variables_df = rm_inactive_variables(df,inactivity_period = "1 day", sampling_rate = "1 hour")
#' print(cropped_variables_df)
#' }
#'
#' @return \code{rm_inactive_dates} and \code{rm_inactive_variables} return a subset of the input list with inactive individuals removed. \code{report_inactive_variables} returns a character vector of inactive individual names.
#' @name rm_inactive_dates
#' @rdname rm_inactive_dates
#' @export rm_inactive_dates
#'
#' @import magrittr
#' @importFrom lubridate duration
#' @importFrom furrr future_map future_map2
rm_inactive_dates <- function(df, inactivity_period = "1 day", sampling_rate = "1 hour") {
  threshold <- .inactivity_threshold(inactivity_period, sampling_rate)
  future_map(df, ~ {
    runs <- .inactive_runs(.x[["value"]], threshold)
    if (!any(runs$inactive)) return(.x)
    # Keep only the rows before the first inactive run. If the first run is itself the
    # inactive one there is no active prefix, so return an empty frame.
    inactive_point <- which.max(runs$inactive) - 1L
    if (inactive_point == 0) return(.x[0, ])
    rows <- sum(runs$lengths[seq_len(inactive_point)])
    .x[seq_len(rows), ]
  })
}


#' @rdname rm_inactive_dates
#' @export
report_inactive_variables <- function(df, inactivity_period = "1 day", sampling_rate = "1 hour") {
  threshold <- .inactivity_threshold(inactivity_period, sampling_rate)
  inactive <- future_map2(df, names(df), ~ {
    if (any(.inactive_runs(.x[["value"]], threshold)$inactive)) .y
  })
  # Active individuals yield NULL above; drop them before coercion, otherwise
  # as.character() turns each NULL slot into the literal string "NULL".
  as.character(Filter(Negate(is.null), inactive))
}


#' @rdname rm_inactive_dates
#' @export
rm_inactive_variables <- function(df, inactivity_period = "1 day", sampling_rate = "1 hour") {
  inactive <- report_inactive_variables(df, inactivity_period, sampling_rate)
  df[!(names(df) %in% inactive)]
}


# --- internal helpers -------------------------------------------------------

# Number of samples that counts as "inactive" (inactivity_period / sampling_rate).
.inactivity_threshold <- function(inactivity_period, sampling_rate) {
  lubridate::duration(inactivity_period) / lubridate::duration(sampling_rate)
}

# Run-length-encode `values`; return per-run logical (a zero run at least `threshold`
# samples long) together with the run lengths.
.inactive_runs <- function(values, threshold) {
  r <- rle(values)
  list(inactive = r$values == 0 & r$lengths >= threshold, lengths = r$lengths)
}
