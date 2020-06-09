#' Lomb Scargle Periodogram by window
#'
#' @description Iteratively computes the Lomb-Scargle periodogram for a time series with irregular (or regular) sampling intervals.
#' @usage lsp_by_window(df = NULL, windows = NULL, values = NULL, times = NULL, sampling_rate = NULL, from = NULL, to = NULL,
#' type = c("period", "frequency"), ofac = 1)
#'
#' @param df
#' optional data.frame with 2 or 3 columns. If the data.frame has 2 columns, column 1 contains the windows, column 2
#' contains the measurement values. If the data.frame has 3 columns, column 1 contains windows, column 2 is POSIXct object,
#' and column 3 is a measurement values.
#'
#' @param windows
#' A vector containing the windows to iterate over and to label the group to which each value belongs.
#' Usually this will be the output from make_time_windows. Optional if df is provided.
#'
#' @param values The data to which we want to find the period. Optional if df is provided.
#' @param times A POSIXct vector. Optional if df is provided.
#' @param sampling_rate
#' A character string indicating the sampling rate of the data.
#' Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @param from
#' An optional numeric indicating from which period or frequency to start looking for peaks.
#' Must be in the same units as the sampling rate.
#' Examples: If the goal is to evaluate a 18 "hour" period or frequency but the sampling rate is "30 minutes",
#' the period to use is a  36 "30 minutes" period or frequency.
#'
#' @param to
#' An optional numeric indicating up to which period or frequency to start looking for peaks.
#'  Must be in the same units as the sampling rate.
#' Examples: If the goal is to evaluate a 28 "hour" period or frequency but the sampling rate is "30 minutes",
#' the period to use is a  56 "30 minutes" period or frequency.
#'
#' @param type
#' Either “frequency” (the default) or “period”. Determines the type of the periodogram x-axis [lomb::lsp()].
#'
#' @param ofac
#' [lomb::lsp()] The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
#'
#' @return
#' A data.frame with the following components:
#' window           A vector containing the window to which the analysis corresponds.
#' period           The period of the timeseries. Outputted in the same sampling rate as the data.
#' power            The maximum power in the frequency/period interval inspected.
#' lsp_p_value      The probability that the maximum peak occurred by chance.
#' scanned          A vector containing the frequencies/period scanned.
#' normalized_power A vector containing the normalized power corresponding to scanned frequencies/periods.
#' sig_level        Powers > sig.level can considered significant peaks.
#'
#' @export
#'
#' @examples
#' lsp_analysis <- lsp_by_window(df = processed_data, sampling_rate = "30 min")
#'
#'
#' @importFrom purrr map_if map_df discard
#' @importFrom rlang is_empty
#' @importFrom tibble tibble
#' @import lubridate
lsp_by_window <- function (df = NULL, windows = NULL, values = NULL, times = NULL, sampling_rate = NULL, from = NULL, to = NULL,
                          type = c("period", "frequency"), ofac = 60, alpha = 0.01, plot = FALSE) {

  ###### Flow control parameters######
  #1. Either a df or 3 lists of: (1) windows, (2) values, and (3) times must be supplied. If a df is not supplied, turn the
  # 3 lists into a df.
  if (is.null(df) & (is.null(times) | is.null(values))) {
    stop("If a data.frame is not supplied. Must include both windows and values.")
  } else if (is.null(df) & is.null(times)) { df = tibble::tibble(windows, values)
  } else if (is.null(df) & !is.null(times)) { df = tibble::tibble(windows, times, values)
  }

  #2.must have sampling_rate
  if (is.null(sampling_rate) & !lubridate::is.POSIXct(times)) {
    stop("Must include sampling_rate. ex. '30 min', '1 hour', '4 seconds', '100 days'.")
    }
  #3. Change the names of the df columns so we can work with it inside the function
  if (!is.null(df) & ncol(df) < 2) {
    stop("data frame must include windows and values")
    } else if (!is.null(df) & ncol(df) == 2) {
    names(df) <- c("window", "values")
    df$times <- NA
    } else if (!is.null(df) & ncol(df) == 3 ) {
      names(df) <- c("window", "times", "values")
    } else {
    stop("data frame can contain at most 3 columns.")
      }
  #4. You can choose between period or frequency for "Type". Period is the default.
  type <- base::match.arg(type, choices = c("period", "frequency"))
  #5. The from - to interval should be in seconds whenever a datetime is given as times
  if (!is.null(times) | !all(is.na(df$times))) {
    warning("the from - to value must be in seconds whenever a datetime object is used as times. Otherwise the function may run infinitely.",
            immediate. = TRUE) }
warning("A from - to interval that is not in the same units as the data or is too small, may cause the function to hit the time limit.")
#### Lomb Scargle periodogram by time windows #####

# If there is no times, run without times, otherwise use times.
if (!all(is.na(df$times))) {

lomb_scargle <- {
  setTimeLimit(300, transient = TRUE)
  map_if(unique(df$window),
              .p = ~ sum(!is.na(pull(filter(df, window == .), values))) >= 2,
              .f =  ~ lsp_mod(x = select(filter(df, window == .), times, values),
                              from = from, to = to, ofac = ofac, type = type, alpha = alpha, plot = plot),
              .else = ~ NULL)
}
} else {
  lomb_scargle <- {
    setTimeLimit(300, transient = TRUE)
    map_if(unique(df$window),
                                .p = ~ sum(!is.na(pull(filter(df, window == .), values))) >= 2,
                                .f =  ~ lsp_mod(x = pull(filter(df, window == .), values), from = from,
                                                to = to, ofac = ofac, type = type, alpha = alpha, plot = plot),
                                .else = ~ NULL)
  }
}


# Remove the NULL windows
names(lomb_scargle) <- seq_along(1:length(lomb_scargle))
lomb_scargle_no_null <- discard(lomb_scargle, is_empty)


# Prepare a tibble with the relevant results. These will allow for running a COSINOR analysis.
results <- map_df(names(lomb_scargle_no_null),
           .f = ~ tibble(window = as.numeric(.),
                       period = if (!is.null(from) | !is.null(to)) {
                         lomb_scargle_no_null[[.]]$peak.at[1]
                       } else if (is.POSIXct(df$times) | is.POSIXct(times)) {
                         dseconds(lomb_scargle_no_null[[.]]$peak.at[1])/duration(sampling_rate)
                         } else {
                         lomb_scargle_no_null[[.]]$peak.at[1]
                         } ,
                       power = lomb_scargle_no_null[[.]]$peak,
                       lsp_p_value = lomb_scargle_no_null[[.]]$p.value,
                       scanned = list(lomb_scargle_no_null[[.]]$scanned),
                       normalized_power = list(lomb_scargle_no_null[[.]]$power),
                       sig_level = lomb_scargle_no_null[[.]]$sig.level)
           )

results
return(results)
}
