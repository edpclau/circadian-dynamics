#' Lomb Scargle Periodogram by window
#'
#' @description Iteratively computes the Lomb-Scargle periodogram for a time series with irregular (or regular) sampling intervals.
#' @usage
#' lsp_by_window(df = NULL, windows = NULL, values = NULL,
#' times = NULL, sampling_rate = NULL,
#' from = 18, to = 30, type = c("period", "frequency"),
#' ofac = 60, alpha = 0.01, plot = FALSE)
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
#' Must be in hours. Default = 18.
#'
#' @param to
#' An optional numeric indicating up to which period or frequency to start looking for peaks.
#' Must be in hours. Default = 30.
#'
#' @param type
#' Either “frequency” or “period” (default). Determines the type of the periodogram x-axis [lomb::lsp()].
#'
#' @param ofac
#' [lomb::lsp()] The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
#'
#' @return
#' A data.frame with the following components:
#'
#' window           A vector containing the window to which the analysis corresponds.
#'
#' period           The period of the timeseries. Outputted in the same sampling rate as the data.
#'
#' power            The maximum power in the frequency/period interval inspected.
#'
#' lsp_p_value      The probability that the maximum peak occurred by chance.
#'
#' scanned          A vector containing the frequencies/period scanned.
#'
#' normalized_power A vector containing the normalized power corresponding to scanned frequencies/periods.
#'
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
#' @importFrom lubridate is.POSIXt is.POSIXct duration
#' @importFrom dplyr pull filter
#'
lsp_by_window <- function (df = NULL, windows = NULL, values = NULL, times = NULL, sampling_rate = NULL, from = 18, to = 30,
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
  type <- match.arg(type, choices = c("period", "frequency"))

  #5. If there is 0 variance in the data, skip window.
  good_windows <- purrr::map_dbl(unique(df$window),
                                 .f = ~ ifelse(var(dplyr::pull(dplyr::filter(df, window == .), values), na.rm = TRUE) == 0,
                                               NA,
                                               .
                                 )
  )

  #6. If all windows have 0 variance, return NA.
  if ( all(is.na(good_windows)) ) {
    results <- tibble(window = unique(df$window),
                                    period = NA ,
                                    power = NA,
                                    lsp_p_value = NA,
                                    normalized_power = NA,
                                    sig_level = NA,
                                    ofac = NA)

    results <- dplyr::arrange(results, window)

    return(results)

  }
  good_windows <- na.omit(good_windows)

 #7. Sampling_rate
  sampling_bin_size = as.numeric(stringr::str_extract(sampling_rate, "\\d*"))
  sampling_rate = stringr::str_remove(sampling_rate, "\\d* *")

  #8. Plan for paralellization
  future::plan(future::multisession)
#### Lomb Scargle periodogram by time windows #####

# Convert from-to to seconds.
if (!is.null(from)) {
from <- as.numeric(duration(from, units = "hours"), sampling_rate)/ sampling_bin_size
}

if (!is.null(to)) {
to  <- as.numeric(duration(to, units = "hours"), sampling_rate) / sampling_bin_size
}

# Calculate Lomb-scargle periodogram
lomb_scargle <- {

  furrr::future_map_if(good_windows,
              .p = ~ sum(!is.na(pull(filter(df, window == .), values))) >= 2,
              .f =  ~ lsp_mod(x = c(pull(filter(df, window == .), values)),
                             from = from, to = to,  ofac = ofac, type = type, alpha = alpha, plot = plot),
              .else = ~ NULL)
}

if ( !(is.null(from) & is.null(to)) ) {
lomb_scargle_full <- {

  furrr::future_map_if(good_windows,
         .p = ~ sum(!is.na(pull(filter(df, window == .), values))) >= 2,
         .f =  ~ lsp_mod(x = c(pull(filter(df, window == .), values)),
                          ofac = ofac, type = type, plot = FALSE, alpha = alpha),
         .else = ~ NULL)
}
names(lomb_scargle_full) <- good_windows
lomb_scargle_full_no_null <- purrr::discard(lomb_scargle_full, is_empty)
}

# Remove the NULL windows
names(lomb_scargle) <- good_windows
lomb_scargle_no_null <- purrr::discard(lomb_scargle, is_empty)



# Prepare a tibble with the relevant results. These will allow for running a COSINOR analysis.
results <- furrr::future_map_dfr(1:length(lomb_scargle_full_no_null),
           .f = ~ tibble(window = good_windows[.],
                       period = as.numeric(duration(lomb_scargle_no_null[[.]]$peak.at[1] * sampling_bin_size, sampling_rate), "hours"),
                       power = lomb_scargle_no_null[[.]]$peak,
                       lsp_p_value = lomb_scargle_no_null[[.]]$p.value,
                       scanned = list(as.numeric(duration(lomb_scargle_full_no_null[[.]]$scanned * sampling_bin_size, sampling_rate), "hours")),
                       normalized_power = list(lomb_scargle_full_no_null[[.]]$power),
                       sig_level = lomb_scargle_no_null[[.]]$sig.level,
                       ofac = lomb_scargle_no_null[[.]]$ofac)
           )
results <- dplyr::bind_rows(results,
                            tibble::tibble(window = unique(df$window)[!(unique(df$window) %in% good_windows)]))
results <- dplyr::arrange(results, window)

return(results)
}
