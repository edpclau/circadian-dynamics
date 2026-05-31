#' @keywords internal
"_PACKAGE"

#' @importFrom stats acf lm median na.omit na.pass var time
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics abline axis layout lines mtext par plot.new text
#' @importFrom utils write.csv
#' @importFrom dplyr across any_of
#' @importFrom readr cols
#' @importFrom rstudioapi selectDirectory
NULL

utils::globalVariables(c(
  "datetime", "value", "values", "ld", "window", "auto_power", "id", "ind",
  "data", "median", "na.pass", "UTCTime", "Address", "UID", "ScanCount",
  "Cnts/min", "Hr", "IND", "Lights", "Min", "Time", "amplitude", "dur",
  "gc_cos_to_raw", "gc_raw_to_cos", "max_zeroes", "method", "monitor",
  "name", "phase", "phase_in_seconds", "phase_se_seconds", "raw_values",
  "rhythm_strength", "smoothed", "time", "window_ends", "window_starts"
))
