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
  "data", "Cnts/min", "Hr", "Lights", "Min", "Time", "amplitude",
  "monitor", "name", "phase", "raw_values", "rhythm_strength", "smoothed",
  "time", "UTCTime", "Address", "UID", "ScanCount", "max_zeroes", "IND", "dur"
))
