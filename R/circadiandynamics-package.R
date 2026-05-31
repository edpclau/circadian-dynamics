#' @keywords internal
"_PACKAGE"

#' @importFrom stats acf lm median na.omit na.pass var
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics abline axis layout lines mtext par plot.new text
#' @importFrom utils write.csv
NULL

utils::globalVariables(c(
  "datetime", "value", "values", "ld", "window", "auto_power", "id", "ind",
  "data", "median", "na.pass", "UTCTime", "Address", "UID", "ScanCount"
))
