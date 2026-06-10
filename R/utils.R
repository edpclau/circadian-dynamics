# Internal, non-exported helpers shared across the package.

# Split a sampling-rate string like "30 minutes" into its numeric bin size (30) and
# unit ("minutes"). Previously duplicated verbatim in analyze_lomb/_acf/_cosinor.
#' @importFrom stringr str_extract str_remove
#' @noRd
.parse_sampling_rate <- function(sampling_rate) {
  list(
    bin  = as.numeric(str_extract(sampling_rate, "\\d*")),
    unit = str_remove(sampling_rate, "\\d* *")
  )
}
