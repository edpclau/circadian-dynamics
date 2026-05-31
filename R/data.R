#' Example Trikinetics locomotor-activity dataset
#'
#' Drosophila locomotor activity recorded with a Trikinetics DAM system: 32
#' individuals at 1-minute resolution over ~26 days, already parsed into the
#' nested per-individual format expected by [process_timeseries.main].
#'
#' @format A named list of 32 tibbles (one per individual, `IND 1`..`IND 32`),
#'   each with columns:
#' \describe{
#'   \item{datetime}{POSIXct timestamp (1-minute resolution).}
#'   \item{ld}{Light/dark indicator (0 = dark, 1 = light).}
#'   \item{value}{Activity counts in the bin.}
#' }
#' @usage data(trikinetics)
"trikinetics"
