#' Fill Gaps in the data
#'
#' @description Finds and fills gaps in a POSIXct vector.
#' @usage find_gaps(times = NULL, sampling_rate = NULL)
#' @param times a POSIXct vector.
#' @param sampling_rate A character string indicating the sampling rate of the data. Examples: '30 minutes', '1 hour', '4 seconds', '100 days'.
#'
#' @return
#' POSIXct vector without gaps in the dates.
#' @export
#'
#' @examples
#' completed_dates <- find_gaps(times = df$datetime, sampling_rate = "30 min")
#'
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @importFrom magrittr "%>%"
find_gaps <- function(times = NULL, sampling_rate = NULL) {

  completed_dates <- tibble::tibble(datetime = times) %>%
    tidyr::complete(datetime = seq(min(datetime),max(datetime), by = lubridate::as.duration(sampling_rate)))

  return(completed_dates)
}
