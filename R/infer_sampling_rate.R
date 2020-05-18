#Infer sampling rate

#' Infer sampling rate
#'
#' @description Attempts to approximate sampling rate by selecting the sampling mode.
#'
#' @param dates POSIXct vector.
#'
#' @return
#' int or dlb for the sampling mode of the POSIXct vector.
#'
#' @export
#'
#' @examples
#' inferred_rate <- infer_sampling_rate(dates = df$datetime)
infer_sampling_rate <- function(dates = NULL)  {
 sampling_rate <- tibble(date = diff(dates)) %>% group_by(date) %>%
  summarise(n = n()) %>% filter(n == max(n)) %>% pull(date) %>% lubridate::as.period()
 return(sampling_rate)
}


