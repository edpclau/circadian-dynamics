#' Define an Event for the USDA RFID data
#'
#' @param df data.frame of RFID data containing at least the following columns: UID, UTCTime, Address, ScanCount.
#' @param core boolean. If TRUE (default), will run paralelization using the multicore approach. This doesn't work well on Windows 10
#' systems. If FALSE, it will use the multisession approach.
#'
#' @return returns a data.frame with an additional column called "event".
#'
#' @export define_event
#'
#' @examples define_event(df)
#'
#' @importFrom future plan multisession
#' @importFrom dplyr select mutate pull arrange case_when
#' @importFrom furrr future_map_if
#' @importFrom purrr map_chr
#' @importFrom tidyr nest unnest
#' @import magrittr
#'
#' @description This function takes data from a specific USDA project. It's not meant to be used with any other datasets.
#'
#' @author Eddie Perez Claudio
#'
#'
#'
#'
define_event <- function(df, core = TRUE) {
  # Make sure the date has been parsed as datetime.
  if (!is.POSIXct(df$UTCTime)) {
    df$UTCTime = mdy_hms(df$UTCTime)
  }

  # Select the columns of interest
  df = select(df,
              UTCTime,
              Address,
              UID,
              ScanCount)


  # Nest the data by Individual ID to ease iteration
  df = nest(df, data = -UID)

  # Plan for paralelization. If not running on a window's computer, it can be set as multicore.
  if (core) {
    plan(multicore)
  } else { plan(multisession) }


  # Building the events
  df = df %>%
    mutate(data = future_map_if( #paralelized version of map_if. It will run only when there is one than one timepoint in the data.
      .x = data,
      .p = ~ nrow(.x) > 1, #if statement
      .f = ~ {
        #Extracting columns to facilitate the iterations
        arranged = arrange(., UTCTime)
        difftime = arranged %>% pull(UTCTime) %>% diff(units = 'secs')
        address = arranged %>% pull(Address)

        #Defining the scope of the iterations. It will be 1 shorter than the data.frame.
        iter = seq(1, length(address) - 1)
        strict_event = c()
        strict_event = map_chr(.x = iter,
                               .f = ~ {
                                 #Here we degine the events. We call it strict event since anything longer than 5 seconds
                                 #will not be considered an event and be a timeout.
                                 #Undefined is any other case not covered in timeout, in, or out.
                                 strict_event[.x] = case_when(
                                   difftime[.x] > 5 ~ 'timeout',
                                   address[.x] == 1 && address[.x + 1] == 2  ~ 'out',
                                   address[.x] == 4 && address[.x + 1] == 3 ~ 'out',
                                   address[.x] == 5 && address[.x + 1] == 7 ~ 'out',
                                   address[.x] == 8 && address[.x + 1] == 6 ~ 'out',
                                   address[.x + 1] == 1 && address[.x] == 2 ~ 'in',
                                   address[.x + 1] == 4 && address[.x] == 3 ~ 'in',
                                   address[.x + 1] == 5 && address[.x] == 7 ~ 'in',
                                   address[.x + 1] == 8 && address[.x] == 6 ~ 'in',
                                   TRUE ~ 'undefined'
                                 )
                               })
        # We add an NA at the start of strict_event to make it the same length as the data.frame
        arranged %>% mutate(event =  c(NA, strict_event))
      },
      .else = ~ .x
    ))

  # Prepare the data to be returned. We don't need the Address anymore.
  df = unnest(df, data) %>% select(-Address)

  return(df)
}



