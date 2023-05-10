#sampling in minutes
#' Actogram
#'
#' @param df
#' @param sampling
#' @param file
#'
#' @return
#' @export
#'
#' @examples
actogram <- function (df, sampling, file = 'actogram') {

  df = dplyr::filter(df, datetime >= lubridate::round_date(min(datetime), unit = 'day'))


  #Set sampling rate and date search
  inds = ncol(df)
  cols = round(log(inds))
  rows = round(inds/3)
  search_factor = 60/sampling
  dday = search_factor * 24
  days = sum(diff(lubridate::yday(df$datetime) - min(lubridate::yday(df$datetime))))
  day = seq(1, days, by = 1)


  #Set matrix layout for the plots
  layout.matrix <- matrix(c(seq(1, days+1, by = 1)), nrow = (days+1), ncol = 1)

  pdf(file = paste0(file,'.pdf'))

  layout(mat = layout.matrix,
         heights = c(rep(1, days), 3), # Heights of the two rows
         widths = c(1)) # Widths of the two columns

  # layout.show(days+1)

  for (ind in seq(1, ncol(df[-1]), by = 1)) {

    #Actogram
    par(mar = c(0,0,0,0))
    for (i in day) {

      slice = seq(1+dday*(i - 1), dday*(i+1), by = 1)

      hour = lubridate::hour(df[[1]][slice])
      hour = seq(1, 48, by = (48-1)/(length(hour) - 1))


      activity = df[-1][[ind]][slice]
      mact = max(activity)
      mact = ifelse( mact == 0 | is.na(mact) , 1, mact )



      plot(x = hour, y = activity, ylim = c(0,mact), type = 'h', bty = "n", yaxt = "n", ann = F, xlim = c(0,49), xaxt = 'n')
      # axis(1, at = seq(1, 48, by = 1), gap.axis = 0)
      abline(v = 23.99, col = 'blue')
      mtext(text = paste(i), side = 2, line = -1.5)

    }

    plot(NULL, ylim = c(0,mact), bty = "n", yaxt = "n", xaxt = 'n', ann = F, xlim = c(0,49))
    axis(1, at = seq(0, 48, by = 1), gap.axis = 2, line = -5)
    mtext('Hours', side = 1, line = -2, font = 2)
    mtext(paste('IND:', names(df)[ind+1], '| Sampling Rate:', sampling, 'minute', '| Start:', min(df$datetime), '| End:', max(df$datetime)), side = 1, line = -1, font = 2)


  }


  dev.off()

}





