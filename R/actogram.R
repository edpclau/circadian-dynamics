#sampling in minutes
#' Actogram
#'
#' @param df A data.frame where the first column is datetime and the rest are individual measurement values.
#' @param sampling Sampling rate in minutes.
#' @param file A character string for the output PDF filename (without extension). Default is \code{"actogram"}.
#'
#' @return Invisibly saves a PDF actogram file named \code{file}.pdf to the working directory.
#' @export
actogram <- function (df, sampling, file = 'actogram') {

  df = dplyr::filter(df, datetime >= lubridate::round_date(min(datetime), unit = 'day'))


  #Set sampling rate and date search
  inds = ncol(df)
  cols = round(log(inds))
  rows = round(inds/3)
  search_factor = 60/sampling
  dday = search_factor * 24
  # Number of calendar days spanned. The previous yday()-based arithmetic
  # under-counted by one within a year and went negative across a year boundary
  # (Dec -> Jan), corrupting the layout. difftime is non-negative and wrap-safe.
  days = as.integer(ceiling(as.numeric(difftime(max(df$datetime), min(df$datetime), units = 'days'))))
  day = seq_len(days)


  #Set matrix layout for the plots
  layout.matrix <- matrix(seq_len(days + 1), nrow = days + 1, ncol = 1)

  pdf(file = paste0(file,'.pdf'))
  # Close the PDF device even if the loop below errors, so a partial/corrupt file
  # is not left open and the graphics device stack is not polluted.
  on.exit(grDevices::dev.off(), add = TRUE)

  layout(mat = layout.matrix,
         heights = c(rep(1, days), 3), # Heights of the two rows
         widths = c(1)) # Widths of the two columns

  # layout.show(days+1)

  vals <- df[-1]
  for (ind in seq_len(ncol(vals))) {

    #Actogram
    par(mar = c(0,0,0,0))
    for (i in day) {

      slice = seq(1+dday*(i - 1), dday*(i+1), by = 1)

      hour = seq(1, 48, by = (48 - 1) / (length(slice) - 1))

      activity = vals[[ind]][slice]
      mact = max(activity)
      if (is.na(mact) || mact == 0) mact = 1



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

}





