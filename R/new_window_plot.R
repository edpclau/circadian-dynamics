
pdf('Window_plots.pdf')
count = 1
for (ind in trikinetics_analyzed) {
  #Decide dimensions of the layout
  #Check for conditional columns
  butterworth = 'butterworth' %in% names(ind$data)
  detrended = 'detrended' %in% names(ind$data)
  smoothed = 'smoothed' %in% names(ind$data)

  #Check if the Autocorrelation and Lom-Scargle Ran
  acf_run = !is.na(ind$acf$results$period) #Did the autocorrelation run?
  lomb_run = !is.na(ind$lomb$results$period) # Did the lomb-scargle run?


  #Decide how many rows of plots the page will have
  n_rows = butterworth + detrended + smoothed + 4

  #Adjustment for margins when an analysis fails to run
  l_adjust = -2


  #Set-up the layout
  par(mfrow = c(n_rows, 1), mar = c(2,4,1.5,0))


  ## Start Plotting ##



  #1. Plot Raw data and place basic IND information
  plot(ind$data$datetime, ind$data$value,
       type ='l', xlab = '', ylab = 'Raw Activity')
  # Top-right text
  mtext(paste('IND', count),
        side = 3, adj = 0.9)

  par(mar = c(2,4,0,0))


  #2. If someone detrended the data, plot the detrended data
  if (detrended){
    plot(ind$data$datetime, ind$data$detrended,
         type ='l', xlab = '', ylab = 'Detrended Activity')
  }

  #3. If the Moving average was used to smooth the data
  if (smoothed) {
  plot(ind$data$datetime, ind$data$smoothed,
       type ='l', xlab = '', ylab = 'MovAvg Activity')
  }

  #4. If the Buttertwoth was used to filter the data
  if (butterworth) {
  plot(ind$data$datetime, ind$data$butterworth,
       type ='l', xlab = '', ylab = 'Butterworth Activity')
  }

  #5. If the autocorrelation was run
  if (acf_run) {
    x = as.numeric(duration(seq(0, length(ind$data$datetime)-1, 1), sampling_rate), 'hours')
    plot(x, ind$acf$results$autocorrelation,
         type ='l', xlab = '', ylab = 'Power')

    ci = 1.965/sqrt(length(x/60))
    abline(h = ci, col = 'red', lty = 2)
    abline(h = -ci, col = 'red', lty = 2)

    text(x = as.numeric(duration(ind$acf$results$datetime, 'minutes'), 'hours'),
         y = ind$acf$results$max_peak_of_int,
         label = '*',
         col = 'red',
         cex = 3)

    text(x = max(x)*0.5,
          y = max(ind$acf$results$autocorrelation)*0.9,
          label = paste('Autocorrelation:',
                        'Period =', round(ind$acf$results$period, 2), '|',
                        'Power (R.I.) =', round(ind$acf$results$max_peak_of_int, 2)
                        ),
          col = 'red'
          )

  } else {
    plot.new()
    mtext('Autocorrelation Not Run', side = 1, line = l_adjust)
  }


  #6. If the lomb-scargle was run
  if (lomb_run) {
   plot(ind$lomb$results$scanned, ind$lomb$results$power,
         type ='l', xlab = '', ylab = 'Power')
    abline(h = ind$lomb$results$sig_level, col= 'blue', lty = 2)
    text(x = max(ind$lomb$results$scanned)*0.605, y = max(ind$lomb$results$power)*0.9,
         label = paste('Lomb-Scargle:',
                       'Period =', round(ind$lomb$results$period, 2), '|',
                       'Power (R.I.) =', round(ind$lomb$results$peak, 2)
         ),
         col = 'blue'
    )

  } else {
    plot.new()
    mtext('Lomb-Scargle Periodogram Not Run', side = 1, line = l_adjust)
  }


  #7. Plot the cosinor of both
  if ((acf_run | lomb_run)){
  plot(ind$data$datetime, ind$data[[ncol(ind$data)]],
       type ='l', xlab = '', ylab = '')
  } else {
    plot.new()
    mtext('Cosinor Not Run', side = 1, line = l_adjust)
  }
  if (lomb_run){
    lines(ind$data$datetime, ind$lomb$cosinor$wave, col = 'blue')
    text(x = mean(ind$data$datetime), y = max(ind$data[[ncol(ind$data)]])*0.9,
         label = paste('Lomb-Scargle Cosinor:',
                       'R^2 =', round(ind$lomb$cosinor$adj_r_squared, 4), '|',
                       'Granger =', scales::scientific(ind$lomb$results$grangercausal$cos_to_rawdata, 2)
         ),
         col = 'blue'
    )
    }
  if (acf_run){
    lines(ind$data$datetime, ind$acf$cosinor$wave, col = 'red')
    text(x = mean(ind$data$datetime), y = max(ind$data[[ncol(ind$data)]])*0.7,
         label = paste('Acf Cosinor:',
                       'R^2 =', round(ind$acf$cosinor$adj_r_squared, 4), '|',
                       'Granger =', scales::scientific(ind$acf$results$grangercausal$cos_to_rawdata, 2)
         ),
         col = 'red'
    )
    }




  #Move the counter forward.
  count = count + 1
}
dev.off()
