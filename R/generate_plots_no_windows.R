#' Function to Generate Plots of the Analyzed Data
#' Must be used when 'Make Time Windows = FALSE'
#'
#' @param trikinetics_analyzed
#' @param sampling_rate
#'
#' @return
#' @export
#'
#' @examples
#'
#'
generate_plots_no_windows <- function(trikinetics_analyzed, sampling_rate = 'hours'){

#Initiate the pdf device
pdf('Window_plots.pdf')

#Start a counter for each IND
count = 1
ind_names = names(trikinetics_analyzed)
for (ind in trikinetics_analyzed) {
  #Decide dimensions of the layout
  #Check for conditional columns
  butterworth = 'butterworth' %in% names(ind$data)
  detrended = 'detrended' %in% names(ind$data)
  smoothed = 'smoothed' %in% names(ind$data)

  #Check if the Autocorrelation and Lom-Scargle Ran
  acf_run = !all(is.na(ind$acf$results$autocorrelation)) #Did the autocorrelation run?
  acf_cosinor_run = !is.na(ind$acf$results$period) #Did the autocorrelation produce a period?
  lomb_run = !all(is.na(ind$lomb$results$power)) # Did the lomb-scargle run?
  lomb_cosinor_run = !is.na(ind$lomb$results$period) # Did the lomb-scargle produce a period?


  # Extract some needed information and normalize the activity.
  # Extract some needed information and normalize the activity.
  values = ind$data$value
  norm_values =  (values - min(values)) / (max(values) - min(values))
  norm_values = ifelse(is.nan(norm_values), 0, norm_values)
  control_wave = ind$control$cosinor$wave
  norm_control_wave = (control_wave - min(control_wave)) / (max(control_wave) - min(control_wave))
  norm_control_wave = ifelse(is.nan(norm_control_wave), 0, norm_control_wave)
  if (acf_cosinor_run){
    acf_wave = ind$acf$cosinor$wave
    norm_acf_wave = (acf_wave - min(acf_wave)) / (max(acf_wave) - min(acf_wave))
    norm_acf_wave = ifelse(is.nan(norm_acf_wave), 0, norm_acf_wave)

  }
  if (lomb_cosinor_run){
    lomb_wave = ind$lomb$cosinor$wave
    norm_lomb_wave = (lomb_wave - min(lomb_wave)) / (max(lomb_wave) - min(lomb_wave))
    norm_lomb_wave = ifelse(is.nan(norm_lomb_wave), 0, norm_lomb_wave)

  }



  #Decide how many rows of plots the page will have
  n_rows = butterworth + detrended + smoothed + acf_cosinor_run + lomb_cosinor_run + 4

  #Adjustment for margins when an analysis fails to run
  l_adjust = -2


  #Set-up the layout
  par(mfrow = c(n_rows, 1), mar = c(2,4,1.5,0))


  ## Start Plotting ##



  #1. Plot Raw data and place basic IND information
  plot(ind$data$datetime, norm_values,
       type ='l', xlab = '', ylab = 'Raw Activity')
  # Top-right text
  mtext(paste('IND', ind_names[count]),
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


    #Only add this label if there is no NA.
    if (!is.na(ind$acf$results$datetime)){
    text(x = as.numeric(duration(ind$acf$results$datetime,sampling_rate), 'hour'),
         y = ind$acf$results$max_peak_of_int,
         label = '*',
         col = 'red',
         cex = 3)
    }

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
  plot(ind$data$datetime, norm_values,
       type ='l', xlab = '', ylab = '')
  lines(ind$data$datetime, norm_control_wave, col = 'red')
  text(x = mean(ind$data$datetime), y = max(norm_values)*0.9,
       label = paste('Control 24H Cosinor:',
                     'R^2 =', round(ind$control$cosinor$adj_r_squared, 4)
       ),
       col = 'red'
  )
  if (acf_cosinor_run){
    plot(ind$data$datetime, norm_values,
         type ='l', xlab = '', ylab = '')
    lines(ind$data$datetime, norm_acf_wave, col = 'blue')
    text(x = mean(ind$data$datetime), y = max(norm_values)*0.9,
         label = paste('Acf Cosinor:',
                       'R^2 =', round(ind$acf$cosinor$adj_r_squared, 4), '|',
                       'Granger =', scales::scientific(ind$acf$results$grangercausal$cos_to_rawdata, 2)
         ),
         col = 'blue'
    )
  }
  if (lomb_cosinor_run){
    plot(ind$data$datetime, norm_values,
         type ='l', xlab = '', ylab = '')
    lines(ind$data$datetime, norm_lomb_wave, col = 'blue')
    text(x = mean(ind$data$datetime), y = max(norm_values)*0.9,
         label = paste('Lomb-Scargle Cosinor:',
                       'R^2 =', round(ind$lomb$cosinor$adj_r_squared, 4), '|',
                       'Granger =', scales::scientific(ind$lomb$results$grangercausal$cos_to_rawdata, 2)
         ),
         col = 'blue'
    )
    }





  #Move the counter forward.
  count = count + 1
}
dev.off()
}
