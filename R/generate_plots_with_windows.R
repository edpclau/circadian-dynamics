#' Function to Generate Plots of the Analyzed Data
#' Must be used when 'Make Time Windows = TRUE'
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
generate_plots_with_windows <- function(trikinetics_analyzed, sampling_rate = 'minutes'){
  #Create a loop over Samples
  purrr::map2(.x = trikinetics_analyzed, .y = names(trikinetics_analyzed),
              .f = ~{
                #Initiate the pdf device
                pdf(paste0('Window_plots_', .y, '.pdf'))

                #Start a counter for each window
                count = 1
                for (window in .x) {
                  #Decide dimensions of the layout
                  #Check for conditional columns
                  butterworth = 'butterworth' %in% names(window$data)
                  detrended = 'detrended' %in% names(window$data)
                  smoothed = 'smoothed' %in% names(window$data)

                  #Check if the Autocorrelation and Lom-Scargle Ran
                  acf_run = !is.na(window$acf$results$period) #Did the autocorrelation run?
                  lomb_run = !is.na(window$lomb$results$period) # Did the lomb-scargle run?


                  #Decide how many rows of plots the page will have
                  n_rows = butterworth + detrended + smoothed + 4

                  #Adjustment for margins when an analysis fails to run
                  l_adjust = -2


                  #Set-up the layout
                  par(mfrow = c(n_rows, 1), mar = c(2,4,1.5,0))


                  ## Start Plotting ##



                  #1. Plot Raw data and place basic window information
                  plot(window$data$datetime, window$data$value,
                       type ='l', xlab = '', ylab = 'Raw Activity')
                  # Top-right text
                  mtext(paste('window', count),
                        side = 3, adj = 0.9)

                  par(mar = c(2,4,0,0))


                  #2. If someone detrended the data, plot the detrended data
                  if (detrended){
                    plot(window$data$datetime, window$data$detrended,
                         type ='l', xlab = '', ylab = 'Detrended Activity')
                  }

                  #3. If the Moving average was used to smooth the data
                  if (smoothed) {
                    plot(window$data$datetime, window$data$smoothed,
                         type ='l', xlab = '', ylab = 'MovAvg Activity')
                  }

                  #4. If the Buttertwoth was used to filter the data
                  if (butterworth) {
                    plot(window$data$datetime, window$data$butterworth,
                         type ='l', xlab = '', ylab = 'Butterworth Activity')
                  }

                  #5. If the autocorrelation was run
                  if (acf_run) {
                    x = as.numeric(duration(seq(0, length(window$data$datetime)-1, 1), sampling_rate), 'hours')
                    plot(x, window$acf$results$autocorrelation,
                         type ='l', xlab = '', ylab = 'Power')

                    ci = 1.965/sqrt(length(x/60))
                    abline(h = ci, col = 'red', lty = 2)
                    abline(h = -ci, col = 'red', lty = 2)

                    text(x = as.numeric(duration(window$acf$results$datetime,sampling_rate), 'hour'),
                         y = window$acf$results$max_peak_of_int,
                         label = '*',
                         col = 'red',
                         cex = 3)

                    text(x = max(x)*0.5,
                         y = max(window$acf$results$autocorrelation)*0.9,
                         label = paste('Autocorrelation:',
                                       'Period =', round(window$acf$results$period, 2), '|',
                                       'Power (R.I.) =', round(window$acf$results$max_peak_of_int, 2)
                         ),
                         col = 'red'
                    )

                  } else {
                    plot.new()
                    mtext('Autocorrelation Not Run', side = 1, line = l_adjust)
                  }


                  #6. If the lomb-scargle was run
                  if (lomb_run) {
                    plot(window$lomb$results$scanned, window$lomb$results$power,
                         type ='l', xlab = '', ylab = 'Power')
                    abline(h = window$lomb$results$sig_level, col= 'blue', lty = 2)
                    text(x = max(window$lomb$results$scanned)*0.605, y = max(window$lomb$results$power)*0.9,
                         label = paste('Lomb-Scargle:',
                                       'Period =', round(window$lomb$results$period, 2), '|',
                                       'Power (R.I.) =', round(window$lomb$results$peak, 2)
                         ),
                         col = 'blue'
                    )

                  } else {
                    plot.new()
                    mtext('Lomb-Scargle Periodogram Not Run', side = 1, line = l_adjust)
                  }


                  #7. Plot the cosinor of both
                  if ((acf_run | lomb_run)){
                    plot(window$data$datetime, window$data[[ncol(window$data)]],
                         type ='l', xlab = '', ylab = '')
                  } else {
                    plot.new()
                    mtext('Cosinor Not Run', side = 1, line = l_adjust)
                  }
                  if (lomb_run){
                    lines(window$data$datetime, window$lomb$cosinor$wave, col = 'blue')
                    text(x = mean(window$data$datetime), y = max(window$data[[ncol(window$data)]])*0.9,
                         label = paste('Lomb-Scargle Cosinor:',
                                       'R^2 =', round(window$lomb$cosinor$adj_r_squared, 4)
                                       # 'Granger =', scales::scientific(window$lomb$results$grangercausal$cos_to_rawdata, 2)
                         ),
                         col = 'blue'
                    )
                  }
                  if (acf_run){
                    lines(window$data$datetime, window$acf$cosinor$wave, col = 'red')
                    text(x = mean(window$data$datetime), y = max(window$data[[ncol(window$data)]])*0.7,
                         label = paste('Acf Cosinor:',
                                       'R^2 =', round(window$acf$cosinor$adj_r_squared, 4)
                                       # 'Granger =', scales::scientific(window$acf$results$grangercausal$cos_to_rawdata, 2)
                         ),
                         col = 'red'
                    )
                  }




                  #Move the counter forward.
                  count = count + 1

                #Close for loop
                }
                dev.off()

              #Close map
              })

#Close function
}
