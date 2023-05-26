#' Utility Function for Plotting Detaiiled data in Windows for the App
#'
#' @param df the output from 'simplify_data'.
#'
#' @return a list of plots.
#' @export
#'
#' @examples window_plots = plot_window_data_app(df)
#' @import magrittr
#' @import ggplot2
#' @importFrom lubridate duration
#' @importFrom tidyr nest unnest
#' @importFrom ggpp geom_label_npc
#' @importFrom furrr future_map
#' @importFrom future plan sequential
plot_window_data_app <- function(df) {


  #Handle the case when there are no windows in the data.
  if (!'window' %in% names(df$data)) {df$data$window = 1}
  if (!'window' %in% names(df$utils)) {df$utils$window = 1}
  raw = df$data
  utils = df$utils

  plot_df = left_join(raw, utils)

  #Check for conditional columns
  butterworth = 'butterworth' %in% names(raw)
  detrended = 'detrended' %in% names(raw)
  smoothed = 'smoothed' %in% names(raw)

  #Make plots for all individuals by window
  #These are the most detailed plots and will take a lot of time to process
  #Here we are doing a double for loop
  map(
    .x = unique(plot_df$data), #Iterate over individuals
    .f = ~ {
      #At this step we are iterating over all the windows for each individual/measurement
      #plots are arranged in the order in which they should appear on the page.
      ind = filter(plot_df, data == .x)



      pdf(file = paste0(.x,'.pdf'))
      map(
        .x = unique(ind$window), #iterate over windows
        .f = ~{

          window_data = filter(ind, window == .x)

          #Decide which plots we are going to run
          acf_run = !all(is.na(window_data$acf_period)) #Did the autocorrelation run?
          lsp_run = !all(is.na(window_data$lsp_period))


          #Set the plotting layout
          num_rows = 2 + acf_run + lsp_run + detrended + butterworth + smoothed

          print(num_rows)

          par(mfcol = c(num_rows, 1))





          #### Plot Raw Data ####
          ggplot(data = window_data, aes(x = datetime, y = raw_values)) +
            geom_line() +
            labs(y = 'Raw Data', x = 'Datetime', title = paste('Window ', .x))

          #### Autocorrelation Plot #######
          if (acf_run) {
            ggplot(window_data, aes(x= datetime, y = acf)) +
              geom_line() +
              geom_vline(aes(xintercept = min(datetime) + acf_start), lty = 'dashed', color = 'blue') +
              geom_vline(aes(xintercept = min(datetime) + acf_end), lty = 'dashed', color = 'blue') +
              geom_point(aes(x = acf_peak_time - duration('1.25 hour'), y = acf_peak, colour = 'Peak of Interest'), size = 2)+
              geom_label_npc(aes(npcx = 1 , npcy = 0.92, label = paste('Period =', acf_period,
                                                                       '\nR.S. =', round(acf_rs, digits = 3),
                                                                       '\nPeak =', round(acf_peak, digits = 3),
                                                                       '\nPeak Time = ', acf_peak_time))) +
              scale_colour_manual(values = 'red', name = '') +
              labs(y = 'Coefficient', x = 'Datetime', title = 'Autocorrelation Coefficient Through Time') +
              theme(legend.position = c(0.88,0.99))
          }

          #### Lomb-Scargle Plot ####
          if (lsp_run) {
            lsp = unnest(select(window_data, lsp_power, lsp_scanned), cols = c(lsp_power, lsp_scanned))

            ggplot(lsp, aes(x = lsp_scanned, y = lsp_power)) +
              geom_line() +
              geom_point(data = window_data, aes(x = lsp_period, y = lsp_peak, colour = 'Peak of Interest')) +
              geom_hline(data = window_data, aes(yintercept = lsp_sig_level), lty = 'dashed') +
              geom_label_npc(data = window_data, aes(npcx = 1, npcy = 0, label = paste('Period =', round(lsp_period, digits = 3),
                                                                              '\nRhythm Strength =', round(lsp_rs, digits = 3),
                                                                              '\nP-value =', round(lsp_p_value, digits = 4),
                                                                              '\nPower =', round(lsp_peak, digits = 3)))) +
              geom_vline(data = window_data, aes(xintercept = acf_from), lty = 'dashed', color = 'blue') +
              geom_vline(data= window_data, aes(xintercept = acf_to), lty = 'dashed', color = 'blue') +
              scale_colour_manual(values = 'red', name = '') +
              labs(y = 'Power', x = 'Period', title = 'Lomb-Scargle Periodogram') +
              theme(legend.position = c(0.125,1))
          }

          ### COSINOR PLOTS ####
          ggplot(data = window_data, aes(x = datetime, y = raw_values)) +
            geom_line() +
            geom_line(aes(y = lomb_cosinor, x = datetime,  colour = 'Lomb-Scargle Periodogram')) +
            # geom_line(aes(y = autocorr_cosinor, x = datetime, colour = 'Autocorrelation')) +
            geom_label_npc(aes(npcx = 1, npcy = 1, label = paste(
              'Period =', round(lsp_period, digits = 3),
              '\nPhase = ', round(lsp_phase, digits = 3),
              '\nAmplitude = ', round(lsp_amp, digits = 3),
              '\nRhythm Strength =', round(lsp_rs, digits = 4),
              '\nPercent Rhytm = ', round(lsp_pr, digits = 4)*100,
              '\nGranger Test = ', round(lsp_gc, digits = 5))
            )
            ) +
            scale_colour_manual(values = c('red', 'blue')) +
            labs(y = 'Raw Values', x = 'Datetime', title = paste('Cosinor Fit for Window ', .x)) +
            theme(
              legend.position = 'none'
            )





          ### BUTTERWORTH ####
          if (butterworth) {
            ggplot(data = window_data, aes(x = datetime, y = butterworth)) +
              geom_line() +
              labs(y = '', x = 'Datetime', title = 'Butterworth Filtered Data')
          }
          ### Detrended ###
          if (detrended) {
            ggplot(data = window_data, aes(x = datetime, y = detrended)) +
              geom_line() +
              labs(y = '', x = 'Datetime', title = 'Detrended Data')
          }
          ### Moving Average ###
          if (smoothed) {
            ggplot(data = window_data, aes(x = datetime, y = smoothed)) +
              geom_line() +
              labs(y = '', x = 'Datetime', title = 'Movavg Data')
          }



        }
      )

      dev.off()





    }
  )




}
