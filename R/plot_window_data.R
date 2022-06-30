#' Utility Function for Plotting Detaiiled data in Windows
#'
#' @param df the output from 'simplify_data'.
#'
#' @return a list of plots.
#' @export
#'
#' @examples window_plots = plot_window_data(df)
#' @import magrittr
#' @import ggplot2
#' @importFrom lubridate duration
#' @importFrom tidyr nest unnest
#' @importFrom ggpp geom_label_npc
#' @importFrom furrr future_map
#' @importFrom future plan sequential
plot_window_data <- function(df) {


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

plot_df = plot_df %>% nest(window_data= -c(data, window)) %>%
  nest(unique_measure = -data)

plan(sequential)
#Make plots for all individuals by window
#These are the most detailed plots and will take a lot of time to process
#Here we are doing a double for loop
p = future_map(
  .x = plot_df$unique_measure,
  .f = ~ {
    #At this step we are iterating over all the windows for each individual/measurement
    #plots are arranged in the order in which they should appear on the page.
    window_plots = future_map2(
      .x = .x$window_data,
      .y = .x$window,
      .f = ~{

        raw_plots = ggplot(data = .x, aes(x = datetime, y = raw_values)) +
          geom_line() +
          labs(y = 'Raw Data', x = 'Datetime', title = paste('Window ', .y))

    #### Autocorrelation Plot #######
       #Did the autocorrelation run?
        acf_run = !all(is.na(.x$acf_period))
        if (acf_run) {
        acf_plots =  ggplot(.x, aes(x= datetime, y = acf)) +
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
        lsp_run = !all(is.na(.x$lsp_period))
        if (lsp_run) {
        lsp = unnest(distinct(select(.x, lsp_power, lsp_scanned)), cols = c(lsp_power, lsp_scanned))
        lsp_plots = ggplot(lsp, aes(x = lsp_scanned, y = lsp_power)) +
          geom_line() +
          geom_point(data = .x, aes(x = lsp_period, y = lsp_peak, colour = 'Peak of Interest')) +
          geom_hline(data = .x, aes(yintercept = lsp_sig_level), lty = 'dashed') +
          geom_label_npc(data = .x, aes(npcx = 1, npcy = 0, label = paste('Period =', round(lsp_period, digits = 3),
                                                                                                            '\nRhythm Strength =', round(lsp_rs, digits = 3),
                                                                                                            '\nP-value =', round(lsp_p_value, digits = 3),
                                                                                                            '\nPower =', round(lsp_peak, digits = 3)))) +
          geom_vline(data = .x, aes(xintercept = acf_from), lty = 'dashed', color = 'blue') +
          geom_vline(data = .x, aes(xintercept = acf_to), lty = 'dashed', color = 'blue') +
          scale_colour_manual(values = 'red', name = '') +
          labs(y = 'Power', x = 'Period', title = 'Lomb-Scargle Periodogram') +
          theme(legend.position = c(0.125,1))
        }

        ### COSINOR PLOTS ####
        cosinor_plots = ggplot(data = .x, aes(x = datetime, y = raw_values)) +
          geom_line() +
          geom_line(aes(y = lomb_cosinor, x = datetime,  colour = 'Lomb-Scargle')) +
          geom_line(aes(y = autocorr_cosinor, x = datetime, colour = 'Autocorrelation')) +
          geom_label_npc(aes(npcx = 1, npcy = 1, label = paste(
                                                               'AutoCorr Period =', round(acf_period, digits = 3),
                                                               '\nAutoCorr Phase = ', round(acf_phase, digits = 3),
                                                               '\nLSP Period =', round(lsp_period, digits = 3),
                                                               '\nLSP Phase = ', round(lsp_phase, digits = 3))
                             )
                         ) +
          scale_colour_manual(values = c('red', 'blue'), name = 'Fitted with:') +
          labs(y = 'Raw Values', x = 'Datetime', title = 'Cosinor Fit') +
          theme(
            legend.position = 'top'
          )

      ### BUTTERWORTH ####
       if (butterworth) {
         butterworth_plots = ggplot(data = .x, aes(x = datetime, y = butterworth)) +
           geom_line() +
           labs(y = '', x = 'Datetime', title = 'Butterworth Filtered Data')
       }
      ### Detrended ###
       if (detrended) {
         detrended_plots = ggplot(data = .x, aes(x = datetime, y = detrended)) +
           geom_line() +
           labs(y = '', x = 'Datetime', title = 'Detrended Data')
       }
          ### Moving Average ###
       if (smoothed) {
         movavg_plots = ggplot(data = .x, aes(x = datetime, y = smoothed)) +
           geom_line() +
           labs(y = '', x = 'Datetime', title = 'Movavg Data')
       }

       plots = list(
         raw_plots = raw_plots,
         detrended_plots = if (detrended) {detrended_plots},
         butterworth_plots = if (butterworth) {butterworth_plots},
         movavg_plots = if (smoothed) {movavg_plots},
         acf_plots = if (acf_run) {acf_plots},
         lsp_plots = if (lsp_run) {lsp_plots},
         cosinor_plots = cosinor_plots
       )

       return(plots[!sapply(plots, is.null)])


        }
    )
    names(window_plots) = .x$window

    return(window_plots)
    }
)
names(p) = plot_df$data

return(p)

}
