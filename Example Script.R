# 1. Load Library
library(circadiandynamics)
library(gridExtra)

# 2. Choose file
file = file.choose()

# 3. Depending on the file choose your import function
###
## The general import function is 'read_csv_data'. It requires that
## your file is in .csv format. The first column must be the datetime column.
## The second column should be your Light/Dark data, if you have any.
## All other columns will be the signals/individuals you want to analyze.
###
## If you're going to import a trikinetics file use 'read_trikinetics_2'.
## In this example we use a trikinetics file.
trikinetics = read_trikinetics_2(file)
##

# 4. (OPTIONAL) Downsample your data
## Here you can downsample your daset if it is too large.
## Usually, we downsample it to 1 hour bins.
## This is set using the amount and units arguments.
# Since this is activity data, we will sum it over each 1 hour period.
trikinetics_downsampled = downsample_time_series_2(trikinetics, amount = 1,
                                                   units = 'hour', method = 'sum')
rm(trikinetics) ## Memory Management (remove variables we won't use again)




## (Optional) Truncate data


# 5. Rhythm Analysis
trikinetics_analyzed = process_timeseries.main(
  df = trikinetics_downsampled[15],

  # Window of Analysis Arguments
  make_windows = TRUE,
  window_size_in_days = 4,
  window_step_in_days = 1,

  # Period in which to look for peaks within each window (in hours)
  from = 18,
  to = 30,

  #The function needs to know the sampling rate of your data.
  #In this case, we downsampled the data to 1 hour.
  sampling_rate = '1 hour',

  # Should the data be detrended before analysis?
  detrend_data = FALSE,

  #Which, if any, smoothing method you want to use on your data?
  #Moving average or Two-pass Butterworth filter?
  #Only one should be TRUE at a time. If both are selected as TRUE,
  #the system will default to butterworth = TRUE.
  movavg = FALSE,
  butterworth = TRUE,
  #If butterworth is TRUE, specify the frequency or period (1/frequency) that
  #you're interested in filtering out.
  #f_high is the high pass filter.
  #f_low is the low pass filter.
  f_low = 1/4,
  f_high = 1/73,
  #Order for the butterworth filter
  order = 2,

  #As a novel test of rhythmicity, we are using the Granger test of
  #causality. It tests wether the Cosinor fit we create, causally predicts
  #changes in the raw data and vice versa. The idea is that if an
  #individual or signal is rhythmic during a given window, it should
  #present a causal relationship between the cosinor and raw data.
  #causal_order is the lag up to which we will consider that a timeseries is
  #being caused.
  causal_order = 1,


  #If you're going to be working with big data, make this argument TRUE.
  #Beware, if your dataset is small, setting this argument to TRUE will make
  #it run slower as there is an overhead to paralleling the analysis.
  big_data = FALSE
)

rm(trikinetics_downsampled) ## Memory Management (remove variables we won't use again)

# 6. Tidy up data for export
## The data outputted by 'process_timeseries.main) is not easily read
##by humans. Therefore, we have deviced a function that arranges the data
## into 3 data.frames that are easy to export and read.
trikinetics_tidy = simplify_data(trikinetics_analyzed)

rm(trikinetics_analyzed) ## Memory Management (remove variables we won't use again)

# 7. Prepare figures
## 7.1 Prepare actograms
### We want to use the first data.frame in the output of 'simplify_data'.
actograms = plot_actogram2(trikinetics_tidy$data)
actograms_by_window = plot_actogram_windows(trikinetics_tidy$data)
## 7.2 Raw data plots
raw_plots = plot_raw_values(trikinetics_tidy$data)
## 7.3 Plot Autocorrelation Results
acf_plots = plot_acf_results(trikinetics_tidy$autocorrelation)
## 7.4 Plot Lomb-Scargle Results
lsp_plots = plot_lsp_results(trikinetics_tidy$lombscargle)
## 7.5 Window level plots.
window_plots = plot_window_data(trikinetics_tidy, alt_cos = TRUE)

# 8.Arrange the Figures for export
## 8.1 Arrange the Actograms
p = arrangeGrob(grobs = actograms)
nwindows = dplyr::n_distinct(actograms[[1]]$data$window)
nplots = length(actograms)
height_ = nwindows/log10(nwindows)
width_ = nplots/log10(nplots+1)

ggsave('actograms.pdf', p,
       height = ifelse(height_ > 10, height_, 10),
       width = ifelse(width_ > 10, width_ , 10),
       limitsize = FALSE)
## 8.2 Arrange the window figures

purrr::map2(
  .x = window_plots,
  .y = names(window_plots),
  .f = ~ {
    ## Prepare the plots
    plots = purrr::map(
      .x = .x,
      .f = ~ {
        arrangeGrob(grobs = .x , ncol = 1, nrow = length(.x))
      })
    class(plots) <- c("arrangelist", 'list')
    # print(class(plots))
    ggsave(paste0(.y,"_window_plots.pdf"), plot = plots, height = 18, width = 10)

  })
## 8.3 Arrange the summary plots
plan(sequential)
future_map(
  # .x = names(raw_plots),
  .x = 'IND 15',
  .f = ~ {
    layout = rbind(c(1,1,1,1),
                   c(2,2,3,6),
                   c(2,2,4,7),
                   c(2,2,5,8),
                   c(2,2,9,10))
    plots =  arrangeGrob(raw_plots[[.x]],
                         actograms_by_window[[.x]],
                         acf_plots$period_plots[[.x]], acf_plots$rhythm_plots[[.x]], acf_plots$granger_plots[[.x]],
                         lsp_plots$period_plots[[.x]], lsp_plots$rhythm_plots[[.x]], lsp_plots$granger_plots[[.x]],
                         lsp_plots$amplitude_plots[[.x]], lsp_plots$phase_plots[[.x]],
                         nrow = 5, ncol = 4,
                         layout_matrix = layout)



    ggsave(paste0(.x,"_summary_plots.pdf"), plot = plots, width = 15, height = 10, limitsize = FALSE)

  })

## 9. Export Data
plan(sequential)
future_map2(
  .x = trikinetics_tidy,
  .y = c('analysis_data', 'autocorrelation_results', 'lomb_scargle_results', 'utils'),
  .f = ~ {
    df = rename(.x, unique_identifier = data)
    write_csv(df, paste0(.y,'.csv'))
    })
