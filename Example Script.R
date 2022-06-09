# 1. Load Library
library(circadiandynamics)

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

# 5. Rhythm Analysis
trikinetics_analyzed = process_timeseries.main(
  df = trikinetics_downsampled,

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
  detrend_data = TRUE,

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
  causal_order = 5,


  #If you're going to be working with big data, make this argument TRUE.
  #Beware, if your dataset is small, setting this argument to TRUE will make
  #it run slower as there is an overhead to paralleling the analysis.
  big_data = FALSE
)


# 6. Tidy up data for export
## The data outputted by 'process_timeseries.main) is not easily read
##by humans. Therefore, we have deviced a function that arranges the data
## into 3 data.frames that are easy to export and read.
trikinetics_tidy = simplify_data(trikinetics_analyzed)


