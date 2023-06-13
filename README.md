A repository for analyzing circadian data

# To install in R use:
```{r}
devtools::install_github("edpclau/circadian-dynamics")
```

# June 13, 2023
I am temporarily removing the Granger Test.We have recently found many instances of this test returning false positives and false negatives. Although we wanted to use this test as a novel test of rhythmicity it seems we can't do that at this moment. 
Remember that this test evaluates whether the Cosinor fit we create,can be used to predict future changes in the raw data and vice versa. The idea is that if an individual or signal is rhythmic for a given period, it should present a causal relationship between the cosinor and raw data.
Note that if the Cosinor and the data match perfectly, the granger test will output an NA. If the granger test returns a pvalue > 0.05 or NA, this does not mean your data is generally arhythmic! We can only say that the data is not-rythimic for the specific period/frequency given to the cosinor. Your data may still be rhythmic just with a different period.

# Usage:

# 1. Load Library
```{r}
library(circadiandynamics)
library(furrr)
library(tidyverse)
library(gridExtra)
```

# 2. Choose file
```{r}
file = file.choose()
```
# 3. Depending on the file choose your import function
The general import function is 'read_csv_data'. It requires that your file is in .csv format. The first column must be the datetime column. The second column should be your Light/Dark data, if you have any. All other columns will be the signals/individuals you want to analyze. If you're going to import a trikinetics file use 'read_trikinetics_2'. In this example we use a trikinetics file.
```{r}
trikinetics = read_trikinetics_2(file)
```

# 4. **** REQUIRED ***** Define meta-data (Sampling Rate)
It is critical that the sampling_rate is specified correctly. We are working on some stability improvements, in the meantime you will have to specify the sampling_rate twice.
```{r}
sampling_rate_in_seconds = 60 #This is an example of 1 minute.
```
### RUN THIS
### Do not modify
```{r}
sampling_rate_numeric = 3600/sampling_rate_in_seconds
sampling_rate_in_minutes = 60/sampling_rate_in_seconds
```
### Thank you!


# 5. Generate Actograms to choose which individuals to analyze
Run without modifying. The actogram will be saved in your working directory.
```{r}
actogram(read_trikinetics(file)[-2], sampling = sampling_rate_in_minutes)
```
# 6. Rhythm Analysis
### This is the main function of the library.
```{r}
trikinetics_analyzed = process_timeseries.main(

  df = trikinetics,

  # Window of Analysis Arguments
  make_windows = TRUE,
  window_size_in_days = 5, # Recommended minimum is 5 days
  window_step_in_days = 1,

  # Period in which to look for peaks within each window (in hours)
  from = 15, #15 hours
  to = 33, #33 hours

  #The function needs to know the sampling rate of your data.
  sampling_rate = '1 minute',

  # Should the data be detrended before analysis?
  detrend_data = FALSE, #Use only for data that has seasonal trends.

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
  #The frequency given to the butterworth must match the sampling rate of the
  #data. That is why we are multiplying it by the sampling rate.
  #Here we are removing all periods below 12 hours and those above 35 hours.
  f_low = 1/(12*sampling_rate_numeric),
  f_high = 1/(35*sampling_rate_numeric),
  # f_high = 0,
  #Order for the butterworth filter
  order = 2,

  #If you're going to be working with big data, make this argument TRUE.
  #Beware, if your dataset is small, setting this argument to TRUE will make
  #it run slower as there is an overhead to paralleling the analysis.
  big_data = FALSE,

  ##Control the p.value threshold and the ovarsampling factor for the
  #lomb-scargle periodogram
  ofac = sampling_rate_in_seconds,
  lomb_pvalue = 0.05
)
```
# 7. Export Figures
## 7.1 Export Detailed Plots
Make sure the sampling_rate says if the data is sampled in minutes, hours, or days. If you selected make_time_windows = TRUE in the analysis,
```{r}
detailed_plots(trikinetics_analyzed, sampling_rate = 'minutes', windows = TRUE)
```
When the output says any of the following: Autocorrelation Not Run or Lomb-Scargle Not Run, it means that for whatever reason, the output of the period for that test was an NA. We can interpret that as being unable to find a period within the range we selected. It could be that the individual we are studying has a period outside the boundaries of the FROM and TO. You could repeat the analysis and widen that search window. <br>
If the output we get for a period is NA, we don't run a Cosinor analysis for it.

# 8. Export Data
## 8.1 Tidy Data
The data outputted by 'process_timeseries.main) is not easily read by humans. Therefore, we have deviced a function that arranges the data into 3 data.frames that are easy to export and read.
```{r}
trikinetics_tidy = simplify_data(trikinetics_analyzed)
```
## 8.2 Export Data
```{r}
plan(sequential)
future_map2(
  .x = trikinetics_tidy,
  .y = c('analysis_data', 'autocorrelation_results', 'lomb_scargle_results', 'utils'),
  .f = ~ {
    df = rename(.x, unique_identifier = data)
    write_csv(df, paste0(.y,'.csv'))
  })
```






