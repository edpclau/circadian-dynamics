A repository for analyzing circadian data

# [Live Notebook on GoogleColab](https://colab.research.google.com/drive/11ly3tdhCDpbl0P33QhPZA0v3-Yf6jbKe?usp=sharing)

# To install in R use:
```{r}
devtools::install_github("edpclau/circadian-dynamics")
```

# What's new in 4.0.0 (breaking changes)

- **One reader per format.** `read_trikinetics(path, layout)`, `read_clocklab()`, `read_csv_data()`, `read_satellite()`, and `read_vitalpatch()` each accept a file *or* a folder and open a dialog when the path is `NULL`. The old `*_interactive()` / `*_folder()` and `read_trikinetics_nested|long()` names have been removed.
- **Analysis functions renamed** off their misleading S3-style dotted names: `analyze_acf()`, `analyze_cosinor()`, `analyze_lomb()`, and `process_timeseries_main()` / `_core()` / `_waveform()`. The old dotted names have been removed.
- `process_timeseries_core()`'s `ofac` default is now `10`, matching `process_timeseries_main()` and the documentation.
- **Lomb-Scargle rhythm strength** is now reported as `relative_power` — the fraction of total spectral power carried by the dominant in-band peak (`peak / sum(power)`), a length-robust index comparable across windows.
- Many unused plotting functions were removed and the result plotters de-duplicated; the Granger test (dropped in 3.0.0) no longer appears anywhere.

Earlier 3.0.0 changes: Granger causality test removed; `rythm` → `rhythm` throughout; new `adjust_pvalues()` for Benjamini-Hochberg/FDR correction.

A runnable end-to-end example on the bundled `trikinetics` dataset is in the package vignette: `vignette("circadiandynamics")`.

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
The general import function is `read_csv_data()`. It requires that your file is in .csv format. The first column must be the datetime column. The second column should be your Light/Dark data, if you have any. All other columns will be the signals/individuals you want to analyze. For a Trikinetics file use `read_trikinetics()`. In this example we use a trikinetics file.
```{r}
trikinetics = read_trikinetics(file)
```

# 4. **** REQUIRED ***** Define meta-data (Sampling Rate)
It is critical that the sampling rate is specified correctly. The helper values below are reused for the actogram sampling (step 5) and the Butterworth cutoffs (step 6).
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
actogram(read_trikinetics(file, layout = "long")[-2], sampling = sampling_rate_in_minutes)
```
# 6. Rhythm Analysis
### This is the main function of the library.
```{r}
trikinetics_analyzed = process_timeseries_main(

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
  #Order for the butterworth filter
  order = 2,

  #If you're going to be working with big data, make this argument TRUE.
  #Beware, if your dataset is small, setting this argument to TRUE will make
  #it run slower as there is an overhead to paralleling the analysis.
  big_data = FALSE,

  ##Control the p.value threshold and the oversampling factor for the
  #Lomb-Scargle periodogram. ofac is a small integer (typically 1-10);
  #values above 20 are capped with a warning.
  ofac = 10,
  lomb_pvalue = 0.05
)
```
# 7. Export Figures
## 7.1 Export Detailed Plots
Make sure the sampling_rate says if the data is sampled in minutes, hours, or days. If you selected make_windows = TRUE in the analysis,
```{r}
detailed_plots(trikinetics_analyzed, sampling_rate = 'minutes', windows = TRUE)
```
When the output says any of the following: Autocorrelation Not Run or Lomb-Scargle Not Run, it means that for whatever reason, the output of the period for that test was an NA. We can interpret that as being unable to find a period within the range we selected. It could be that the individual we are studying has a period outside the boundaries of the FROM and TO. You could repeat the analysis and widen that search window. <br>
If the output we get for a period is NA, we don't run a Cosinor analysis for it.

# 8. Export Data
## 8.1 Tidy Data
The data outputted by `process_timeseries_main()` is nested and not easy to read by hand. `simplify_data()` arranges it into 4 tidy data.frames — `data`, `autocorrelation`, `lombscargle`, and `utils` — that are easy to export and read.
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






