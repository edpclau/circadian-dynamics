# circadian-dynamics
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fedpclau%2Fcircadian-dynamics.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Fedpclau%2Fcircadian-dynamics?ref=badge_shield)

A repository for analyzing circadian data
## 1. To install in R use:
```{r}
devtools::install_github("edpclau/circadian-dynamics")
```


## 2. Example script for TriKinetics monitors

###        a) Import Trikinetics data into R (opens a GUI)   
```{r}
monitor <- read_trikinetics()
```

###        b) Change bin size (in this case its a 1hr bin)   

```{r}
monitor_downsampled <- downsample_time_series(monitor[-2], amount = 1, units = "hour", method = "sum")
```

###        c) Set window size, steps and data filtering options   

```{r}
monitor_processed <- multivariate_process_timeseries(
  df = monitor_downsampled,
  sampling_rate = '1 hour',
  window_size_in_days =3,
  window_step_in_days = 1, 
  detrend_data = TRUE,
  butterworth = TRUE,
  f_low=1/4,
  f_high=0,
  plot = TRUE
  )

```

###        d) Data analysis and period determination steps  

```{r}
 
monitor_analysis <- multivariate_rythm_analysis(
  monitor_processed,
  sampling_rate = "1 hour",
  alpha=0.000000001,
  autocorrelation = TRUE,
  lomb_scargle = TRUE,
    from = 16, to = 32,
  ofac =60)

```

###        e) Export data and figures


```{r}
export_all(raw_data = monitor_downsampled,
           processed_data = monitor_processed,
           rythm_analysis_data = monitor_analysis,
         autocorrelation = TRUE,
           new_dir_name = "Monitor_40")
```           
           
