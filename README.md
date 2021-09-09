# circadian-dynamics
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fedpclau%2Fcircadian-dynamics.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Fedpclau%2Fcircadian-dynamics?ref=badge_shield)

A repository for analyzing circadian data
## 1. To install in R use:
```{r}
devtools::install_github("edpclau/circadian-dynamics")
```


## 2. Importing a CSV File 

### 2.1 Make sure your CSV file is in the following format:
![image](https://user-images.githubusercontent.com/65506362/132695215-95d6b9ad-9faa-4443-ab28-ad2d2075eafd.png)

### The first column in your file must be the "datetime". It is important the datetime is in the international standart format (ISO 8601). 
#### If you're an Excel user:
> Right click on cell > Cell format > Category > Custom > Type > write this : yyyy-mm-ddThh:MM:ss


###  2.2 Use the generic [readr](https://readr.tidyverse.org/index.html) function [read_csv](https://readr.tidyverse.org/reference/read_delim.html)
#### Example:     
```{r}
df <- read_csv("filepath")
```

##  3. Analysis Pipeline
### 3.1 *Optional* Change bin size / sampling rate (in this case its a 1hr bin)   

```{r}
df_downsampled <- downsample_time_series(df, amount = 1, units = "hour", method = "sum")
```

### 3.2 Preprocess the Data
#### This step helps the user:
##### 1. Define a moving a window's span (window_size_in_days) and sliding range (window_step_in_days). 
##### 2. Detrend the data
##### 3. Remove noise using a hi and low pass butterworth filter by setting the cut-off frequencies for the high end (f_high) and low end (f_low) 

```{r}
df_processed <- multivariate_process_timeseries(
  df = df_downsampled,
  sampling_rate = '1 hour',
  window_size_in_days =3,
  window_step_in_days = 1, 
  detrend_data = TRUE,
  butterworth = TRUE,
  f_low=1/4,
  f_high=0
  )

```

### 3.3 Periodicity analysis
#### We use multiple approaches to determine the period, phase, rythm strength, etc. Among the methods used are: Autocorrelation and the Lomb Scargle Periodogram.
#### The period found using these methods is fitted using a linear function (Cosinor). 

```{r}
 
df_analysis <- multivariate_rythm_analysis(
  df_processed,
  sampling_rate = "1 hour",
  alpha=0.000000001,
  autocorrelation = TRUE,
  lomb_scargle = TRUE,
    from = 16, to = 32,
  ofac =60)

```

## 4. *Optional* Export your analysis and build figures
#### This is a helper function that creates basic visualizations of your analysis and exports all the data generated into a folder on your device.

```{r}
export_all(raw_data = df_downsampled,
           processed_data = df_processed,
           rythm_analysis_data = df_analysis,
         autocorrelation = TRUE,
           new_dir_name = "analysis")
```           
           
