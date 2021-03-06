% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acf_window.R
\name{acf_window}
\alias{acf_window}
\title{Auto Correlation with a moving window}
\usage{
function(df = NULL,  from = 18, to = 30,
sampling_rate = "1 hour", window_vector = NULL, values = NULL)
}
\arguments{
\item{df}{A data.frame with 2 columns. Column 1 must contain the windows to iterate over.
Column 2 must supply the values. This parameter is optional if window_vector and values are supplied.
df must not have gaps in the dates, acf asumes data is evenly spaced.}

\item{from}{The period (in hours) from which to start looking for peaks in the autocorrelation. Default = 18.}

\item{to}{The period (in hours) up to which  to look for peaks in the autocorrelation. Default = 30.}

\item{sampling_rate}{A charater string which indicates the sampling rate of the data.
For example: "1 second", "2 minutes", "1 hour" (default),"3 days", "11 months".}

\item{window_vector}{A vector containing the windows to iterate over and to label the group
to which each value belongs. Usually this will be the output from make_time_windows.}

\item{values}{The data for which we want to find the period.}
}
\value{
A data.frame with the autocorrelation results for each window which include: period, peaks,
power, lags for the peaks.
}
\description{
Uses autocorrelation to find a circadian period for a given timeseries
}
\examples{
autocorrelations_multipeak <- acf_window(df = df_with_windows,
multipeak_period = FALSE, peak_of_interest = 2,
sampling_unit = "hours")

}
\seealso{
[stats::acf()] which this functions uses to run the autocorrelation.
}
