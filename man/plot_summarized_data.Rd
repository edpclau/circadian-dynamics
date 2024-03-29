% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_summarized_data.R
\name{plot_summarized_data}
\alias{plot_summarized_data}
\title{Plot a sumarrized version of the Rythm data found by Rythm_analysis}
\usage{
plot_summarized_data(raw_data = NULL, analyzed_data = NULL,
dir_choose_gui = TRUE)
}
\arguments{
\item{raw_data}{data.frame or tibble containing the raw data. The first column must be a datetime.}

\item{analyzed_data}{data.frame returned from the rythm_analysis_by_window and multivariate_rythm_analysis functions.}

\item{dir_choose_gui}{If TRUE (default) a GUI will help select the folder in which to save the data and plots. If FALSE,
everything will be saved in the current directory.}
}
\description{
Plot a sumarrized version of the Rythm data found by Rythm_analysis
}
\examples{
plot_summarized_data(raw_data = monitor_downsampled, analyzed_data = monitor_analysis, dir_choose_gui = TRUE)

}
