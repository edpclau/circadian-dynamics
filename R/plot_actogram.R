
#' Export/Plot Actogram
#'
#' @details Exports actogram plots for each column of a data.frame of tibble with a datetime object.
#' @usage plot_actogram(df = NULL, datetime_column = 1, filename = "actogram.pdf")
#' @param df A data.frame or tibble which contains a datetime column and measurement values.
#' @param datetime_column An integer which indicates the column that contains the datetime.
#' @param filename A charater string  ending with ".pdf" which customizes the name of the file to be exported. "actogram.pdf" (default)
#'
#' @examples
#' plot_actogram(df = monitor,
#' datetime_column = 1)
plot_actogram <- function(df = NULL, datetime_column = 1, filename = "actogram.pdf") {

##### Flow Control #####
if (!is.null(df)) {
  names(df)[datetime_column] <- "datetime"
  df <- df %>% dplyr::select(datetime, dplyr::everything())
} else (
  stop("Must provide a data.frame or tibble.")
  )




df <- df %>% make_time_windows(window_size_in_days = 2, window_step_in_days = 1)

pdf(file = filename)
#### Parameters for how the plots will look on the page
n_plots = length(unique(df$window))
par(mar=c(2,4,1.75,2), mfrow = c(n_plots,1))


for (ind in 3:ncol(df)) {


for (i in unique(df$window)) {

plot(
  x = dplyr::filter(df, window == i) %>% dplyr::pull(datetime),
  y = dplyr::filter(df, window == i) %>% dplyr::pull(ind),
  type = "h",
  ylab = " ",
  xlab = " "
)



}

  mtext(text = paste(names(df)[ind]), side= 3, padj = 1.5, adj = 0, outer = TRUE)
  mtext(text=paste(min(df$datetime),max(df$datetime),sep=" --- "),side= 3,outer=TRUE,padj=1.5)

}
invisible(dev.off())

}
