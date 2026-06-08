#' Plot autocorrelation results by window
#'
#' @param df The `autocorrelation` table from [simplify_data()].
#' @return A named list with `period_plots` and `rhythm_plots`, one ggplot per individual.
#' @export
#' @examples
#' \dontrun{
#' plots <- plot_acf_results(simplified$autocorrelation)
#' }
#' @importFrom rlang .data
plot_acf_results <- function(df) {
  nested <- .prep_window_df(df, c("rhythm_strength", "period"))
  list(
    period_plots = .window_metric(nested, "period", "Autocorrelation Period", ylab = "Hours"),
    rhythm_plots = .window_metric(nested, "rhythm_strength", "Autocorrelation Rhythm Strength", hline = 1)
  )
}
