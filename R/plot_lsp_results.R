#' Plot Lomb-Scargle results by window
#'
#' @param df The `lombscargle` table from [simplify_data()].
#' @return A named list with `period_plots`, `rhythm_plots`, `amplitude_plots`,
#'   and `phase_plots`, one ggplot per individual.
#' @export
#' @examples
#' \dontrun{
#' plots <- plot_lsp_results(simplified$lombscargle)
#' }
#' @importFrom rlang .data
plot_lsp_results <- function(df) {
  nested <- .prep_window_df(df, c("rhythm_strength", "period", "amplitude", "phase"))
  list(
    period_plots = .window_metric(nested, "period", "Lomb-Scargle Period", ylab = "Hours"),
    rhythm_plots = .window_metric(nested, "rhythm_strength", "Lomb-Scargle Rhythm Strength", hline = 1),
    amplitude_plots = .window_metric(nested, "amplitude", "Cosinor Amplitude (lsp)"),
    phase_plots = .window_metric(nested, "phase", "Cosinor Phase (lsp)")
  )
}
