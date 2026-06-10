#' Plot Lomb-Scargle results by window
#'
#' @param df The `lombscargle` table from [simplify_data()].
#' @return A named list with `period_plots`, `relative_power_plots`,
#'   `amplitude_plots`, and `phase_plots`, one ggplot per individual.
#' @export
#' @examples
#' \dontrun{
#' plots <- plot_lsp_results(simplified$lombscargle)
#' }
#' @importFrom rlang .data
plot_lsp_results <- function(df) {
  nested <- .prep_window_df(df, c("relative_power", "period", "amplitude", "phase", "amp_se", "phase_se"))
  list(
    period_plots = .window_metric(nested, "period", "Lomb-Scargle Period", ylab = "Hours"),
    relative_power_plots = .window_metric(nested, "relative_power", "Lomb-Scargle Relative Spectral Power", ylab = "Fraction of total power"),
    amplitude_plots = .window_metric(nested, "amplitude", "Cosinor Amplitude (lsp)", se = "amp_se"),
    phase_plots = .window_metric(nested, "phase", "Cosinor Phase (lsp)", se = "phase_se")
  )
}
