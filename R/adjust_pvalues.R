#' Adjust p-values for multiple testing across a results grid
#'
#' @description Appends a multiple-testing-adjusted p-value column to a long results
#' data.frame (e.g. one row per individual x window). Use after collecting cosinor or
#' Lomb-Scargle p-values across many tests.
#'
#' @param df A data.frame containing a numeric p-value column.
#' @param p_col Character. Name of the p-value column to adjust.
#' @param method Character. Adjustment method passed to [stats::p.adjust]. Default "BH".
#'
#' @return `df` with an added column named `paste0(p_col, "_adj")`.
#' @export
#'
#' @examples
#' df <- data.frame(cosinor_p_value = c(0.001, 0.02, 0.2))
#' adjust_pvalues(df, p_col = "cosinor_p_value")
adjust_pvalues <- function(df, p_col = "cosinor_p_value", method = "BH") {
  if (!p_col %in% names(df)) {
    stop("Column '", p_col, "' not found in df.")
  }
  df[[paste0(p_col, "_adj")]] <- stats::p.adjust(df[[p_col]], method = method)
  df
}
