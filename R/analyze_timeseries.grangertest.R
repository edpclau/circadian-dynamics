#' Apply the Granger Causality test on both directions
#'
#' @param value
#' @param cos
#' @param order
#'
#' @return
#' @export
#'
#' @examples
analyze_timeseries.grangertest <- function(value, cos, order = 1, period) {

  #if no cosinor, return nothing
  if (is_empty(cos) | is.na(period)) {
    return(
      list(
        rawdata_to_cos = NA,
        cos_to_rawdata = NA
        )
      )
  } else {


  return(
    list(
      rawdata_to_cos = lmtest::grangertest(x = value, y = cos, order = order)[[4]][2],
      cos_to_rawdata = lmtest::grangertest(y = value, x = cos, order = order)[[4]][2]
    )
  )

  }

}
