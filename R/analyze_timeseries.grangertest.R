#' Apply the Granger Causality test on both directions
#'
#' @param value
#' @param cos
#' @param order
#'
#' @return
#' @export
#'
#' @importFrom lmtest grangertest
#' @examples
analyze_timeseries.grangertest <- function(value, cos, order = 2) {

  #if no cosinor, return nothing
  if (is_empty(cos)) {
    return(
      list(
        rawdata_to_cos = NA,
        cos_to_rawdata = NA
        )
      )
  } else {
    value = ts(value)
    cos = ts(cos)
    rawdata_to_cos = tryCatch(
            expr = {
              # message(paste('Failed Granger Test with causal order',order+1))
              grangertest(x = value, y = cos, order = order)[[4]][2]
              },
            error = function(e) {
              # message(paste('Failed Granger Test with causal order',order+2))
              # message('Possible Aliased coefficients. Try a larger order.')
              # message('Results will not Contain Granger Test due to Aliased Coefficients.')
              return(NA)},
            finally = return(NA)
          )


    cos_to_rawdata = tryCatch(
          expr = {
            grangertest(y = value, x = cos, order = order)[[4]][2]
          },
          error = function(e) {
            # message('Final Results will not Contain this test.')
            return(NA)},
          finally = return(NA)
    )
    }






  return(
    list(
      rawdata_to_cos = rawdata_to_cos,
      cos_to_rawdata = cos_to_rawdata
    )
  )

  }

