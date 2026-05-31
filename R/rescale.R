#' Rescale the data from 0 to 1.
#'
#' @param x A numeric vector to rescale.
#'
#' @return A numeric vector rescaled to the range \code{[0, 1]}.
#' @export
rescale <- function(x){
  return(
    (x-min(x))/(max(x)-min(x))
    )
  }
