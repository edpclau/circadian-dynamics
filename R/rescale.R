#' Rescale the data from 0 to 1.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
rescale <- function(x){
  return(
    (x-min(x))/(max(x)-min(x))
    )
  }
