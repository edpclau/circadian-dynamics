#' Find peaks in an lsp object
#'
#' @param object
#' @param npeaks
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom pracma findpeaks
lsp_peaks = function (object, npeaks = 5, plotit = FALSE)
{
  pks = findpeaks(object$power, npeaks = npeaks, minpeakheight = 0,
                  sortstr = TRUE)
  peaks = pks[, 1]
  tmes = object$scanned[pks[, 2]]
  tme = round(tmes, 2)
  d = data.frame(time = tme, peaks = peaks)
  return(d)
}
