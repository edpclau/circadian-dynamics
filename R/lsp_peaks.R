#' Find peaks in an lsp object
#'
#' @param object An lsp object returned by \code{lsp_mod()}.
#' @param npeaks Integer. Maximum number of peaks to return. Default is 5.
#' @param plotit Logical. Currently unused; reserved for future plotting support. Default = FALSE.
#'
#' @return A data.frame with columns \code{time} (period/frequency at each peak) and \code{peaks} (peak power).
#' @keywords internal
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
