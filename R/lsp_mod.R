#' Lomb-Scargle Periodogram
#'
#' @description
#' A modification on the lomb-pkg's lsp function [lomb::lsp()]. Automatically increases the oversampling factor when it is too small for
#' the chosen from-to interval. Computes the Lomb-Scargle periodogram for a time series with irregular (or regular) sampling intervals.
#' Allows selecting a frequency range to be inspected, as well as the spacing of frequencies scanned.
#' @usage
#' lsp_mod(x, times = NULL, from = NULL, to = NULL, type = c("frequency", "period"), ofac = 1, alpha = 0.01, plot = TRUE, ...)
#'
#' @param x
#' The data to be analysed. x can be either a two-column numerical dataframe or matrix, with sampling times in columnn 1 and measurements in column 2, a single numerical vector containing measurements, or a single vector ts object (which will be converted to a numerical vector).
#' @param times
#' If x is a single vector, times can be provided as a numerical vector of equal length containing sampling times. If x is a vector and times is NULL, the data are assumed to be equally sampled and times is set to 1:length(x).
#'
#' @param from
#' The starting frequency (or period, depending on type) to begin scanning for periodic components.
#'
#' @param to
#' The highest frequency (or period, depending on type) to scan.
#'
#' @param type
#' Either “frequency” (the default) or “period”. Determines the type of the periodogram x-axis.
#'
#' @param ofac
#' The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
#'
#' @param alpha
#' The significance level. The periodogram plot shows a horizontal dashed line. Periodogram peaks exceeding this line can be considered significant at alpha. Defaults to 0.01. Only used if plot=TRUE.
#'
#' @param plot
#' Logical. If plot=TRUE the periodogram is plotted.
#'
#' @param ...
#' Further graphical parameters affecting the periodogram plot.
#'
#'
#' @return
#' A named list with the following components:
#'
#' scanned	 A vector containing the frequencies/periods scanned.
#'
#' power	   A vector containing the normalised power corresponding to scanned frequencies/periods.
#'
#' data	     Names of the data vectors analysed.
#'
#' n	       The length of the data vector.
#'
#' type	     The periodogram type used, either "frequency" or "period".
#'
#' ofac	     The oversampling factor used.
#'
#' n.out	   The length of the output (powers). This can be >n if ofac >1.
#'
#' alpha	   The false alarm probability used.
#'
#' sig.level Powers > sig.level can be considered significant peaks at p=alpha.
#'
#' peak	     The maximum power in the frequency/period interval inspected.
#'
#' peak.at	 The frequency/period at which the maximum peak occurred.
#'
#' p.value	 The probability that the maximum peak occurred by chance.
#'
#' @note
#' For a description of the properties of the Lomb-Scargle Periodogram, its computation and comparsion with other methods see Ruf, T. (1999). Function lsp uses the algorithm given by Press et al (1994). The Lomb-Scargle Periodogram was originaly proposed by Lomb N.R. (1976) and furher extended by Scargle J.D. (1982).
#'
#' @author
#' Original code by: Thomas Ruf thomas.ruf@vetmeduni.ac.at based on code by Press et al (1994).
#'
#' Modified by: Eddie Perez Claudio eddieperezclaudio@gmail.com
#'
#' @references
#' Lomb N.R. (1976) Least-squares frequency analysis of unequally spaced data. Astrophysics and Space Science 39:447–462
#' Press W.H., Teukolsky S.A., Vetterling S.T., Flannery, B.P. (1994) Numerical recipes in C: the art of scientific computing.2nd edition. Cambridge University Press, Cambridge, 994pp.
#' Ruf, T. (1999) The Lomb-Scargle Periodogram in Biological Rhythm Research: Analysis of Incomplete and Unequally Spaced Time-Series. Biological Rhythm Research 30: 178–201.
#' Scargle J.D. (1982) Studies in astronomical time series. II. Statistical aspects of spectral analysis of unevenly spaced data. The Astrophysical Journal 302: 757–763.
#'
#' @export
#'
#' @examples
#' # ibex contains an unevenly sampled time series
#' data(ibex)
#' lsp(ibex[2:3],)
#' lsp(ibex$temp,times=ibex$hours,type='period',ofac=5)

#' # lynx contains evenly sampled data
#' lsp(lynx)
#' lynx.spec <- lsp(lynx,type='period',from=2,to=20,ofac=5)
#' summary(lynx.spec)
#'
#' @importFrom pracma findpeaks
lsp_mod <- function (x, times = NULL, from = NULL, to = NULL,
                     type = c("frequency", "period" ), ofac = 1, alpha = 0.0001, plot = TRUE, ...)
{
###### Flow Control
  type <- match.arg(type)
  if (ofac != floor(ofac)) {
    ofac <- floor(ofac)
    warning("ofac coerced to integer")
  }
  if (ofac < 1) {
    ofac <- 1
    warning("ofac must be integer >=1. Set to 1")
  }
  if (!is.null(times)) {
    if (!is.vector(times))
      stop("no multivariate methods available")
    if (length(x) != length(times))
      stop("Length of data and times vector must be equal")
    names <- c(deparse(substitute(times)), deparse(substitute(x)))
  }
  if (is.null(times) && is.null(ncol(x))) {
    names <- c("Time", deparse(substitute(x)))
    times <- 1:length(x)
  }
  if (is.matrix(x) || is.data.frame(x)) {
    if (ncol(x) > 2)
      stop("no multivariate methods available")
    if (ncol(x) == 2) {
      names <- colnames(x)
      times <- x[, 1]
      x <- x[, 2]
    }
  }

# If no times are provided and


  times <- times[!is.na(x)]
  x <- x[!is.na(x)]
  nobs <- length(x)
  if (nobs < 2)
    stop("time series must have at least two observations")

 # if all numbers are exactly the same, return NA for the current window.
 # x cannot have NA's
    if (length(x) == rle(c(x))$lengths[1]) {
      sp.out <- list(scanned = NA, power = NA, data = NA,
                     n = NA, type = type, ofac = ofac, n.out = NA, alpha = alpha,
                     sig.level = 0, peak = 0, peak.at = NA,
                     p.value = NA)
      class(sp.out) <- "lsp"

      return(sp.out)
    }


  times <- as.numeric(times)
  start <- min(times)
  end <- max(times)
  av.int <- mean(diff(times))
  o <- order(times)
  times <- times[o]
  x <- x[o]
  y <- cbind(times, x)
  colnames(y) <- names
  datanames <- colnames(y)
  t <- y[, 1]
  y <- y[, 2]
  n <- length(y)
  tspan <- t[n] - t[1]
  fr.d <- 1/tspan
  step <- 1/(tspan * ofac)
  if (type == "period") {
    hold <- from
    from <- to
    to <- hold
    if (!is.null(from))
      from <- 1/from
    if (!is.null(to))
      to <- 1/to
  }
  if (is.null(to)) {
    f.max <- floor(0.5 * n * ofac) * step
  } else {
    f.max <- to
  }


if (fr.d > f.max & step > 0) {
  return(NULL)}

freq <- seq(fr.d, f.max, by = step)

if (!is.null(from))
    freq <- freq[freq >= from]
  n.out <- length(freq)

while (n.out == 0 & type == "period" & !is.null(from)) {
  ofac <- ofac + 1
  step <- 1/(tspan * ofac)
  freq <- seq(fr.d, f.max, by = step)
  freq <- freq[freq >= from]
  n.out <- length(freq)
 }

  if (n.out == 0)
    stop("erroneous frequency range specified ")

  x <- t * 2 * pi
  y <- y - mean(y)
  norm <- 1/(2 * var(y))
  w <- 2 * pi * freq
  PN <- rep(0, n.out)
  for (i in 1:n.out) {
    wi <- w[i]
    tau <- 0.5 * atan2(sum(sin(wi * t)), sum(cos(wi * t)))/wi
    arg <- wi * (t - tau)
    cs <- cos(arg)
    sn <- sin(arg)
    A <- (sum(y * cs))^2
    B <- sum(cs * cs)
    C <- (sum(y * sn))^2
    D <- sum(sn * sn)
    PN[i] <- A/B + C/D
  }
  PN <- norm * PN
  PN.peaks <- pracma::findpeaks(c(PN))[,1]
  if(all(is.na(PN.peaks))) {
    PN.max = NA
  } else {
  PN.max <- max(PN.peaks)
  }
  # if (PN.max == -Inf) {PN.max = max(PN)}
# If we can't find a peak, return NA.
  if(rlang::is_null(PN.max) | is.na(PN.max)) {
    scanned <- if (type == "frequency")
      freq
    else 1/freq
    if (type == "period") {
      scanned <- scanned[n.out:1]
      PN <- PN[n.out:1]
    }
    effm <- 2 * n.out/ofac
    level <- -log(1 - (1 - alpha)^(1/effm))

    sp.out <- list(scanned = scanned, power = PN, data = NA,
                   n = NA, type = type, ofac = ofac, n.out = NA, alpha = alpha,
                   sig.level = level, peak = NA, peak.at = NA,
                   p.value = NA)
    class(sp.out) <- "lsp"

    return(sp.out)
  }

  peak.freq <- freq[PN == PN.max]
  if (type == "period")
    peak.at <- c(1/peak.freq, peak.freq)
  else peak.at <- c(peak.freq, 1/peak.freq)
  scanned <- if (type == "frequency")
    freq
  else 1/freq
  if (type == "period") {
    scanned <- scanned[n.out:1]
    PN <- PN[n.out:1]
  }
  effm <- 2 * n.out/ofac
  level <- -log(1 - (1 - alpha)^(1/effm))
  exPN <- exp(-PN.max)
  p <- effm * exPN
  if (p > 0.01)
    p <- 1 - (1 - exPN)^effm

  sp.out <- list(scanned = scanned, power = PN, data = datanames,
                 n = n, type = type, ofac = ofac, n.out = n.out, alpha = alpha,
                 sig.level = level, peak = PN.max, peak.at = peak.at,
                 p.value = p)
  class(sp.out) <- "lsp"


  if (plot) {
    plot(sp.out, ...)
    return(invisible(sp.out))
  }
  else return(sp.out)
}
