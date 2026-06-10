# Colourblind-safe palette helpers. Kept dependency-free on purpose: the Okabe-Ito
# hexes are constants and continuous scales use ggplot2's bundled viridis, so the
# package needs no extra colour package.

# Okabe-Ito qualitative palette (Okabe & Ito, Color Universal Design; popularised by
# Wong 2011, Nature Methods 8:441). The field-recommended categorical default for
# distinguishing individuals/groups; 8 hues that stay distinguishable for most
# colour-vision deficiencies. Order matches Wong's figure (black last).
.okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#000000"  # black
)

#' First `n` Okabe-Ito colours
#'
#' Internal colourblind-safe categorical palette accessor. Supplies up to eight
#' hues in the canonical Okabe-Ito order.
#'
#' @param n Number of colours to return (1-8).
#' @return A character vector of `n` hex colour codes.
#' @keywords internal
okabe_ito <- function(n = 8) {
  if (n > length(.okabe_ito)) {
    stop("okabe_ito() supplies at most ", length(.okabe_ito),
         " colours; asked for ", n, ".", call. = FALSE)
  }
  .okabe_ito[seq_len(n)]
}
