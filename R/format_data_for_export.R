#' Format Data for Export
#'
#' @param results
#'
#' @return
#' @export
#'
#' @examples
data_export <- function(results) {

if (is_empty(names(results))) {

  data = purrr::map_df(
    .x = results,
    .id = 'window',
    .f = ~ {
      .x$data
    }
  )

  acf = purrr::map_df(.x = results,
              .id = 'window',
             .f = ~{
                 dplyr::tibble(
                 period = .x$acf$results$period,
                 rythm_strength = .x$acf$results$rythm_strength,
                 granger_causality = min(unlist(.x$acf$results$grangercausal)),
                 mesor = .x$acf$cosinor$mesor,
                 amplitude = .x$acf$cosinor$amplitude,
                 phase = .x$acf$cosinor$phase,
                 adj_r_sqr = .x$acf$cosinor$adj_r_squared
               )
             })

 lomb =  purrr::map_df(.x = results,
                       .id = 'window',
                .f = ~ {
  dplyr::tibble(
    period = .x$lomb$results$period,
    rythm_strength = min(.x$lomb$results$rythm_strength),
    granger_causality = min(unlist(.x$lomb$results$grangercausal)),
    mesor = .x$lomb$cosinor$mesor,
    amplitude = .x$lomb$cosinor$amplitude,
    phase = .x$lomb$cosinor$phase,
    adj_r_sqr = .x$lomb$cosinor$adj_r_squared
  )
                }
  )

} else {

data = results$data

acf = dplyr::tibble(
  period = results$acf$results$period,
  rythm_strength = results$acf$results$rythm_strength,
  granger_causality = min(unlist(results$acf$results$grangercausal)),
  mesor = results$acf$cosinor$mesor,
  amplitude = results$acf$cosinor$amplitude,
  phase = results$acf$cosinor$phase,
  adj_r_sqr = results$acf$cosinor$adj_r_squared
  )

lomb = dplyr::tibble(
  period = results$lomb$results$period,
  rythm_strength = min(results$lomb$results$rythm_strength),
  granger_causality = min(unlist(results$lomb$results$grangercausal)),
  mesor = results$lomb$cosinor$mesor,
  amplitude = results$lomb$cosinor$amplitude,
  phase = results$lomb$cosinor$phase,
  adj_r_sqr = results$lomb$cosinor$adj_r_squared
  )
}


return(
  list(
    data = data,
    acf = acf,
    lomb = lomb
  )
)

  }
