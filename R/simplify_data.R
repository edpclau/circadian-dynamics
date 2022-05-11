#' Simplify Data
#'
#' @param df Output from processed_timeseries.main
#' @param big_data Are you using a large dataset? TRUE or FALSE (default)
#'
#' @return
#' @export
#'
#' @importFrom future multisession sequential
#' @importFrom furrr future_map_dfr
#' @importFrom dplyr tibble
#'
simplify_data <- function(df, big_data = FALSE) {
  if (big_data) {
    plan(multisession)
  } else {
    plan(sequential)
  }

  if (any(class(df[[1]][[1]]) == 'list')) {
    #### Tibble Processed Data Values ####
    processed_data = future_map_dfr(.x = df,
                                    .id = 'data',
                                    .f = ~ {
                                      df = .x
                                      windows = seq(length(.x))
                                      future_map_dfr(.x = windows,
                                                     .f = ~ {
                                                       data = df[[.x]]$data
                                                       data$lomb_cosinor = df[[.x]]$lomb$cosinor$wave
                                                       data$autocorr_cosinor = df[[.x]]$acf$cosinor$wave
                                                       names(data)[2] = 'raw_values'
                                                       data
                                                     },
                                                     .id = 'window')
                                    })
    ##### Autocorrelation Results #####
    autocorrelation = future_map_dfr(.x = df,
                                     .id = 'data',
                                     .f = ~ {
                                       df = .x
                                       windows = seq(length(.x))
                                       future_map_dfr(.x = windows,
                                                      .f = ~ {
                                                        tibble(
                                                          peak_datetime = df[[.x]]$acf$results$datetime,
                                                          period = df[[.x]]$acf$results$period,
                                                          rythm_strength = df[[.x]]$acf$results$rythm_strength,
                                                          gc_raw_to_cos = df[[.x]]$acf$results$grangercausal$rawdata_to_cos,
                                                          gc_cos_to_raw = df[[.x]]$acf$results$grangercausal$cos_to_rawdata,
                                                          mesor = df[[.x]]$acf$cosinor$mesor,
                                                          amplitude = df[[.x]]$acf$cosinor$amplitude,
                                                          amp_se = df[[.x]]$acf$cosinor$amplitude_se,
                                                          acrophase = df[[.x]]$acf$cosinor$acrophase,
                                                          acro_se = df[[.x]]$acf$cosinor$acrophase_se,
                                                          phase = df[[.x]]$acf$cosinor$phase,
                                                          phase_se = df[[.x]]$acf$cosinor$phase_se,
                                                          adj_r_squared = df[[.x]]$acf$cosinor$adj_r_squared,
                                                          cosinor_p_value = df[[.x]]$acf$cosinor$p_value
                                                        )
                                                      },
                                                      .id = 'window')
                                     })




    ##### Lomb Scargle Results #####
    lombscargle = future_map_dfr(.x = df,
                                 .id = 'data',
                                 .f = ~ {
                                   df = .x
                                   windows = seq(length(.x))
                                   future_map_dfr(.x = windows,
                                                  .f = ~ {
                                                    tibble(
                                                      peak_datetime = df[[.x]]$lomb$results$datetime,
                                                      period = df[[.x]]$lomb$results$period,
                                                      rythm_strength = df[[.x]]$lomb$results$rythm_strength,
                                                      gc_raw_to_cos = df[[.x]]$lomb$results$grangercausal$rawdata_to_cos,
                                                      gc_cos_to_raw = df[[.x]]$lomb$results$grangercausal$cos_to_rawdata,
                                                      mesor = df[[.x]]$lomb$cosinor$mesor,
                                                      amplitude = df[[.x]]$lomb$cosinor$amplitude,
                                                      amp_se = df[[.x]]$lomb$cosinor$amplitude_se,
                                                      acrophase = df[[.x]]$lomb$cosinor$acrophase,
                                                      acro_se = df[[.x]]$lomb$cosinor$acrophase_se,
                                                      phase = df[[.x]]$lomb$cosinor$phase,
                                                      phase_se = df[[.x]]$lomb$cosinor$phase_se,
                                                      adj_r_squared = df[[.x]]$lomb$cosinor$adj_r_squared,
                                                      cosinor_p_value = df[[.x]]$lomb$cosinor$p_value
                                                    )
                                                  },
                                                  .id = 'window')
                                 })




  } else {
    #### Tibble Processed Data Values ####
    processed_data = future_map_dfr(.x = df,
                     .f = ~ {
                       data = .x$data
                       data$lomb_cosinor = .x$lomb$cosinor$wave
                       data$autocorr_cosinor = .x$acf$cosinor$wave
                       names(data)[2] = 'raw_values'
                       data
                     },
                     .id = 'data')
    ##### Autocorrelation Results #####
    autocorrelation = future_map_dfr(.x = df,
                                     .f = ~ {
                                       tibble(
                                         peak_datetime = .x$acf$results$datetime,
                                         period = .x$acf$results$period,
                                         rythm_strength = .x$acf$results$rythm_strength,
                                         gc_raw_to_cos = .x$acf$results$grangercausal$rawdata_to_cos,
                                         gc_cos_to_raw = .x$acf$results$grangercausal$cos_to_rawdata,
                                         mesor = .x$acf$cosinor$mesor,
                                         amplitude = .x$acf$cosinor$amplitude,
                                         amp_se = .x$acf$cosinor$amplitude_se,
                                         acrophase = .x$acf$cosinor$acrophase,
                                         acro_se = .x$acf$cosinor$acrophase_se,
                                         phase = .x$acf$cosinor$phase,
                                         phase_se = .x$acf$cosinor$phase_se,
                                         adj_r_squared = .x$acf$cosinor$adj_r_squared,
                                         cosinor_p_value = .x$acf$cosinor$p_value
                                       )
                                     },
                                     .id = 'data')




    ##### Lomb Scargle Results #####
    lombscargle = future_map_dfr(.x = df,
                                 .f = ~ {
                                   tibble(
                                     peak_datetime = .x$lomb$results$datetime,
                                     period = .x$lomb$results$period,
                                     rythm_strength = .x$lomb$results$rythm_strength,
                                     gc_raw_to_cos = .x$lomb$results$grangercausal$rawdata_to_cos,
                                     gc_cos_to_raw = .x$lomb$results$grangercausal$cos_to_rawdata,
                                     mesor = .x$lomb$cosinor$mesor,
                                     amplitude = .x$lomb$cosinor$amplitude,
                                     amp_se = .x$lomb$cosinor$amplitude_se,
                                     acrophase = .x$lomb$cosinor$acrophase,
                                     acro_se = .x$lomb$cosinor$acrophase_se,
                                     phase = .x$lomb$cosinor$phase,
                                     phase_se = .x$lomb$cosinor$phase_se,
                                     adj_r_squared = .x$lomb$cosinor$adj_r_squared,
                                     cosinor_p_value = .x$lomb$cosinor$p_value
                                   )
                                 },
                                 .id = 'data')

  }
  return(
    list(
      data = processed_data,
      autocorrelation = autocorrelation,
      lombscargle = lombscargle
    )
  )
}