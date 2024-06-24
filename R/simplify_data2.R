# Changes to "simplify_data()" to resolve incongruity with "plot_window_data()"

simplify_data2 <- function(df, big_data = FALSE) {
  if (big_data) {
    plan(multisession)
  } else {
    plan(sequential)
  }
  
  if (any(class(df[[1]][[1]]) == 'list')) {
    print('TRUE')
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
                                                       names(data)[which(names(data) == 'value')] = 'raw_values'
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
                                                          acf_peak = df[[.x]]$acf$results$max_peak_of_int,
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
                                                      period = ifelse(is_empty(df[[.x]]$lomb$results$period), NA, df[[.x]]$lomb$results$period),
                                                      rythm_strength = ifelse(is_empty(df[[.x]]$lomb$results$rythm_strength), NA, df[[.x]]$lomb$results$rythm_strength),
                                                      lsp_peak = df[[.x]]$lomb$results$peak,
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
    
    #### Tibble utils data ####
    utils_data = future_map_dfr(.x = df,
                                .id = 'data',
                                .f = ~ {
                                  df = .x
                                  windows = seq(length(.x))
                                  future_map_dfr(.x = windows,
                                                 .f = ~ {
                                                   
                                                   tibble(
                                                     datetime = df[[.x]]$data$datetime,
                                                     acf = df[[.x]]$acf$results$autocorrelation,
                                                     acf_period = df[[.x]]$acf$results$period,
                                                     acf_rs = df[[.x]]$acf$results$rythm_strength,
                                                     acf_peak = df[[.x]]$acf$results$max_peak_of_int,
                                                     acf_peak_time = df[[.x]]$acf$results$datetime,
                                                     lsp_period = ifelse(is_empty(df[[.x]]$lomb$results$period), NA, df[[.x]]$lomb$results$period),
                                                     lsp_peak = df[[.x]]$lomb$results$peak,
                                                     lsp_sig_level = df[[.x]]$lomb$results$sig_level,
                                                     lsp_p_value = df[[.x]]$lomb$results$p_value,
                                                     lsp_scanned = list(df[[.x]]$lomb$results$scanned),
                                                     lsp_power = list(df[[.x]]$lomb$results$power),
                                                     lsp_rs = ifelse(is_empty(df[[.x]]$lomb$results$rythm_strength), NA, df[[.x]]$lomb$results$rythm_strength),
                                                     acf_start = df[[.x]]$acf$results$start,
                                                     acf_end = df[[.x]]$acf$results$end,
                                                     acf_from = df[[.x]]$acf$results$from,
                                                     acf_to = df[[.x]]$acf$results$to,
                                                     lsp_phase = df[[.x]]$lomb$cosinor$phase,
                                                     acf_phase = df[[.x]]$acf$cosinor$phase,
                                                     acf_amp = df[[.x]]$acf$cosinor$amplitude,
                                                     acf_pr = df[[.x]]$acf$cosinor$adj_r_squared,
                                                     lsp_amp = df[[.x]]$lomb$cosinor$amplitude,
                                                     lsp_pr = df[[.x]]$lomb$cosinor$adj_r_squared,
                                                     lsp_gc = ifelse(df[[.x]]$lomb$results$grangercausal$rawdata_to_cos < df[[.x]]$lomb$results$grangercausal$cos_to_rawdata, df[[.x]]$lomb$results$grangercausal$rawdata_to_cos, df[[.x]]$lomb$results$grangercausal$cos_to_rawdata),
                                                     acf_gc = ifelse(df[[.x]]$acf$results$grangercausal$rawdata_to_cos < df[[.x]]$acf$results$grangercausal$cos_to_rawdata, df[[.x]]$acf$results$grangercausal$rawdata_to_cos, df[[.x]]$acf$results$grangercausal$cos_to_rawdata)
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
                                      names(data)[which(names(data) == 'value')] = 'raw_values'
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
                                         acf_peak = .x$acf$results$max_peak_of_int,
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
                                     lsp_peak = .x$lomb$results$peak,
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
    
    #### Tibble utils data ####
    utils_data = future_map_dfr(.x = df,
                                .id = 'data',
                                .f = ~ {
                                  tibble(
                                    datetime = .x$data$datetime,
                                    acf = .x$acf$results$autocorrelation,
                                    acf_period = .x$acf$results$period,
                                    acf_rs = .x$acf$results$rythm_strength,
                                    acf_peak = .x$acf$results$max_peak_of_int,
                                    acf_peak_time = .x$acf$results$datetime,
                                    lsp_period = .x$lomb$results$period,
                                    lsp_peak = .x$lomb$results$peak,
                                    lsp_sig_level = .x$lomb$results$sig_level,
                                    lsp_p_value = .x$lomb$results$p_value,
                                    lsp_scanned = list(.x$lomb$results$scanned),
                                    lsp_power = list(.x$lomb$results$power),
                                    lsp_rs = .x$lomb$results$rythm_strength,
                                    acf_start = .x$acf$results$start,
                                    acf_end = .x$acf$results$end,
                                    acf_from = .x$acf$results$from,
                                    acf_to = .x$acf$results$to,
                                    lsp_phase = .x$lomb$cosinor$phase,
                                    acf_phase = .x$acf$cosinor$phase,
                                    acf_amp = .x$acf$cosinor$amplitude,
                                    acf_pr = .x$acf$cosinor$adj_r_squared,
                                    lsp_amp = .x$lomb$cosinor$amplitude,
                                    lsp_pr = .x$lomb$cosinor$adj_r_squared,
                                    lsp_gc = ifelse(.x$lomb$results$grangercausal$rawdata_to_cos < .x$lomb$results$grangercausal$cos_to_rawdata, .x$lomb$results$grangercausal$rawdata_to_cos, .x$lomb$results$grangercausal$cos_to_rawdata),
                                    acf_gc = ifelse(.x$acf$results$grangercausal$rawdata_to_cos < .x$acf$results$grangercausal$cos_to_rawdata, .x$acf$results$grangercausal$rawdata_to_cos, .x$acf$results$grangercausal$cos_to_rawdata)
                                    
                                  )
                                  
                                })
    
  }
  return(
    list(
      data = processed_data,
      autocorrelation = autocorrelation,
      lombscargle = lombscargle,
      utils = utils_data
    )
  )
}
