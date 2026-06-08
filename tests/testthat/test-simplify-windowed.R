# Characterization test for simplify_data on the WINDOWED pipeline output.
# Pins the structure so the branch-merge refactor cannot drift it.
test_that("simplify_data tidies windowed output with a window column and stable schema", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries_main(
    df, make_windows = TRUE, window_size_in_days = 3, window_step_in_days = 1,
    sampling_rate = "1 hour", detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  tidy <- simplify_data(out)

  expect_named(tidy, c("data", "autocorrelation", "lombscargle", "utils"))

  # every table is keyed by individual + window
  for (tbl in tidy) {
    expect_true(all(c("data", "window") %in% names(tbl)))
  }
  # more than one window was produced and unpacked
  expect_gt(length(unique(tidy$autocorrelation$window)), 1)

  expect_named(tidy$autocorrelation, c(
    "data", "window", "peak_datetime", "period", "rhythm_strength", "acf_peak",
    "mesor", "amplitude", "amp_se", "acrophase", "acro_se", "phase", "phase_se",
    "adj_r_squared", "cosinor_p_value"
  ))
  # NB: lomb results carry no $datetime, so tibble() drops the NULL peak_datetime
  # column here (unlike the autocorrelation table). This is existing behaviour.
  expect_named(tidy$lombscargle, c(
    "data", "window", "period", "rhythm_strength", "lsp_peak",
    "mesor", "amplitude", "amp_se", "acrophase", "acro_se", "phase", "phase_se",
    "adj_r_squared", "cosinor_p_value"
  ))
  # no Granger columns survive anywhere
  expect_false(any(grepl("^gc_|_gc$|granger", names(tidy$lombscargle), ignore.case = TRUE)))
})
