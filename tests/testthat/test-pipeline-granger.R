test_that("core pipeline output contains no grangercausal field", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  out <- process_timeseries_core(
    df = df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  expect_null(out$acf$results$grangercausal)
  expect_null(out$lomb$results$grangercausal)
})
