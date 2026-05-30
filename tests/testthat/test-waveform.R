test_that("waveform smoothing path runs without an undefined binning_n", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  expect_no_error(
    process_timeseries.waveform(
      df, detrend_data = FALSE, smooth_data = TRUE,
      butterworth = FALSE, binning_n = 4
    )
  )
})
