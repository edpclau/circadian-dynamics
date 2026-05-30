test_that("the canonical pipeline runs end-to-end after legacy removal", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries.main(
    df = df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  expect_true(is.list(out))
  expect_equal(out$ind1$lomb$results$period, 24, tolerance = 1)
})
