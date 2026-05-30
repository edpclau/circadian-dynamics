test_that("cosinor recovers amplitude/mesor with non-unit bin size (30 min)", {
  df <- make_sine(period_h = 24, sampling_min = 30, amplitude = 2, mesor = 5)
  res <- analyze_timeseries.cosinor(df, sampling_rate = "30 minutes", period = 24)
  expect_equal(res$mesor, 5, tolerance = 0.05)
  expect_equal(res$amplitude, 2, tolerance = 0.05)
})
