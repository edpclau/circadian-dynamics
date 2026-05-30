test_that("ACF rhythm_strength equals max peak over the 95% white-noise bound", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  res <- analyze_timeseries.acf(df, from = 18, to = 30, sampling_rate = "1 hour")
  # Definition: max_peak_of_int / (1.965 / sqrt(n))
  expected <- res$max_peak_of_int / (1.965 / sqrt(nrow(df)))
  expect_equal(res$rhythm_strength, expected, tolerance = 1e-8)
  expect_true(res$rhythm_strength > 1)  # clean signal is clearly rhythmic
})

test_that("ACF rhythm_strength is NA for a flat (zero-variance) signal", {
  flat <- make_sine(amplitude = 0)  # constant value -> zero variance
  res <- analyze_timeseries.acf(flat, from = 18, to = 30, sampling_rate = "1 hour")
  expect_true(is.na(res$rhythm_strength))
})
