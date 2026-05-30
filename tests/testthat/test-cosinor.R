test_that("cosinor recovers amplitude/mesor with non-unit bin size (30 min)", {
  df <- make_sine(period_h = 24, sampling_min = 30, amplitude = 2, mesor = 5)
  res <- analyze_timeseries.cosinor(df, sampling_rate = "30 minutes", period = 24)
  expect_equal(res$mesor, 5, tolerance = 0.05)
  expect_equal(res$amplitude, 2, tolerance = 0.05)
})

test_that("amplitude_se scales linearly with signal magnitude (delta method / amplitude)", {
  df1 <- make_sine(amplitude = 2, mesor = 5, noise_sd = 0.3, seed = 1)
  df2 <- df1
  df2$value <- df2$value * 2  # exact 2x scaling of signal + noise
  r1 <- analyze_timeseries.cosinor(df1, sampling_rate = "1 hour", period = 24)
  r2 <- analyze_timeseries.cosinor(df2, sampling_rate = "1 hour", period = 24)
  # Correct SE is linear in scale -> ratio 2. The old (/amplitude^2) bug gives ratio 1.
  expect_equal(r2$amplitude_se / r1$amplitude_se, 2, tolerance = 1e-4)
  expect_true(is.finite(r1$amplitude_se) && r1$amplitude_se > 0)
})

test_that("acrophase is in [0, 2*pi) and finite", {
  df <- make_sine(amplitude = 2, mesor = 5, phase_h = 6, noise_sd = 0.1)
  res <- analyze_timeseries.cosinor(df, sampling_rate = "1 hour", period = 24)
  expect_true(is.finite(res$acrophase))
  expect_gte(res$acrophase, 0)
  expect_lt(res$acrophase, 2 * pi)
  expect_true(is.finite(res$acrophase_se) && res$acrophase_se > 0)
})
