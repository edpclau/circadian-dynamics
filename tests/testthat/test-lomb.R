test_that("Lomb warns and caps an implausibly large ofac, still finds ~24h", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  expect_warning(
    res <- analyze_lomb(df, sampling_rate = "1 hour",
                                   from = 18, to = 30, ofac = 60),
    regexp = "ofac"
  )
  expect_equal(res$period, 24, tolerance = 1)
})

test_that("Lomb relative_power is the peak's share of total spectral power, in (0,1]", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  res <- analyze_lomb(df, sampling_rate = "1 hour", from = 18, to = 30, ofac = 1)
  expect_equal(res$relative_power, res$peak / sum(res$power, na.rm = TRUE))
  expect_gt(res$relative_power, 0)
  expect_lte(res$relative_power, 1)
  expect_null(res$rhythm_strength)  # the old significance-ratio metric is gone
})
