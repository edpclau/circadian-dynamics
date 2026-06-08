test_that("Lomb warns and caps an implausibly large ofac, still finds ~24h", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  expect_warning(
    res <- analyze_lomb(df, sampling_rate = "1 hour",
                                   from = 18, to = 30, ofac = 60),
    regexp = "ofac"
  )
  expect_equal(res$period, 24, tolerance = 1)
})

test_that("Lomb rhythm strength is the peak power relative to the significance threshold", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  res <- analyze_lomb(df, sampling_rate = "1 hour", from = 18, to = 30, ofac = 1)
  expect_equal(res$rhythm_strength, res$peak / res$sig_level)
  expect_gt(res$rhythm_strength, 1)  # a clean 24h sine is clearly significant
})
