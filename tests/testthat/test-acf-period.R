test_that("ACF recovers ~24h period at 1-hour sampling (bin = 1)", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 8)
  res <- analyze_acf(df, from = 18, to = 30, sampling_rate = "1 hour")
  expect_equal(res$period, 24, tolerance = 1)
})

test_that("ACF recovers ~24h period at 30-min sampling (bin = 30) — sampling_bin_size scaling", {
  df <- make_sine(period_h = 24, sampling_min = 30, n_days = 8)
  res <- analyze_acf(df, from = 18, to = 30, sampling_rate = "30 minutes")
  expect_equal(res$period, 24, tolerance = 1)
})

test_that("ACF recovers ~24h period at 15-min sampling (bin = 15)", {
  df <- make_sine(period_h = 24, sampling_min = 15, n_days = 8)
  res <- analyze_acf(df, from = 18, to = 30, sampling_rate = "15 minutes")
  expect_equal(res$period, 24, tolerance = 1)
})
