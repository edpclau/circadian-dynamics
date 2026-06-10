# Onset detection + free-running period (tau) estimation.
# Synthetic cosines have a known period, so onset drift must recover it:
# a 24 h signal => tau ~ 24, a 25 h signal => tau ~ 25 (onsets drift +1 h/day).

test_that("detect_onsets finds ~one onset per cycle as rising threshold crossings", {
  d <- make_sine(period_h = 24, n_days = 8, sampling_min = 30)
  on <- detect_onsets(d, sampling_rate = "30 minutes")
  # ~one onset per 24 h cycle over 8 days (allow +/-1 for edges)
  expect_true(nrow(on) >= 6 && nrow(on) <= 9)
  expect_true(all(c("onset_index", "onset_datetime", "onset_hours") %in% names(on)))
  # onsets are monotonically increasing in time
  expect_false(is.unsorted(on$onset_hours))
})

test_that("estimate_tau recovers the free-running period from onset drift", {
  d24 <- make_sine(period_h = 24, n_days = 12, sampling_min = 30)
  d25 <- make_sine(period_h = 25, n_days = 12, sampling_min = 30)
  expect_equal(estimate_tau(detect_onsets(d24, "30 minutes"))$tau, 24, tolerance = 0.3)
  expect_equal(estimate_tau(detect_onsets(d25, "30 minutes"))$tau, 25, tolerance = 0.5)
})
