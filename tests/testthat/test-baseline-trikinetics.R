# Real-data regression guardrail. Periods below were validated against the
# maintainer's ground-truth knowledge of data/trikinetics.rda on 2026-05-30.
# Any refactor (1B structural cleanup, 1C vectorization) MUST keep these stable.
# Generous tolerances absorb legitimate minor algorithmic changes while catching
# real regressions. 15-min binning is used (1-min is overkill; period is unchanged).

bin15 <- function(d) {
  d$datetime <- lubridate::floor_date(d$datetime, "15 minutes")
  dplyr::summarise(dplyr::group_by(d, datetime),
                   value = sum(value, na.rm = TRUE), .groups = "drop")
}

test_that("trikinetics periods match the ground-truth-validated baseline", {
  data("trikinetics", package = "circadiandynamics", envir = environment())
  sub <- lapply(trikinetics[paste("IND", c(1, 2, 3, 4))], bin15)
  res <- process_timeseries.main(
    sub, make_windows = FALSE, sampling_rate = "15 minutes",
    from = 15, to = 33, detrend_data = FALSE, butterworth = TRUE,
    f_low = 1/(12*4), f_high = 1/(35*4), order = 2, ofac = 1
  )

  # IND 1: dead/empty channel -> arrhythmic (no significant LSP peak).
  expect_gt(res[["IND 1"]]$lomb$results$p_value, 0.05)

  # IND 2: strongly rhythmic; ACF and LSP agree ~23.8 h.
  expect_equal(res[["IND 2"]]$lomb$results$period, 23.9, tolerance = 0.6)
  expect_equal(res[["IND 2"]]$acf$results$period,  23.8, tolerance = 1.2)

  # IND 4: LSP ~25.9 h (ACF ~28 h is a confirmed method difference, not asserted tightly).
  expect_equal(res[["IND 4"]]$lomb$results$period, 25.9, tolerance = 0.8)
})
