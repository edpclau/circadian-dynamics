# Regression tests for the correctness bugs fixed in the 2026-06-08 code review.
# The pre-existing suite used tolerance = 1 h, which masked the off-by-one and the
# duration() double-wrap, so these silent-wrong-number bugs had no real coverage.

test_that("analyze_lomb analyses the last (processed) column, not raw value (F3)", {
  s <- make_sine(period_h = 24, sampling_min = 60, n_days = 8)
  set.seed(1)
  # raw `value` is pure noise; the processed last column carries the 24 h rhythm.
  df <- data.frame(value = rnorm(nrow(s)), processed = s$value)
  res <- analyze_lomb(df, sampling_rate = "1 hour", from = 18, to = 30, ofac = 1)
  expect_equal(res$period, 24, tolerance = 1)   # would be NA/garbage if it read `value`
})

test_that("ACF period has no off-by-one at hourly sampling (F7)", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 10)
  res <- analyze_acf(df, from = 18, to = 30, sampling_rate = "1 hour")
  expect_equal(res$period, 24, tolerance = 0.01)  # exactly 24 h, previously 25 h
})

test_that("ACF peak datetime is a sane hour value, not an astronomical wrap (F8)", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 10)
  res <- analyze_acf(df, from = 18, to = 30, sampling_rate = "1 hour")
  expect_true(is.finite(res$datetime))
  expect_lt(res$datetime, 24 * 14)   # ~48 h after fix; was ~176400 h before
})

test_that("ACF and Lomb guards survive windows containing NAs (F1/F2)", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 8)
  df$value[c(5, 20, 100)] <- NA
  expect_no_error(analyze_acf(df, from = 18, to = 30, sampling_rate = "1 hour"))
  expect_no_error(analyze_lomb(data.frame(value = df$value),
                               sampling_rate = "1 hour", from = 18, to = 30, ofac = 1))
})

test_that("analyze_lomb reports no significance when no peak falls in band (F4)", {
  # A clean 24 h rhythm searched in a band that excludes 24 h -> no in-band peak.
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 8)
  res <- analyze_lomb(df, sampling_rate = "1 hour", from = 10, to = 14, ofac = 1)
  expect_true(is.na(res$period))
  expect_true(is.na(res$p_value))          # must NOT leak the global out-of-band p-value
  expect_true(is.na(res$relative_power))
})

test_that("rm_inactive keeps a continuously active individual (F16)", {
  n <- 48; dt <- as.POSIXct("2022-01-01", tz = "UTC") + 0:(n - 1) * 3600
  df <- data.frame(datetime = dt,
                   active = rep(c(3, 4, 5, 6), length.out = n),  # never zero
                   dead   = c(rep(2, 3), rep(0, n - 3)))         # long zero run
  keep <- rm_inactive(df, inactivity_period = "1 day", sampling_rate = "1 hour")
  expect_true("active" %in% names(keep))
  expect_false("dead" %in% names(keep))
})

test_that("rm_inactive_dates returns empty when inactivity starts at row 1 (F15)", {
  n <- 48; dt <- as.POSIXct("2022-01-01", tz = "UTC") + 0:(n - 1) * 3600
  d <- list(a = data.frame(datetime = dt, value = c(rep(0, 30), rep(7, 18))))
  out <- rm_inactive_dates(d, inactivity_period = "1 day", sampling_rate = "1 hour")
  expect_equal(nrow(out$a), 0)
})

test_that("report_inactive_variables emits no literal 'NULL' for active individuals (F17)", {
  n <- 48; dt <- as.POSIXct("2022-01-01", tz = "UTC") + 0:(n - 1) * 3600
  d <- list(act  = data.frame(datetime = dt, value = rep(c(1, 2, 3), length.out = n)),
            dead = data.frame(datetime = dt, value = c(rep(0, 30), rep(1, 18))))
  inactive <- report_inactive_variables(d, inactivity_period = "1 day", sampling_rate = "1 hour")
  expect_false("NULL" %in% inactive)
  expect_true("dead" %in% inactive)
})

test_that("crop_data with only `to` does not silently discard all rows (F18)", {
  n <- 48; dt <- as.POSIXct("2022-01-01 00:00:00", tz = "UTC") + 0:(n - 1) * 3600
  d <- list(x = data.frame(datetime = dt, value = seq_len(n)))
  cr <- crop_data(d, to = as.character(dt[24]))
  expect_gt(nrow(cr$x), 0)
})

test_that("average_of_group works when the first column is not named 'datetime' (F23)", {
  n <- 10; ts <- as.POSIXct("2022-01-01", tz = "UTC") + 0:(n - 1) * 3600
  df <- data.frame(time = ts, a = 1:n, b = (1:n) * 2)
  out <- average_of_group(df)
  expect_true("mean" %in% names(out))
  expect_equal(out$mean, (df$a + df$b) / 2)
})
