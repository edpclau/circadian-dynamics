test_that("process_timeseries_core does not leak a global future plan change", {
  future::plan(future::sequential)
  before <- class(future::plan())
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  invisible(process_timeseries_core(
    df = df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, big_data = TRUE
  ))
  after <- class(future::plan())
  expect_identical(after, before)
})

test_that("process_timeseries_main does not leak a global future plan change", {
  future::plan(future::sequential)
  before <- class(future::plan())
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  invisible(process_timeseries_main(
    df = df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, big_data = FALSE
  ))
  expect_identical(class(future::plan()), before)
})
