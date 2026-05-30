test_that("simplify_data tidies pipeline output with canonical names and no Granger cols", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries.main(df, make_windows = FALSE, sampling_rate = "1 hour",
                                 detrend_data = FALSE, butterworth = FALSE)
  tidy <- simplify_data(out)
  expect_named(tidy, c("data", "autocorrelation", "lombscargle", "utils"))
  expect_true("acf_peak" %in% names(tidy$autocorrelation))
  expect_true("lsp_peak" %in% names(tidy$lombscargle))
  expect_true("lsp_power" %in% names(tidy$utils))
  expect_false(any(grepl("^gc_|_gc$", names(tidy$autocorrelation))))
  expect_false(any(grepl("^gc_|_gc$", names(tidy$utils))))
})
