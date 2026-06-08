test_that("result plotters and detailed_plots run on real pipeline output (no Granger)", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries.main(
    df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  tidy <- simplify_data(out)

  acf_plots <- plot_acf_results(tidy$autocorrelation)
  lsp_plots <- plot_lsp_results(tidy$lombscargle)

  expect_named(acf_plots, c("period_plots", "rhythm_plots"))
  expect_named(lsp_plots, c("period_plots", "rhythm_plots", "amplitude_plots", "phase_plots"))
  expect_s3_class(acf_plots$period_plots$ind1, "ggplot")
  expect_s3_class(lsp_plots$phase_plots$ind1, "ggplot")

  # detailed_plots writes a PDF; run in a temp dir so it doesn't litter.
  wd <- getwd()
  tmp <- tempfile("plots")
  dir.create(tmp)
  setwd(tmp)
  on.exit({ setwd(wd); unlink(tmp, recursive = TRUE) }, add = TRUE)
  expect_no_error(detailed_plots(out, sampling_rate = "hours", windows = FALSE))
  expect_true(file.exists("Window_plots.pdf"))
})
