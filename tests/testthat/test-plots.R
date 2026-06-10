test_that("result plotters and detailed_plots run on real pipeline output (no Granger)", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries_main(
    df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  tidy <- simplify_data(out)

  acf_plots <- plot_acf_results(tidy$autocorrelation)
  lsp_plots <- plot_lsp_results(tidy$lombscargle)

  expect_named(acf_plots, c("period_plots", "rhythm_plots"))
  expect_named(lsp_plots, c("period_plots", "relative_power_plots", "amplitude_plots", "phase_plots"))
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

# Does a ggplot carry at least one layer drawn with the given Geom?
.has_geom <- function(p, geom) {
  any(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))
}

test_that("cosinor amplitude/phase window plots carry SE-derived error bars; metrics without SE do not", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries_main(
    df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  tidy <- simplify_data(out)
  lsp_plots <- plot_lsp_results(tidy$lombscargle)

  # Amplitude and phase have a standard error (amp_se / phase_se) -> show uncertainty.
  expect_true(.has_geom(lsp_plots$amplitude_plots$ind1, "GeomErrorbar"))
  expect_true(.has_geom(lsp_plots$phase_plots$ind1, "GeomErrorbar"))
  # Period and relative power have no SE column -> no error bars invented.
  expect_false(.has_geom(lsp_plots$period_plots$ind1, "GeomErrorbar"))
  expect_false(.has_geom(lsp_plots$relative_power_plots$ind1, "GeomErrorbar"))
})

test_that("plot_raw_values shades the dark phase with a full-height background band, not greyscale tiles", {
  df <- tibble::tibble(
    data = "ind1",
    datetime = seq(as.POSIXct("2020-01-02 00:00", tz = "UTC"), by = "1 hour", length.out = 72),
    raw_values = rep(c(1:12, 12:1), 3),
    ld = rep(c(rep(1, 12), rep(0, 12)), 3)  # 12 h light : 12 h dark
  )
  plots <- plot_raw_values(df)

  expect_s3_class(plots$ind1, "ggplot")
  expect_true(.has_geom(plots$ind1, "GeomRect"))   # dark-phase background band
  expect_true(.has_geom(plots$ind1, "GeomLine"))   # the activity trace on top
  expect_false(.has_geom(plots$ind1, "GeomTile"))  # no more variable-height tile hack
})

test_that("plot_actogram draws a double-plotted actogram with onset markers per individual", {
  t <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "30 min", length.out = 48 * 7)  # 7 days
  h <- as.numeric(difftime(t, t[1], units = "hours"))
  val <- pmax(0, cos(2 * pi * (h - 6) / 24)) * 10  # positive, rhythmic, 24 h
  df <- data.frame(data = "ind1", datetime = t, raw_values = val)

  plots <- plot_actogram(df, sampling_rate = "30 minutes")
  expect_s3_class(plots$ind1, "ggplot")
  expect_true(.has_geom(plots$ind1, "GeomSegment"))  # activity bars
  expect_true(.has_geom(plots$ind1, "GeomPoint"))    # onset markers
})

test_that("plot_actogram shades the dark phase when an ld column is present", {
  t <- seq(as.POSIXct("2020-01-01 00:00", tz = "UTC"), by = "30 min", length.out = 48 * 7)
  hod <- as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60
  ld <- as.integer(hod >= 6 & hod < 18)        # light 06:00-18:00, dark otherwise
  val <- ifelse(ld == 0, 10, 1)                # nocturnal: active in the dark
  df <- data.frame(data = "ind1", datetime = t, raw_values = val, ld = ld)

  with_ld <- plot_actogram(df, sampling_rate = "30 minutes")$ind1
  no_ld   <- plot_actogram(df[, c("data", "datetime", "raw_values")], sampling_rate = "30 minutes")$ind1

  expect_true(.has_geom(with_ld, "GeomRect"))   # dark-phase background shading
  expect_false(.has_geom(no_ld, "GeomRect"))    # none invented without ld
  expect_true(.has_geom(with_ld, "GeomSegment")) # activity bars still drawn
})

test_that("plot_acrophase_polar draws the acrophase vector and a 95% confidence ellipse", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries_main(
    df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  tidy <- simplify_data(out)
  # the joint ellipse needs the cosinor coefficients + their covariance plumbed through
  expect_true(all(c("cos_coeff", "sin_coeff", "var_sin", "var_cos", "cov_sincos", "cosinor_n")
                  %in% names(tidy$lombscargle)))

  pp <- plot_acrophase_polar(tidy$lombscargle)
  expect_s3_class(pp$ind1, "ggplot")
  expect_true(.has_geom(pp$ind1, "GeomPath"))     # confidence ellipse (+ reference rings)
  expect_true(.has_geom(pp$ind1, "GeomSegment"))  # the acrophase/amplitude vector
})

test_that("okabe_ito returns the canonical colourblind-safe hexes in order", {
  expect_equal(okabe_ito(2), c("#E69F00", "#56B4E9"))
  expect_equal(length(okabe_ito(8)), 8L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", okabe_ito(8))))
  expect_equal(okabe_ito(8)[8], "#000000")
  expect_error(okabe_ito(9))
})

test_that("plot_acf_correlogram draws autocorrelation vs lag with white-noise bounds and a peak marker", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries_main(
    df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  tidy <- simplify_data(out)
  cg <- plot_acf_correlogram(tidy$utils)

  expect_s3_class(cg$ind1, "ggplot")
  expect_true(.has_geom(cg$ind1, "GeomLine"))   # the correlogram
  expect_true(.has_geom(cg$ind1, "GeomHline"))  # +/- white-noise bounds
  expect_true(.has_geom(cg$ind1, "GeomPoint"))  # period peak (height = RI)
  expect_identical(rlang::as_label(cg$ind1$mapping$x), "lag_hours")
  expect_identical(rlang::as_label(cg$ind1$mapping$y), "acf")
})

test_that("plot_periodogram draws power vs period with a significance threshold and peak", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries_main(
    df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  tidy <- simplify_data(out)
  pg <- plot_periodogram(tidy$utils)

  expect_s3_class(pg$ind1, "ggplot")
  expect_true(.has_geom(pg$ind1, "GeomLine"))   # the spectrum
  expect_true(.has_geom(pg$ind1, "GeomHline"))  # significance threshold
  expect_true(.has_geom(pg$ind1, "GeomPoint"))  # annotated peak
  # x maps period, y maps power (period on x is the chronobiology convention)
  expect_identical(rlang::as_label(pg$ind1$mapping$x), "period")
  expect_identical(rlang::as_label(pg$ind1$mapping$y), "power")
})
