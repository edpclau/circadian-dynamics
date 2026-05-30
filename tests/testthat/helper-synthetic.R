# Deterministic synthetic circadian signal for tests.
# A clean cosine: value = mesor + amplitude * cos(2*pi*(t_hours - phase_h)/period_h),
# optionally with reproducible Gaussian noise.
make_sine <- function(period_h = 24, n_days = 6, sampling_min = 60,
                      amplitude = 2, mesor = 5, phase_h = 6,
                      noise_sd = 0, seed = 42) {
  set.seed(seed)
  step_sec <- sampling_min * 60
  n <- n_days * 24 * 60 / sampling_min
  dt <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
            by = step_sec, length.out = n)
  t_h <- as.numeric(difftime(dt, dt[1], units = "hours"))
  value <- mesor + amplitude * cos(2 * pi * (t_h - phase_h) / period_h)
  if (noise_sd > 0) value <- value + rnorm(n, 0, noise_sd)
  tibble::tibble(datetime = dt, value = value)
}
