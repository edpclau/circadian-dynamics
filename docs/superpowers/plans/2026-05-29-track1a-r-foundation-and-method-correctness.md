# Track 1A — R Foundation & Method Correctness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Establish a test harness for the `circadiandynamics` R package and fix every statistical/implementation error, producing a tested package whose analysis methods are numerically trustworthy (Gate A).

**Architecture:** Add `testthat` + a synthetic-signal fixture so every correction is TDD-driven. Then fix the period-scaling bug, amplitude/acrophase standard-error formulas, Granger removal, the `binning_n` latent bug, `future::plan()` side effects, Lomb `ofac` handling, rhythm-strength documentation, and add FDR adjustment. Produce the canonical-decisions doc that Gate A and Track 2 depend on.

**Tech Stack:** R, `testthat` (3e), `devtools`, `usethis`, `lubridate`, `broom`, `lomb`, `pracma`, `furrr`/`future`.

---

## Context for the engineer (zero-context assumptions)

- This is an R **package**. Source lives in `R/*.R`; docs are roxygen comments compiled to `man/*.Rd`; exports are in `NAMESPACE`.
- **No system R on this machine — the R toolchain lives in a pixi env.** Prefix EVERY R command with `pixi run`. `devtools`, `testthat`, `usethis`, `roxygen2`, `lomb`, and all `DESCRIPTION` imports are installed there. (`lomb` is CRAN-only; if the env is ever rebuilt, restore it with `pixi run setup-cran`.)
- Run a single test file with:
  `pixi run Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/<file>.R")'`
- Run the whole suite with: `pixi run Rscript -e 'devtools::test()'` (or `pixi run test`)
- After editing roxygen, regenerate docs with: `pixi run Rscript -e 'devtools::document()'` (or `pixi run document`)
- `R CMD check` runs via `pixi run check`.
- Background reading (this repo): `research/01-library-review.md` lists every issue; the umbrella spec is `docs/superpowers/specs/2026-05-29-circadiandynamics-three-track-overhaul-design.md`.
- **Branch:** all work happens on `refactor/r-api-cleanup`, created in Task 1 off the current `planning/three-track-overhaul` branch (so the spec/plan/research come along).

## File structure (created or modified by this plan)

- Create: `tests/testthat.R`, `tests/testthat/helper-synthetic.R`
- Create: `tests/testthat/test-cosinor.R`, `test-pipeline-granger.R`, `test-waveform.R`, `test-future-plan.R`, `test-lomb.R`, `test-rhythm-strength.R`, `test-fdr.R`
- Create: `R/adjust_pvalues.R`, `man/adjust_pvalues.Rd`
- Create: `docs/superpowers/canonical-decisions-track1.md`
- Modify: `R/analyze_timeseries.cosinor.R` (period scaling, amplitude_se, acrophase via atan2, acrophase_se)
- Modify: `R/main.R` (remove Granger calls; fix `binning_n`; `future::plan()` restore)
- Modify: `R/process_timeseries.R` (remove `future::plan()` side effect)
- Modify: `R/read_vitalpatch.R` (remove `future::plan()` side effect)
- Modify: `R/analyze_timeseries.lomb.R` (ofac sanity)
- Modify: `R/analyze_timeseries.acf.R` (rhythm_strength roxygen)
- Modify: `DESCRIPTION` (add `Suggests: testthat (>= 3.0.0)`, `Config/testthat/edition: 3`)
- Modify: `NAMESPACE` (drop Granger export; add `adjust_pvalues`) — via `devtools::document()`

---

### Task 1: Branch, test scaffolding, synthetic fixture

**Files:**
- Create: `tests/testthat.R`, `tests/testthat/helper-synthetic.R`
- Modify: `DESCRIPTION`

- [ ] **Step 1: Create the working branch**

```bash
git checkout planning/three-track-overhaul
git checkout -b refactor/r-api-cleanup
```

- [ ] **Step 2: Enable testthat**

Run: `Rscript -e 'usethis::use_testthat(3)'`
Expected: creates `tests/testthat/` and `tests/testthat.R`, and adds `testthat (>= 3.0.0)` to `Suggests` plus `Config/testthat/edition: 3` in `DESCRIPTION`. If `usethis` is unavailable, create `tests/testthat.R` containing:

```r
library(testthat)
library(circadiandynamics)

test_check("circadiandynamics")
```

and add to `DESCRIPTION`:

```
Suggests:
    testthat (>= 3.0.0)
Config/testthat/edition: 3
```

- [ ] **Step 3: Write the synthetic-signal fixture helper**

Create `tests/testthat/helper-synthetic.R`:

```r
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
```

- [ ] **Step 4: Verify the harness loads**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: runs with 0 failures (no test files yet, or only passing ones). No error about missing testthat.

- [ ] **Step 5: Commit**

```bash
git add tests/ DESCRIPTION
git commit -m "test: add testthat harness and synthetic-signal fixture"
```

---

### Task 2: Canonical-decisions doc (Gate A input)

**Files:**
- Create: `docs/superpowers/canonical-decisions-track1.md`

This is a documentation task (no test). It records the de-duplication and naming decisions so the Plan 1B swarm cannot collide. It does **not** delete code yet — that is Plan 1B.

- [ ] **Step 1: Write the decisions doc**

Create `docs/superpowers/canonical-decisions-track1.md` with a table that resolves every duplicate pair. Use this exact content as the starting point; fill the "Winner" column by inspecting each pair in `R/`:

```markdown
# Track 1 Canonical Decisions (Gate A)

For each duplicated function, exactly one survives Plan 1B. The other is deleted and its
`export()` removed from NAMESPACE. The "newer/working" implementation referenced by
`Example Script.R` and `README.md` wins unless inspection shows otherwise.

| Pair | Winner (keep) | Remove | Notes |
|---|---|---|---|
| make_time_windows / _2 | make_time_windows_2 | make_time_windows | _2 used by process_timeseries.core |
| butterworth_filter / _2 | butterworth_filter_2 | butterworth_filter | _2 used by process_timeseries.waveform |
| downsample_time_series / _2 | downsample_time_series_2 | downsample_time_series | confirm callers |
| read_trikinetics / _2 | read_trikinetics_2 | read_trikinetics | _2 used by README/Example |
| rythm_analysis_by_window / _2 | rythm_analysis_by_window_2 | rythm_analysis_by_window | confirm callers |
| smooth_detrend_by_windows / _2 | smooth_detrend_by_windows (used via smooth_and_detrend) | _2 | confirm callers |
| simplify_data / simplify_data2 | simplify_data2 | simplify_data | latest commit added simplify_data2 |
| plot_actogram / plot_actogram2 | plot_actogram2 | plot_actogram | confirm callers |
| cosinor_lm / analyze_timeseries.cosinor | analyze_timeseries.cosinor | cosinor_lm | core pipeline uses analyze_timeseries.cosinor |

## Naming convention (final, applied in Plan 1B)
- Drop the `_2` suffixes after the winner is chosen (e.g. `read_trikinetics_2` -> `read_trikinetics`).
- "Rythm" -> "rhythm" everywhere (function names, args, docs).
- Core analysis entry points must be non-interactive: no `file.choose()` / `rstudioapi` inside
  analysis or core read functions; interactive pickers move to thin `*_interactive()` wrappers.

## Method-correctness decisions (implemented in Plan 1A, Tasks 3-10)
- analyze_timeseries.cosinor period MUST be divided by sampling_bin_size (Task 3).
- amplitude_se uses the delta method divided by amplitude (not amplitude^2) (Task 4).
- acrophase uses atan2; acrophase_se uses the delta method (Task 4).
- Granger test removed from the core pipeline (Task 5).
- rhythm_strength definitions documented; Lomb variant marked experimental (Task 9).
- p-values get a BH/FDR adjustment helper across the individual x window grid (Task 10).
```

- [ ] **Step 2: Verify caller assumptions for each pair**

Run: `Rscript -e 'cat(system("grep -rn \"make_time_windows\\|butterworth_filter\\|downsample_time_series\\|read_trikinetics\\|rythm_analysis_by_window\\|smooth_detrend_by_windows\\|simplify_data\\|plot_actogram\\|cosinor_lm\" R/", intern = TRUE), sep = "\n")'`
Expected: a list of call sites. For any row where the actual caller contradicts the "Winner" column, correct the table.

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/canonical-decisions-track1.md
git commit -m "docs: canonical de-duplication and method decisions for Track 1 (Gate A)"
```

---

### Task 3: Fix cosinor period-scaling bug

**Problem:** `analyze_timeseries.cosinor` converts the period to sampling units but, unlike `cosinor_lm`, never divides by `sampling_bin_size`. For any sampling rate whose bin size is not 1 (e.g. `"30 minutes"`), the period is wrong by a factor of `sampling_bin_size`, so the fit is garbage. The README example uses `"1 minute"` (bin = 1), which masks the bug.

**Files:**
- Test: `tests/testthat/test-cosinor.R`
- Modify: `R/analyze_timeseries.cosinor.R:103`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-cosinor.R`:

```r
test_that("cosinor recovers amplitude/mesor with non-unit bin size (30 min)", {
  df <- make_sine(period_h = 24, sampling_min = 30, amplitude = 2, mesor = 5)
  res <- analyze_timeseries.cosinor(df, sampling_rate = "30 minutes", period = 24)
  expect_equal(res$mesor, 5, tolerance = 0.05)
  expect_equal(res$amplitude, 2, tolerance = 0.05)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cosinor.R")'`
Expected: FAIL — recovered amplitude is far from 2 (period mis-scaled by 30x).

- [ ] **Step 3: Fix the period scaling**

In `R/analyze_timeseries.cosinor.R`, change the period line (currently line 103):

```r
  #3. Period must be in the correct sampling_rate
  period = as.numeric(lubridate::duration(period, 'hours'), sampling_rate)
```

to:

```r
  #3. Period must be in the correct sampling_rate (in number of samples per cycle)
  period = as.numeric(lubridate::duration(period, 'hours'), sampling_rate) / sampling_bin_size
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cosinor.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/analyze_timeseries.cosinor.R tests/testthat/test-cosinor.R
git commit -m "fix: divide cosinor period by sampling_bin_size (fixes non-unit sampling rates)"
```

---

### Task 4: Fix amplitude/acrophase standard errors and use atan2

**Problem:** `amplitude_se` divides by `amplitude^2` (should be `amplitude`), so its scaling with signal magnitude is wrong. The acrophase quadrant logic is fragile hand-rolled branching; replace with `atan2`, and give `acrophase_se` a correct delta-method form using the model covariance.

**Files:**
- Test: `tests/testthat/test-cosinor.R` (append)
- Modify: `R/analyze_timeseries.cosinor.R:120-158`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-cosinor.R`:

```r
test_that("amplitude_se scales linearly with signal magnitude (delta method / amplitude)", {
  df1 <- make_sine(amplitude = 2, mesor = 5, noise_sd = 0.3, seed = 1)
  df2 <- df1
  df2$value <- df2$value * 2  # exact 2x scaling of signal + noise
  r1 <- analyze_timeseries.cosinor(df1, sampling_rate = "1 hour", period = 24)
  r2 <- analyze_timeseries.cosinor(df2, sampling_rate = "1 hour", period = 24)
  # Correct SE is linear in scale -> ratio 2. The old (/amplitude^2) bug gives ratio 1.
  expect_equal(r2$amplitude_se / r1$amplitude_se, 2, tolerance = 1e-4)
  expect_true(is.finite(r1$amplitude_se) && r1$amplitude_se > 0)
})

test_that("acrophase is in [0, 2*pi) and finite", {
  df <- make_sine(amplitude = 2, mesor = 5, phase_h = 6, noise_sd = 0.1)
  res <- analyze_timeseries.cosinor(df, sampling_rate = "1 hour", period = 24)
  expect_true(is.finite(res$acrophase))
  expect_gte(res$acrophase, 0)
  expect_lt(res$acrophase, 2 * pi)
  expect_true(is.finite(res$acrophase_se) && res$acrophase_se > 0)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cosinor.R")'`
Expected: FAIL — the amplitude_se ratio is ~1 (bug), not 2.

- [ ] **Step 3: Replace the amplitude/phase block**

In `R/analyze_timeseries.cosinor.R`, replace the block from the amplitude calculation through the manual quadrant `if` statements (currently lines ~126-148):

```r
  # Calculating Amplitude and phase
  # Amplitude of the function = square root of (sin_coeff^2 + cos_coeff^2)
  amplitude <- sqrt(sin_coeff^2 + cos_coeff^2)
  amplitude_se <- sqrt((sin_coeff^2*sin_se^2) + (cos_coeff^2 * cos_se^2))/ amplitude^2
  #Phase equals arctan(- cos_coeff / sin_coeff)
  acrophase <- atan( sin_coeff / cos_coeff )
  acrophase_se <- ((cos_se^2 * sin_coeff^-2) + (cos_coeff^2 / sin_coeff^3 * sin_se^2)) / (1 + (cos_coeff/sin_coeff)^2)^2


  if (cos_coeff < 0 & sin_coeff >= 0) {
    acrophase <-   acrophase + pi
  }
  if (cos_coeff < 0 & sin_coeff < 0) {
    acrophase <- pi + acrophase
  }

  if (cos_coeff >= 0 & sin_coeff < 0) {
    acrophase <- 2*pi + acrophase
  }

  # if (cos_coeff >= 0 & sin_coeff >= 0) {
  #   acrophase <- acrophase + pi
  # }
```

with this corrected block:

```r
  # Amplitude = sqrt(sin_coeff^2 + cos_coeff^2)
  amplitude <- sqrt(sin_coeff^2 + cos_coeff^2)

  # Delta-method SE for amplitude using the model covariance matrix.
  # A = sqrt(b_s^2 + b_c^2);  Var(A) = (1/A^2) * [b_s^2 Vss + b_c^2 Vcc + 2 b_s b_c Vsc]
  V   <- vcov(model)
  Vss <- V["sinw", "sinw"]; Vcc <- V["cosw", "cosw"]; Vsc <- V["sinw", "cosw"]
  amplitude_se <- sqrt(sin_coeff^2 * Vss + cos_coeff^2 * Vcc +
                         2 * sin_coeff * cos_coeff * Vsc) / amplitude

  # Acrophase via atan2 (correct quadrant), mapped to [0, 2*pi).
  acrophase <- atan2(sin_coeff, cos_coeff)
  if (acrophase < 0) acrophase <- acrophase + 2 * pi

  # Delta-method SE for acrophase.
  # phi = atan2(b_s, b_c); Var(phi) = (1/A^4)[b_c^2 Vss + b_s^2 Vcc - 2 b_s b_c Vsc]
  acrophase_se <- sqrt(cos_coeff^2 * Vss + sin_coeff^2 * Vcc -
                         2 * sin_coeff * cos_coeff * Vsc) / amplitude^2
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cosinor.R")'`
Expected: PASS (all four cosinor tests).

- [ ] **Step 5: Commit**

```bash
git add R/analyze_timeseries.cosinor.R tests/testthat/test-cosinor.R
git commit -m "fix: correct cosinor amplitude_se/acrophase_se (delta method) and use atan2"
```

---

### Task 5: Remove the Granger test from the core pipeline

**Problem:** `README.md` says the Granger test was removed (June 2023) for false positives/negatives, but `analyze_timeseries.grangertest` is still called in `process_timeseries.core` (`R/main.R:165,172,195,199`). Remove it from the pipeline.

**Files:**
- Test: `tests/testthat/test-pipeline-granger.R`
- Modify: `R/main.R` (delete the four grangercausal lines)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-pipeline-granger.R`:

```r
test_that("core pipeline output contains no grangercausal field", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  out <- process_timeseries.core(
    df = df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  expect_null(out$acf$results$grangercausal)
  expect_null(out$lomb$results$grangercausal)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pipeline-granger.R")'`
Expected: FAIL — `grangercausal` is present.

- [ ] **Step 3: Delete the Granger calls**

In `R/main.R`, delete these four lines (two in the windowed branch ~165, 172; two in the non-windowed branch ~195, 199):

```r
      acf_results$grangercausal = analyze_timeseries.grangertest(value = x$value, cos = acf_cosinor$wave, order = causal_order)
```
```r
      lsp_results$grangercausal = analyze_timeseries.grangertest(value = x$value, cos = lsp_cosinor$wave,  order = causal_order)
```
```r
  acf_results$grangercausal = analyze_timeseries.grangertest(value = df$value, cos = acf_cosinor$wave,  order = causal_order)
```
```r
  lsp_results$grangercausal = analyze_timeseries.grangertest(value = df$value, cos = lsp_cosinor$wave,  order = causal_order)
```

Leave `R/analyze_timeseries.grangertest.R` in place for now (its export is removed in Plan 1B per the canonical-decisions doc); this task only unwires it from the pipeline.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pipeline-granger.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/main.R tests/testthat/test-pipeline-granger.R
git commit -m "fix: remove Granger causality test from the core pipeline"
```

---

### Task 6: Fix the `binning_n` latent bug in `process_timeseries.waveform`

**Problem:** `process_timeseries.waveform` (`R/main.R:88-106`) calls `smooth_and_detrend(df, ..., binning_n = binning_n)` but `binning_n` is never a parameter or local — it only survives via lazy evaluation. Add it as an explicit parameter with a default and thread it through.

**Files:**
- Test: `tests/testthat/test-waveform.R`
- Modify: `R/main.R` (`process_timeseries.waveform` signature + call; `process_timeseries.core` call site)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-waveform.R`:

```r
test_that("waveform smoothing path runs without an undefined binning_n", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  expect_no_error(
    process_timeseries.waveform(
      df, detrend_data = FALSE, smooth_data = TRUE,
      butterworth = FALSE, binning_n = 4
    )
  )
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-waveform.R")'`
Expected: FAIL — `unused argument (binning_n = 4)` (the parameter does not exist yet).

- [ ] **Step 3: Add the parameter**

In `R/main.R`, change the `process_timeseries.waveform` signature from:

```r
process_timeseries.waveform <- function(df = NULL,
                                        detrend_data = TRUE,
                                        smooth_data = FALSE,
                                        butterworth = TRUE,
                                        f_low = 1/4,
                                        f_high = 1/73,
                                        order = 2) {
```

to (add `binning_n = 4`):

```r
process_timeseries.waveform <- function(df = NULL,
                                        detrend_data = TRUE,
                                        smooth_data = FALSE,
                                        butterworth = TRUE,
                                        f_low = 1/4,
                                        f_high = 1/73,
                                        order = 2,
                                        binning_n = 4) {
```

The existing `smooth_and_detrend(df, smooth_data = smooth_data, detrend_data = detrend_data, binning_n = binning_n)` call now resolves correctly.

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-waveform.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/main.R tests/testthat/test-waveform.R
git commit -m "fix: make binning_n an explicit parameter of process_timeseries.waveform"
```

---

### Task 7: Remove `future::plan()` side effects from functions

**Problem:** `read_vitalpatch` (`R/read_vitalpatch.R:29`) and `process_timeseries` (`R/process_timeseries.R:68`) call `future::plan(multisession)` unconditionally, hijacking the caller's parallel backend. A library must not mutate global state. Where parallelism is wanted (`big_data` in `process_timeseries.core`/`.main`), save and restore the previous plan with `on.exit`.

**Files:**
- Test: `tests/testthat/test-future-plan.R`
- Modify: `R/read_vitalpatch.R`, `R/process_timeseries.R`, `R/main.R`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-future-plan.R`:

```r
test_that("process_timeseries.core does not leak a global future plan change", {
  future::plan(future::sequential)
  before <- class(future::plan())
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  invisible(process_timeseries.core(
    df = df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, big_data = TRUE
  ))
  after <- class(future::plan())
  expect_identical(after, before)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-future-plan.R")'`
Expected: FAIL — the plan is left as `multisession` after the call.

- [ ] **Step 3: Restore the plan in core/main; drop unconditional plans**

In `R/main.R`, in `process_timeseries.core`, replace:

```r
if (big_data) {
plan(multisession)
} else {plan(sequential)}
```

with:

```r
oplan <- future::plan()
on.exit(future::plan(oplan), add = TRUE)
if (big_data) future::plan(future::multisession)
```

Apply the identical replacement in `process_timeseries.main` (it has the same `if (big_data) {...}` block).

In `R/process_timeseries.R`, delete the line:

```r
  future::plan(future::multisession)
```

In `R/read_vitalpatch.R`, delete the lines:

```r
  #Plan for paralellization
  future::plan(future::multisession)
```

(The `furrr::future_map*` calls still run; they simply respect whatever plan the caller has set, defaulting to sequential.)

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-future-plan.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/main.R R/process_timeseries.R R/read_vitalpatch.R tests/testthat/test-future-plan.R
git commit -m "fix: stop mutating the global future plan; save/restore in big_data path"
```

---

### Task 8: Lomb `ofac` sanity guard

**Problem:** The README example sets `ofac = sampling_rate_in_seconds` (=60), conflating the Lomb oversampling factor with the sampling rate; this is an enormous oversample. Add a guard in `analyze_timeseries.lomb` that warns and caps absurd values, keeping behavior otherwise identical.

**Files:**
- Test: `tests/testthat/test-lomb.R`
- Modify: `R/analyze_timeseries.lomb.R` (after the `type <- 'period'` setup)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-lomb.R`:

```r
test_that("Lomb warns and caps an implausibly large ofac, still finds ~24h", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  expect_warning(
    res <- analyze_timeseries.lomb(df, sampling_rate = "1 hour",
                                   from = 18, to = 30, ofac = 60),
    regexp = "ofac"
  )
  expect_equal(res$period, 24, tolerance = 1)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-lomb.R")'`
Expected: FAIL — no warning is emitted.

- [ ] **Step 3: Add the guard**

In `R/analyze_timeseries.lomb.R`, immediately after the line `type <- 'period'`, insert:

```r
  #ofac is an integer oversampling factor (typically 1-10), NOT the sampling rate.
  if (!is.null(ofac) && ofac > 20) {
    warning("ofac = ", ofac, " is implausibly large for an oversampling factor; ",
            "capping at 20. ofac is the Lomb oversampling factor, not the sampling rate.")
    ofac <- 20
  }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-lomb.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/analyze_timeseries.lomb.R tests/testthat/test-lomb.R
git commit -m "fix: guard against implausibly large Lomb ofac (oversampling-factor confusion)"
```

---

### Task 9: Document and pin rhythm-strength

**Problem:** `rythm_strength` is undocumented and the Lomb variant is self-labeled "beta". Pin the ACF formula with a characterization test and document both, marking the Lomb variant experimental.

**Files:**
- Test: `tests/testthat/test-rhythm-strength.R`
- Modify: `R/analyze_timeseries.acf.R` (roxygen only)

- [ ] **Step 1: Write the characterization test**

Create `tests/testthat/test-rhythm-strength.R`:

```r
test_that("ACF rhythm_strength equals max peak over the 95% white-noise bound", {
  df <- make_sine(period_h = 24, sampling_min = 60, n_days = 6)
  res <- analyze_timeseries.acf(df, from = 18, to = 30, sampling_rate = "1 hour")
  # Definition: max_peak_of_int / (1.965 / sqrt(n))
  expected <- res$max_peak_of_int / (1.965 / sqrt(nrow(df)))
  expect_equal(res$rythm_strength, expected, tolerance = 1e-8)
  expect_true(res$rythm_strength > 1)  # clean signal is clearly rhythmic
})
```

- [ ] **Step 2: Run test to verify it passes (characterization — already true)**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-rhythm-strength.R")'`
Expected: PASS (this pins current behavior so later refactors cannot silently change it).

- [ ] **Step 3: Document the metric**

In `R/analyze_timeseries.acf.R` roxygen header, add a `@details` line (above `@return`):

```r
#' @details The reported `rythm_strength` is the maximum autocorrelation peak within the
#' search band divided by the 95% white-noise confidence bound `1.965 / sqrt(n)`. Values > 1
#' indicate the peak exceeds what white noise would produce. The Lomb-Scargle `rythm_strength`
#' (in [analyze_timeseries.lomb]) is a separate, experimental measure and is not directly comparable.
```

Run: `Rscript -e 'devtools::document()'`
Expected: regenerates `man/analyze_timeseries.acf.Rd` with the new details.

- [ ] **Step 4: Commit**

```bash
git add R/analyze_timeseries.acf.R man/analyze_timeseries.acf.Rd tests/testthat/test-rhythm-strength.R
git commit -m "docs: document and pin ACF rhythm_strength; flag Lomb variant experimental"
```

---

### Task 10: Add FDR p-value adjustment across the grid

**Problem:** Scanning many individuals x windows yields many cosinor/Lomb p-values with no multiple-testing correction. Add a small, decoupled helper that appends a Benjamini-Hochberg-adjusted column to a long results data.frame.

**Files:**
- Create: `R/adjust_pvalues.R`
- Test: `tests/testthat/test-fdr.R`
- Modify: `NAMESPACE` (via `devtools::document()`)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-fdr.R`:

```r
test_that("adjust_pvalues appends a BH-adjusted column matching stats::p.adjust", {
  df <- data.frame(id = 1:5, cosinor_p_value = c(0.001, 0.02, 0.2, 0.5, 0.9))
  out <- adjust_pvalues(df, p_col = "cosinor_p_value", method = "BH")
  expect_true("cosinor_p_value_adj" %in% names(out))
  expect_equal(out$cosinor_p_value_adj,
               stats::p.adjust(df$cosinor_p_value, method = "BH"))
})

test_that("adjust_pvalues errors clearly when the column is missing", {
  df <- data.frame(id = 1:3)
  expect_error(adjust_pvalues(df, p_col = "nope"), regexp = "nope")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-fdr.R")'`
Expected: FAIL — `could not find function "adjust_pvalues"`.

- [ ] **Step 3: Implement the helper**

Create `R/adjust_pvalues.R`:

```r
#' Adjust p-values for multiple testing across a results grid
#'
#' @description Appends a multiple-testing-adjusted p-value column to a long results
#' data.frame (e.g. one row per individual x window). Use after collecting cosinor or
#' Lomb-Scargle p-values across many tests.
#'
#' @param df A data.frame containing a numeric p-value column.
#' @param p_col Character. Name of the p-value column to adjust.
#' @param method Character. Adjustment method passed to [stats::p.adjust]. Default "BH".
#'
#' @return `df` with an added column named `paste0(p_col, "_adj")`.
#' @export
#'
#' @examples
#' df <- data.frame(cosinor_p_value = c(0.001, 0.02, 0.2))
#' adjust_pvalues(df, p_col = "cosinor_p_value")
adjust_pvalues <- function(df, p_col = "cosinor_p_value", method = "BH") {
  if (!p_col %in% names(df)) {
    stop("Column '", p_col, "' not found in df.")
  }
  df[[paste0(p_col, "_adj")]] <- stats::p.adjust(df[[p_col]], method = method)
  df
}
```

- [ ] **Step 4: Regenerate docs and run the test**

Run: `Rscript -e 'devtools::document(); devtools::load_all("."); testthat::test_file("tests/testthat/test-fdr.R")'`
Expected: creates `man/adjust_pvalues.Rd`, adds `export(adjust_pvalues)` to `NAMESPACE`, tests PASS.

- [ ] **Step 5: Commit**

```bash
git add R/adjust_pvalues.R man/adjust_pvalues.Rd NAMESPACE tests/testthat/test-fdr.R
git commit -m "feat: add adjust_pvalues() BH/FDR helper for the individual x window grid"
```

---

### Task 11: Gate A — full suite, R CMD check, sign-off

**Files:** none (verification + doc update)

- [ ] **Step 1: Run the full test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: all tests from Tasks 3-10 PASS, 0 failures.

- [ ] **Step 2: Run R CMD check**

Run: `Rscript -e 'devtools::check(args = c("--no-manual"), error_on = "error")'`
Expected: 0 errors. Record any remaining WARNINGs/NOTEs — these are addressed in Plan 1B (e.g. lingering duplicate exports, Granger function still present). Do **not** fix structural NOTEs here; that is Plan 1B's scope.

- [ ] **Step 3: Mark Gate A reached in the decisions doc**

Append to `docs/superpowers/canonical-decisions-track1.md`:

```markdown
## Gate A status
Reached on completion of Plan 1A: cosinor period scaling, amplitude/acrophase SEs, Granger removal,
binning_n, future::plan side effects, Lomb ofac guard, rhythm_strength docs, and FDR helper are done
and tested. Method semantics are now frozen for Track 2 (Python redesign).
```

- [ ] **Step 4: Commit and report**

```bash
git add docs/superpowers/canonical-decisions-track1.md
git commit -m "chore: Gate A reached — method semantics frozen for Track 2"
```

Report the test counts and the `R CMD check` summary to the user. **STOP for the Gate A checkpoint** before starting Plan 1B.

---

## Self-Review (completed during authoring)

- **Spec coverage:** Track 1 misspecifications from the spec — cosinor SE (Task 4), LSP ofac (Task 8), Granger removal (Task 5), `binning_n` (Task 6), `future::plan` (Task 7), `rythm_strength` (Task 9), FDR (Task 10) — all have tasks. Plus the newly found cosinor period-scaling bug (Task 3). De-duplication / `Authors@R` / vignette / typos are **deferred to Plan 1B** by design (noted in Task 2).
- **Placeholder scan:** no TBD/TODO; every code step shows complete code and exact commands.
- **Type consistency:** function names used in tests (`analyze_timeseries.cosinor`, `process_timeseries.core`, `process_timeseries.waveform`, `analyze_timeseries.lomb`, `analyze_timeseries.acf`, `adjust_pvalues`) match the functions modified/created; `make_sine` defined in Task 1 is used throughout.
