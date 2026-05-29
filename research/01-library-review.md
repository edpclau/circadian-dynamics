# 01 — Review of `circadiandynamics`

*Compiled 2026-05-29 against package version 2.1.5 (branch `stable`).*

## Intent

`circadiandynamics` (GPL-3; authors Eddie Pérez Claudio, Manuel Giannoni-Guzmán, Jonathan Alemán,
José L. Agosto-Rivera) is an R package for **period and rhythm analysis of long, evenly- or
unevenly-sampled datetime time series**. Its organizing idea is distinctive: rather than estimating
*a* period for a whole recording, it **slides a multi-day window across the series and re-estimates
period / phase / amplitude / rhythmicity per window**, so a clock can be watched as it drifts,
strengthens, or decays over time — across many individuals at once. (See `DESCRIPTION`, `R/main.R`.)

## Data flow / architecture

A functional `purrr`/`furrr` fan-out:

```
read_* (Trikinetics / ClockLab / VitalPatch / "satellite" / generic CSV)
        ↓  list of per-individual data.frames
process_timeseries.main         (map over individuals)         R/main.R:215
  └ process_timeseries.core     (map over time windows)        R/main.R:114
        ├ rmv_gaps → na_to_zero  (gap fill onto a regular grid) R/process_timeseries.R, R/main.R
        ├ waveform: detrend + Butterworth bandpass / movavg     R/main.R:88
        ├ analyze_timeseries.acf      (autocorrelation period)  R/analyze_timeseries.acf.R
        ├ analyze_timeseries.lomb     (Lomb–Scargle period)     R/analyze_timeseries.lomb.R
        ├ analyze_timeseries.cosinor  (fit at detected period)  R/analyze_timeseries.cosinor.R
        └ analyze_timeseries.grangertest (deprecated)           R/analyze_timeseries.grangertest.R
        ↓
simplify_data → tidy CSVs   +   detailed_plots / actograms / summary PDFs
```

## Methods implemented

- **Lomb–Scargle periodogram** for period detection, via a hand-patched fork of `lomb::lsp`
  (`R/lsp_mod.R`) that auto-increases the oversampling factor when the from–to band is too narrow,
  and degrades to `NA` on zero variance / no peak.
- **Autocorrelation (ACF)** period detection with `pracma::findpeaks`, restricted to a circadian
  band, plus a Bartlett-style rhythm-strength `max_peak / (1.965/√n)` (`R/analyze_timeseries.acf.R:141`).
- **Single-component COSINOR** — linear regression on `sin(2πt/period) + cos(2πt/period)` at the
  period handed in by LSP/ACF; returns MESOR, amplitude, acrophase (with SE propagation), phase in
  time units, adjusted R² ("percent rhythm"), and p-value (`R/analyze_timeseries.cosinor.R`).
- **Granger-causality "rhythmicity test"** between raw signal and cosinor fit — a novel idea the
  authors themselves **retracted** for false positives/negatives (`README.md:10-13`).
- **Signal conditioning** — two-pass zero-phase Butterworth bandpass (`signal::filtfilt`),
  moving-average smoothing, linear detrending (`pracma::detrend`), gap detection/filling, downsampling.
- **Visualization** — single/double-plotted actograms, per-window actograms, raw/periodogram/ACF/
  phase plots, a Shiny viewer, batch PDF export.

## Supported instruments — a genuine strength

The reader set is unusually broad and crosses domains: **Trikinetics/DAM** (Drosophila locomotor),
**ClockLab**, **VitalPatch** (human wearable biosensor, ms-epoch timestamps), a "satellite" reader,
and **generic CSV**. Spanning insect behavior *and* human wearables in one package is rare.

## Weaknesses / issues found

### Code health
- **Pervasive `_2` duplication.** `make_time_windows`/`_2`, `butterworth_filter`/`_2`,
  `downsample_time_series`/`_2`, `read_trikinetics`/`_2`, `rythm_analysis_by_window`/`_2`,
  `smooth_detrend_by_windows`/`_2`, `simplify_data`/`simplify_data2`, `plot_actogram`/`2` — both
  halves are exported in `NAMESPACE`. A half-finished refactor frozen in place; users cannot tell
  which is canonical, and the two cosinor implementations (`R/cosinor_lm.R` vs
  `R/analyze_timeseries.cosinor.R`) can silently diverge.
- **Latent bug:** `R/main.R:98` passes `binning_n = binning_n` inside `process_timeseries.waveform`,
  but `binning_n` is never a parameter or local of that function — it survives only via R's lazy
  evaluation while the smoothing branch doesn't force it. Fragile.
- **Granger inconsistency:** the README says the Granger test was removed (June 2023), but
  `analyze_timeseries.grangertest` is still defined, exported, and **actively called four times**
  inside the core pipeline (`R/main.R:165,172,195,199`). The "removal" is a comment, not code.
- **Library sets global side effects:** `future::plan(multisession)` is called *inside* functions
  (`R/read_vitalpatch.R:29`, `R/process_timeseries.R:68`) — a package should not hijack the user's
  parallel backend.
- **Interactive-only entry points** (`file.choose()`, `rstudioapi::selectDirectory()`) hurt
  scriptability and reproducibility.
- **No tests, no CI, not on CRAN/Bioconductor** — no `tests/`, no `testthat`. For a statistical
  package this is the largest risk.
- **Polish:** typos throughout (`Rythm`, `substraction`, `ovarsampling`, `satelite`↔`satellite`),
  legacy `Author:`/`Maintainer:` fields instead of `Authors@R`, `@usage` blocks that don't match
  real signatures.

### Statistical / methodological
- The README example sets `ofac = sampling_rate_in_seconds` (=60), conflating the **Lomb
  oversampling factor** with the sampling rate — a huge oversample that will be slow and is not what
  `ofac` means.
- `rythm_strength` is self-labeled "beta… needs validation" (`R/analyze_timeseries.lomb.R:160`); the
  cosinor `amplitude_se` divides by `amplitude^2` (`R/analyze_timeseries.cosinor.R:129`), worth
  re-deriving against a reference.
- Single-component cosinor only (no harmonics / asymmetric waveforms), and **no multiple-testing
  correction** when scanning many individuals × many windows.

## Positioning

Its analysis menu (LSP + ACF + cosinor) is conventional and well-covered by validated tools
elsewhere. Its differentiators are (a) per-window *time-resolved* tracking across many individuals
and (b) the unusually broad reader set. See [02 — Cross-language landscape](02-cross-language-landscape.md)
and [03 — Cutting edge & gaps](03-cutting-edge-and-gaps.md).

## Sources
- Package source: `DESCRIPTION`, `NAMESPACE`, `README.md`, `R/*.R` (read directly).
