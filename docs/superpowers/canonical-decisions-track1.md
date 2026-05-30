# Track 1 Canonical Decisions (Gate A)

*Written 2026-05-29 during Plan 1A, from actual call-site inspection (`grep` over `R/`, `Example Script.R`, `README.md`).*

This doc is the contract for Track 2 (Python redesign) and the work list for Plan 1B (structural
cleanup). Plan 1A implements only the **method-correctness** decisions below; **no deletions happen
in Plan 1A** — deletions are Plan 1B.

## Architectural finding: two parallel pipelines

The package contains two complete, overlapping analysis pipelines:

| | **Current pipeline (CANONICAL)** | **Legacy pipeline (DEPRECATE)** |
|---|---|---|
| Entry | `process_timeseries.main` / `.core` (`R/main.R`) | `process_timeseries` (`R/process_timeseries.R`) |
| Per-method | `analyze_timeseries.acf` / `.lomb` / `.cosinor` | `acf_window` / `lsp_by_window` / `cosinor_lm` via `rythm_analysis_by_window` |
| Windowing | built inline in `.core` | `make_time_windows` |
| Conditioning | `process_timeseries.waveform` → `butterworth_filter_2`, `smooth_and_detrend` | `butterworth_filter`, `smooth_detrend_by_windows` |
| Tidy/export | `simplify_data` | `export_data` / `export_all` |

**Evidence the current pipeline is canonical:** `README.md` and `Example Script.R` drive analysis
through `process_timeseries.main(...)` → `detailed_plots(...)` → `simplify_data(...)`. The legacy
`process_timeseries`/`rythm_analysis_by_window` path is not referenced by either.

**Decision:** keep the current pipeline; deprecate the legacy pipeline in Plan 1B (after maintainer
confirmation — see Open Decisions).

## Per-function disposition (actions executed in Plan 1B)

Status legend: KEEP = canonical, keep & rename; DEPRECATE = legacy, remove after confirmation;
DELETE = dead (no callers); SHARED = used by current pipeline and/or plotting, keep; DECISION = needs
maintainer input at Gate A.

| Function | Status | Caller evidence | Plan 1B action |
|---|---|---|---|
| `make_time_windows` | SHARED | `process_timeseries.R` (legacy), `plot_actogram.R`, `overplot_actogram.R` | Keep (plotting needs it); drop legacy caller with the legacy pipeline. |
| `make_time_windows_2` | DELETE | only a **commented** line in `main.R:62`; `.core` builds windows inline | Delete file + export. |
| `butterworth_filter` | DEPRECATE | `process_timeseries.R:115` (legacy) only | Remove with legacy pipeline. |
| `butterworth_filter_2` | KEEP→rename | `process_timeseries.waveform` (current) | Keep; rename to `butterworth_filter` after the legacy one is gone. |
| `smooth_detrend_by_windows` | DEPRECATE | `process_timeseries.R` (legacy) | Remove with legacy pipeline. |
| `smooth_detrend_by_windows_2` | DELETE | no callers | Delete file + export. |
| `rythm_analysis_by_window` | DEPRECATE | `multivariate_rythm_analysis.R` (legacy) | Remove with legacy pipeline; rename "rythm"→"rhythm" elsewhere. |
| `rythm_analysis_by_window_2` | DELETE | no callers | Delete file + export. |
| `cosinor_lm` | DEPRECATE | `rythm_analysis_by_window` (legacy) | Remove with legacy pipeline. (Has the same SE bug fixed in `analyze_timeseries.cosinor` — not worth fixing a deprecated fn.) |
| `downsample_time_series` | DELETE | no callers | Delete file + export. |
| `downsample_time_series_2` | KEEP→rename | `managing_inactives.R` | Keep; rename to `downsample_time_series`. |
| `simplify_data` | KEEP | `README.md:119`, `Example Script.R:126` | Keep (canonical tidy step). |
| `simplify_data2` | DECISION | no callers; added in latest commits (`dd1f4d6`, `54d304c`) | See Open Decisions. |
| `read_trikinetics` | KEEP (distinct) | `README.md:52`, `Example:57` — returns a **long df** for `actogram()` | Keep; it is NOT a duplicate of `_2`. |
| `read_trikinetics_2` | KEEP (distinct) | `README.md:32` — returns a **nested per-individual list** for analysis | Keep; rename the pair to intent-revealing names (e.g. `read_trikinetics_long` / `read_trikinetics_nested`). |
| `plot_actogram` | SHARED | `export_all.R:77` | Keep. |
| `plot_actogram2` | DECISION | no direct callers found | See Open Decisions. |
| `analyze_timeseries.grangertest` | DELETE export | unwired from pipeline in Plan 1A Task 5 | Remove export in Plan 1B (function file may remain or be deleted). |

## Naming conventions (applied in Plan 1B)

- Resolve `_2` suffixes per the table (the surviving function loses the suffix).
- `rythm` → `rhythm` everywhere (function names, args, roxygen, the legacy "Rythm" in `DESCRIPTION` Title).
- For genuinely distinct pairs (`read_trikinetics` family), use intent-revealing names, not numeric suffixes.
- Core analysis/read functions must be non-interactive: move `file.choose()` / `rstudioapi::selectDirectory()` into thin `*_interactive()` wrappers.

## Method-correctness decisions (implemented in Plan 1A, Tasks 3–10)

- `analyze_timeseries.cosinor` period divided by `sampling_bin_size` (Task 3).
- `amplitude_se` via delta method ÷ `amplitude` (not `amplitude^2`); `acrophase` via `atan2`;
  `acrophase_se` via delta method (Task 4).
- Granger test removed from the core pipeline (Task 5).
- `binning_n` made an explicit parameter of `process_timeseries.waveform` (Task 6).
- No more global `future::plan()` mutation inside functions; save/restore in the `big_data` path (Task 7).
- Lomb `ofac` sanity guard (warn + cap) (Task 8).
- ACF `rythm_strength` documented; Lomb variant flagged experimental (Task 9).
- `adjust_pvalues()` BH/FDR helper added (Task 10).

## Open decisions for the maintainer (resolve at the Gate A checkpoint)

1. **Legacy pipeline:** confirm it can be removed entirely in Plan 1B. Anyone still calling
   `process_timeseries()` / `rythm_analysis_by_window()` / `export_data()` directly would break.
2. **`simplify_data` vs `simplify_data2`:** `simplify_data2` was just added but isn't wired in and
   the README still uses `simplify_data`. Which is canonical? (Determines which we keep/rename.)
3. **`plot_actogram2`:** no callers found — delete, or is it a WIP intended replacement for
   `plot_actogram`?
4. **`read_trikinetics` family:** confirm both long-form and nested-form readers should survive, and
   approve intent-revealing names over `_2`.
