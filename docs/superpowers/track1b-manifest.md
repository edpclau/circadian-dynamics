# Track 1B Reachability Manifest

*Plan 1B, Task 1 — produced 2026-05-30 by static call-graph analysis (`grep` over `R/`). Non-destructive: no R source modified.*

This manifest classifies every `R/*.R` file as **KEEP**, **KEEP+RENAME**, **DELETE-legacy**, or **DELETE-dead**, starting from the canonical "keep roots" frozen at Gate A (see `docs/superpowers/canonical-decisions-track1.md`, "Decisions RESOLVED at the Gate A checkpoint"). It encodes:

- TWO parallel pipelines; the **current** pipeline (`process_timeseries.main`/`.core` + `analyze_timeseries.*`) is canonical, the **legacy** pipeline (`process_timeseries` + `rythm_analysis_by_window` + `acf_window`/`lsp_by_window`/`cosinor_lm` + `export_*`) is removed entirely.
- `simplify_data2` names become canonical (will replace `simplify_data`).
- `plot_actogram2` is KEPT (documented, not wired).
- `read_trikinetics` (long form) and `read_trikinetics_2` (nested) are BOTH kept.

Classification legend:
- **KEEP** — reachable from / part of the supported canonical surface.
- **KEEP+RENAME** — surviving `_2` whose non-`_2` twin is legacy (proposed new name given).
- **DELETE-legacy** — only reachable from the legacy pipeline (or a legacy top-level entry with no live callers).
- **DELETE-dead** — no callers anywhere (and not part of the canonical public surface).

"Caller" evidence below counts **only real code call sites in `R/`** (roxygen `#'` examples/`@usage` and commented-out lines are NOT callers). README/`Example Script.R` usage is treated as canonical-surface membership, not an R/ caller.

## Classification table

| # | File | Class | Defines | Caller evidence (within `R/`) |
|---|------|-------|---------|-------------------------------|
| 1 | acf_window.R | **DELETE-legacy** | `acf_window` | Only `rythm_analysis_by_window.R:66` (legacy). Exported but NOT in canonical surface; superseded by `analyze_timeseries.acf`. |
| 2 | actogram.R | **KEEP** | `actogram` | No R/ caller; canonical surface (README/Example long-form actogram). |
| 3 | adjust_pvalues.R | **KEEP** | `adjust_pvalues` | No R/ caller; canonical surface (FDR helper, Plan 1A Task 10). |
| 4 | analyze_timeseries.acf.R | **KEEP** | `analyze_timeseries.acf` | `main.R:160,190` (current `.core`/`.main`). |
| 5 | analyze_timeseries.cosinor.R | **KEEP** | `analyze_timeseries.cosinor` | `main.R:162,168,191,194`. |
| 6 | analyze_timeseries.grangertest.R | **DELETE-dead** | `analyze_timeseries.grangertest` | No callers (unwired from core pipeline in Plan 1A Task 5). Export must be removed. |
| 7 | analyze_timeseries.lomb.R | **KEEP** | `analyze_timeseries.lomb` | `main.R:166,193`; calls `lsp_mod`, `lsp_peaks`. |
| 8 | arbitrary_noise_substraction.R | **KEEP** | `arbitrary_noise_subtraction` | No R/ caller; canonical surface (kept extra). |
| 9 | average_of_group.R | **KEEP** | `average_of_group` | Called by `multivariate_process_timeseries.R:59` (legacy), but listed as a kept extra in the canonical surface → KEEP. |
| 10 | bind_outputs.R | **KEEP** (`bind_processed`) / **see note** | `bind_processed`, `bind_analysis` | `bind_processed` + `bind_analysis` both called only by `export_all.R:82,84` (legacy). `bind_processed`/`bind_outputs` is in the canonical surface → file KEEP. See Surprises re `bind_analysis`. |
| 11 | butterworth_filter.R | **DELETE-legacy** | `butterworth_filter` | `process_timeseries.R:114` (legacy) only. |
| 12 | butterworth_filter_2.R | **KEEP+RENAME** → `butterworth_filter` | `butterworth_filter_2` | `main.R:101` (current `.waveform`). Rename after legacy `butterworth_filter` removed. |
| 13 | cosinor_lm.R | **DELETE-legacy** | `cosinor_lm` | `rythm_analysis_by_window.R:72,93` (legacy) only. |
| 14 | crop_data.R | **KEEP** | `crop_data` | No R/ caller; canonical surface (kept extra). |
| 15 | define_event.R | **KEEP** | `define_event` | No R/ caller; canonical surface (kept extra). |
| 16 | detailed_plots.R | **KEEP** | `detailed_plots` | No R/ caller; canonical surface (README/Example). Calls `generate_plots_with_windows`/`generate_plots_no_windows`. |
| 17 | downsample_time_series.R | **KEEP** | `downsample_time_series` | **Called by `downsample_time_series_2.R:51`** (a KEEP function). NOT dead — see Surprises (contradicts the Gate A "no callers" note). |
| 18 | downsample_time_series_2.R | **KEEP+RENAME** → `downsample_time_series` | `downsample_time_series_2` | No live caller (only roxygen example in `managing_inactives.R:13`); canonical surface kept extra. Calls the per-series worker `downsample_time_series`. |
| 19 | export_all.R | **DELETE-legacy** | `export_all` | No callers; legacy top-level export entry. Calls `bind_processed`, `bind_analysis`, `plot_actogram`, `plot_phase`, `export_plots`, `plot_summarized_data`, `export_data`. |
| 20 | export_data.R | **DELETE-legacy** | `export_data` | Only `export_all.R:128` (legacy). |
| 21 | export_plots_multivariate.R | **DELETE-legacy** | `export_plots_multivariate` | No callers; legacy multivariate export entry. Calls `export_plots` (legacy). |
| 22 | find_gaps.R | **KEEP** | `find_gaps` | `main.R:69` (current `.rmv_gaps`) AND `process_timeseries.R:88` (legacy). Shared → KEEP. |
| 23 | format_data_for_export.R | **DELETE-dead** | `data_export` | No callers anywhere. Export must be removed. |
| 24 | generate_plots_no_windows.R | **KEEP** | `generate_plots_no_windows` | `detailed_plots.R:15` (KEEP). (Has stale `$grangercausal` refs — Plan 1B cleanup, not classification.) |
| 25 | generate_plots_with_windows.R | **KEEP** | `generate_plots_with_windows` | `detailed_plots.R:13` (KEEP). |
| 26 | lsp_by_window.R | **DELETE-legacy** | `lsp_by_window` | Only `rythm_analysis_by_window.R:84` (legacy). Exported but NOT in canonical surface; superseded by `analyze_timeseries.lomb`. |
| 27 | lsp_mod.R | **KEEP** | `lsp_mod` | `analyze_timeseries.lomb.R:117` (KEEP) AND `lsp_by_window.R:147,157` (legacy). Shared → KEEP. |
| 28 | lsp_peaks.R | **KEEP** | `lsp_peaks` | `analyze_timeseries.lomb.R:135` (KEEP). |
| 29 | main.R | **KEEP** | `process_timeseries.rmv_gaps`, `.na_to_zero`, `.waveform`, `.core`, `.main` | Canonical pipeline entry (README/Example). |
| 30 | make_time_windows.R | **KEEP** | `make_time_windows` | `plot_actogram.R:61`, `overplot_actogram.R:64` (both KEEP) AND `process_timeseries.R:95` (legacy). Shared → KEEP. |
| 31 | make_time_windows_2.R | **DELETE-dead** | `make_time_windows_2` | Only a COMMENTED line `main.R:61`; no live caller. Export must be removed. |
| 32 | managing_inactives.R | **KEEP** | `rm_inactive_dates`, `report_inactive_variables`, `rm_inactive_variables` | `report_inactive_variables` called internally by `rm_inactive_variables` (`:100`); all three in canonical surface. |
| 33 | multivariate_process_timeseries.R | **DELETE-legacy** | `multivariate_process_timeseries` | No callers; calls legacy `process_timeseries` (`:62`) and `average_of_group`. Legacy multivariate entry. |
| 34 | multivariate_rythm_analysis.R | **DELETE-legacy** | `multivariate_rythm_analysis` | No callers; calls legacy `rythm_analysis_by_window` (`:74,82`). Legacy multivariate entry. |
| 35 | overplot_actogram.R | **KEEP** | `overplot_actogram` | No R/ caller; canonical surface (kept extra). Calls `make_time_windows` (KEEP). |
| 36 | plot_acf_results.R | **KEEP** | `plot_acf_results` | No R/ caller; canonical surface (README/Example). |
| 37 | plot_actogram.R | **KEEP** | `plot_actogram` | `export_all.R:77` (legacy) is the only R/ caller, but it is in the canonical surface (README/Example) → KEEP. Calls `make_time_windows` (KEEP). |
| 38 | plot_actogram2.R | **KEEP** | `plot_actogram2` | No R/ caller; explicitly KEPT & documented at Gate A. |
| 39 | plot_actogram_windows.R | **KEEP** | `plot_actogram_windows` | No R/ caller; canonical surface (README/Example). |
| 40 | plot_autocorrelation.R | **KEEP** | `plot_autocorrelation` | No R/ caller; canonical surface (kept extra). |
| 41 | plot_figures.R | **DELETE-legacy** | `export_plots` | Only `export_all.R:108` and `export_plots_multivariate.R:49` (both legacy). Exported but NOT in canonical surface. |
| 42 | plot_lsp_results.R | **KEEP** | `plot_lsp_results` | No R/ caller; canonical surface (README/Example). |
| 43 | plot_phase.R | **KEEP** | `plot_phase` | `export_all.R:90` (legacy) is the only R/ caller, but it is in the canonical surface → KEEP. |
| 44 | plot_raw_values.R | **KEEP** | `plot_raw_values` | No R/ caller; canonical surface (README/Example). |
| 45 | plot_summarized_data.R | **KEEP** | `plot_summarized_data` | `export_all.R:122` (legacy) is the only R/ caller, but it is in the canonical surface → KEEP. |
| 46 | plot_window_data.R | **KEEP** | `plot_window_data` | No R/ caller; canonical surface (kept extra). |
| 47 | plot_window_data_app.R | **KEEP** | `plot_window_data_app` | No R/ caller; canonical surface (kept extra). |
| 48 | print_version.R | **KEEP** | `package_version`, `msg`, `text_col` (+ top-level startup `msg(...)`) | Startup-message side-effect file; no exports, no callers; listed as kept extra (`print_version`). |
| 49 | process_timeseries.R | **DELETE-legacy** | `process_timeseries` | Legacy pipeline entry. Only caller is `multivariate_process_timeseries.R:62` (also legacy). Calls `find_gaps`, `make_time_windows`, `smooth_detrend_by_windows`, `butterworth_filter`. |
| 50 | read_clocklab.R | **KEEP** | `read_clocklab`, `read_clocklab_folder` | No R/ caller; canonical surface (readers). |
| 51 | read_csv_data.R | **KEEP** | `read_csv_data` | No R/ caller; canonical surface (readers). |
| 52 | read_satelite.R | **KEEP** | `read_satellite` | No R/ caller; canonical surface (readers). (Note: file misspelled "satelite"; function correctly `read_satellite`.) |
| 53 | read_trikinetics.R | **KEEP** (rename later) | `read_trikinetics`, `read_trikinetics_folder` | No R/ caller; canonical surface (long-form readers for `actogram()`). Intent-revealing rename deferred. |
| 54 | read_trikinetics_2.R | **KEEP** (rename later) | `read_trikinetics_2`, `.read_trikinetics`, `read_trikinetics_folder_2` | No R/ caller; canonical surface (nested readers for analysis). Intent-revealing rename deferred. |
| 55 | read_vitalpatch.R | **KEEP** | `read_vitalpatch` | No R/ caller; canonical surface (readers). |
| 56 | rescale.R | **KEEP** | `rescale` | No R/ caller; canonical surface (kept extra). |
| 57 | rm_inactive.R | **KEEP** | `rm_inactive` | No R/ caller; canonical surface (kept extra). |
| 58 | rythm_analysis_by_window.R | **DELETE-legacy** | `rythm_analysis_by_window` | Only `multivariate_rythm_analysis.R:74,82` (legacy). Calls `acf_window`, `lsp_by_window`, `cosinor_lm` (all DELETE). |
| 59 | rythm_analysis_by_window_2.R | **DELETE-dead** | `rythm_analysis_by_window_2` | No callers. Export must be removed. |
| 60 | shiny_plot_window_data.R | **KEEP** | `shiny_plot_window_data` | No R/ caller; canonical surface (kept extra). |
| 61 | simplify_data.R | **KEEP (body to be replaced)** | `simplify_data` | No live R/ caller (only a comment ref in `simplify_data2.R:1`); canonical name (README/Example). Body will be replaced by `simplify_data2`'s body at Gate A. |
| 62 | simplify_data2.R | **DELETE-dead** (name source) | `simplify_data2` | No callers. Per Gate A: its BODY replaces `simplify_data`; the `simplify_data2` name/file goes away. Treat as DELETE-dead (no export today). |
| 63 | smooth_detrend_by_windows.R | **DELETE-legacy** | `smooth_detrend_by_windows` | `process_timeseries.R:104` (legacy) only. |
| 64 | smooth_detrend_by_windows_2.R | **KEEP+RENAME** → `smooth_and_detrend` is already non-`_2` | `smooth_and_detrend` | `main.R:98` (current `.waveform`). NOTE: function is already named `smooth_and_detrend` (no `_2`); only the FILE name carries `_2`. KEEP; rename file to `smooth_and_detrend.R` for clarity. |

## DELETE set (files to remove in Plan 1B)

### DELETE-legacy (only reachable from the legacy pipeline)
1. `acf_window.R`
2. `butterworth_filter.R`
3. `cosinor_lm.R`
4. `export_all.R`
5. `export_data.R`
6. `export_plots_multivariate.R`
7. `lsp_by_window.R`
8. `multivariate_process_timeseries.R`
9. `multivariate_rythm_analysis.R`
10. `plot_figures.R` (defines `export_plots`)
11. `process_timeseries.R`
12. `rythm_analysis_by_window.R`
13. `smooth_detrend_by_windows.R`

### DELETE-dead (no callers anywhere)
14. `analyze_timeseries.grangertest.R`
15. `format_data_for_export.R` (defines `data_export`)
16. `make_time_windows_2.R`
17. `rythm_analysis_by_window_2.R`
18. `simplify_data2.R` (its body is migrated into `simplify_data.R`; the file/name is then removed)

**Note on `simplify_data2.R`:** per Gate A decision #2 it is not deleted outright — its *body* replaces `simplify_data`'s body, then the `simplify_data2` file/name is removed. It contributes no surviving symbol of its own, so it is in the DELETE set for the purpose of "files that won't exist after Plan 1B".

## Safety check — no KEEP file depends on any DELETE-set function

For every function defined in a DELETE-set file, I grepped `R/` for real call sites and confirmed every caller is itself in a DELETE-set file. Summary of the evidence:

| DELETE function | Real R/ callers | All callers in DELETE set? |
|---|---|---|
| `acf_window` | `rythm_analysis_by_window.R` | YES (DELETE-legacy) |
| `lsp_by_window` | `rythm_analysis_by_window.R` | YES (DELETE-legacy) |
| `cosinor_lm` | `rythm_analysis_by_window.R` | YES (DELETE-legacy) |
| `rythm_analysis_by_window` | `multivariate_rythm_analysis.R` | YES (DELETE-legacy) |
| `butterworth_filter` | `process_timeseries.R` | YES (DELETE-legacy) |
| `smooth_detrend_by_windows` | `process_timeseries.R` | YES (DELETE-legacy) |
| `process_timeseries` | `multivariate_process_timeseries.R` | YES (DELETE-legacy) |
| `export_data` | `export_all.R` | YES (DELETE-legacy) |
| `export_plots` (plot_figures.R) | `export_all.R`, `export_plots_multivariate.R` | YES (DELETE-legacy) |
| `multivariate_process_timeseries` | — (none) | — |
| `multivariate_rythm_analysis` | — (none) | — |
| `export_all` | — (none) | — |
| `export_plots_multivariate` | — (none) | — |
| `analyze_timeseries.grangertest` | — (none) | DELETE-dead |
| `data_export` | — (none) | DELETE-dead |
| `make_time_windows_2` | — (only a commented line in `main.R:61`) | DELETE-dead |
| `rythm_analysis_by_window_2` | — (none) | DELETE-dead |
| `simplify_data2` | — (none) | DELETE-dead |

**Conclusion: NO KEEP file calls any DELETE-set function.** The shared helpers explicitly checked are all correctly KEEP because they have at least one live caller in the current pipeline or in a kept plot function:
- `make_time_windows` ← `plot_actogram` + `overplot_actogram` (KEEP) — has a legacy caller too, but survives via plotting.
- `find_gaps` ← `main.R` (`process_timeseries.rmv_gaps`, current).
- `lsp_mod` ← `analyze_timeseries.lomb` (current).
- `lsp_peaks` ← `analyze_timeseries.lomb` (current).
- `smooth_and_detrend` ← `main.R` (`process_timeseries.waveform`, current).
- `downsample_time_series` ← `downsample_time_series_2` (KEEP+RENAME).
- `average_of_group` — only R/ caller is legacy `multivariate_process_timeseries`, but it is an independently-supported public extra (canonical surface); KEEP with no broken dependency.

The reverse direction (KEEP functions whose only R/ caller is a DELETE file) is benign for `plot_actogram`, `plot_phase`, `plot_summarized_data`, `bind_processed`, `average_of_group`: each is part of the canonical public surface and remains exported/reachable via README/Example/extras after the legacy callers are removed. None of these are dependencies *of* a DELETE file in a way that would break a KEEP file.

## NAMESPACE exports that correspond to DELETE files (to remove later)

These currently-exported names map to DELETE-set files and **must have their `export(...)` lines removed** when the files are deleted in Plan 1B:

- `export(acf_window)` → acf_window.R (DELETE-legacy)
- `export(lsp_by_window)` → lsp_by_window.R (DELETE-legacy)
- `export(cosinor_lm)` → cosinor_lm.R (DELETE-legacy)
- `export(rythm_analysis_by_window)` → rythm_analysis_by_window.R (DELETE-legacy)
- `export(rythm_analysis_by_window_2)` → rythm_analysis_by_window_2.R (DELETE-dead)
- `export(butterworth_filter)` → butterworth_filter.R (DELETE-legacy) — but the name will be **re-used** by the renamed `butterworth_filter_2`, so net effect is "keep the name, change which file it points to".
- `export(smooth_detrend_by_windows)` → smooth_detrend_by_windows.R (DELETE-legacy)
- `export(process_timeseries)` → process_timeseries.R (DELETE-legacy)
- `export(export_data)` → export_data.R (DELETE-legacy)
- `export(export_all)` → export_all.R (DELETE-legacy)
- `export(export_plots)` → plot_figures.R (DELETE-legacy)
- `export(export_plots_multivariate)` → export_plots_multivariate.R (DELETE-legacy)
- `export(multivariate_process_timeseries)` → multivariate_process_timeseries.R (DELETE-legacy)
- `export(multivariate_rythm_analysis)` → multivariate_rythm_analysis.R (DELETE-legacy)
- `export(make_time_windows_2)` → make_time_windows_2.R (DELETE-dead)
- `export(analyze_timeseries.grangertest)` → analyze_timeseries.grangertest.R (DELETE-dead)
- `export(data_export)` → format_data_for_export.R (DELETE-dead)

Exports to be **renamed** (KEEP+RENAME), not removed:
- `export(butterworth_filter_2)` → drop `_2` (collides with the to-be-removed legacy `butterworth_filter` export; net: one `butterworth_filter` export survives).
- `export(downsample_time_series_2)` → rename to `downsample_time_series` (and drop the legacy `export(downsample_time_series)` — currently `downsample_time_series` is also exported; after rename only one survives).
- `export(smooth_and_detrend)` already non-`_2`; only the file is renamed, NAMESPACE unchanged.

`simplify_data2` is **not** currently exported (no `export(simplify_data2)` in NAMESPACE), so no export removal is needed for it; `export(simplify_data)` stays.

## Surprises / ambiguities (escalate — do not silently resolve in later tasks)

1. **`downsample_time_series` is NOT dead (contradicts the Gate A table).** `canonical-decisions-track1.md` line 45 says `downsample_time_series` has "no callers → Delete file + export." In fact `downsample_time_series_2.R:51` calls `downsample_time_series(...)` as its per-series worker inside `future_map`. So the KEEP+RENAME of `downsample_time_series_2` → `downsample_time_series` would create a **name collision / lost worker** if the original `downsample_time_series.R` is deleted. **Resolution needed:** either (a) inline the worker into the renamed function, or (b) rename the worker to an internal `.downsample_one()` helper. Deleting `downsample_time_series.R` outright (as the Gate A note implies) would break `downsample_time_series_2`. Flagging rather than guessing.

2. **`smooth_detrend_by_windows_2.R` defines `smooth_and_detrend` (already de-suffixed).** Only the *file* carries the `_2`; the function and its export are already `smooth_and_detrend`. So this is a file-rename only, not a function/export rename. Classified KEEP+RENAME (file → `smooth_and_detrend.R`).

3. **`bind_analysis` (in bind_outputs.R) is legacy-only.** `bind_outputs.R` is KEEP because `bind_processed` is in the canonical surface, but the second function in the same file, `bind_analysis`, is called only by `export_all.R` (DELETE-legacy) and is **not** exported in NAMESPACE and **not** in the canonical surface. When `export_all.R` is removed, `bind_analysis` becomes dead. **Resolution needed:** confirm `bind_analysis` should be dropped from `bind_outputs.R` while keeping `bind_processed`. (Does not break any KEEP file.)

4. **`generate_plots_no_windows` / `simplify_data` / `format_data_for_export` still reference `$grangercausal`** (e.g. `generate_plots_no_windows.R:139,149`). `format_data_for_export.R` is DELETE-dead anyway, but the stale `$grangercausal` columns in the KEEP plot path (`generate_plots_*`) and in `simplify_data`/`simplify_data2` should be stripped — already noted as a Plan 1B follow-up in the canonical-decisions doc; not a classification issue.

5. **Several KEEP plot/bind functions have their ONLY R/ caller inside `export_all.R` (DELETE-legacy):** `plot_actogram`, `plot_phase`, `plot_summarized_data`, `bind_processed`. They remain KEEP because they are part of the documented canonical surface (README / Example / explicit kept-extras list), but after legacy removal they will have **zero in-package callers** and rely solely on being exported public API. Not a conflict, but worth noting so they aren't later mistaken for dead code.

6. **`read_satelite.R` filename is misspelled** ("satelite") while the function is correctly `read_satellite`. KEEP; flag the file rename for the naming-conventions sweep.

7. **`print_version.R` runs code at source time** (top-level `msg(...)` startup banner) and defines a function named `package_version` that shadows `base::package_version`. KEEP per the canonical surface, but the shadowing + load-time side effect should be reviewed (ideally moved into `.onAttach` in a `zzz.R`); flagging, not resolving.

## Counts

- KEEP: 43 files
- KEEP+RENAME: 3 files (`butterworth_filter_2.R`, `downsample_time_series_2.R`, `smooth_detrend_by_windows_2.R`)
- DELETE-legacy: 13 files
- DELETE-dead: 5 files (`analyze_timeseries.grangertest.R`, `format_data_for_export.R`, `make_time_windows_2.R`, `rythm_analysis_by_window_2.R`, `simplify_data2.R`)
- Total: 43 + 3 + 13 + 5 = **64 files** (matches the 64 rows of the table and `ls R/*.R`).
</content>
</invoke>
