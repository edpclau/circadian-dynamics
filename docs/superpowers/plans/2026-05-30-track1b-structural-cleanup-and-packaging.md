# Track 1B — Structural Cleanup & Packaging Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the legacy pipeline, de-duplicate the `_2` twins, unify naming (`rythm`→`rhythm`, canonical function names), adopt `simplify_data2`'s column names, strip the now-dead Granger columns, make entry points non-interactive, and bring the package to clean packaging standards — resolving the structural WARNINGs/NOTEs that Gate A's `R CMD check` left.

**Architecture:** Continue on branch `refactor/r-api-cleanup` (Plan 1A already landed there through commit `0adeec8`). Start with a reachability-analysis lead pass that produces an exact keep/delete/rename **manifest** from the canonical surface, so deletions are evidence-based. Then mechanical task groups (legacy removal → simplify_data adoption → `_2` de-dup + rename → Granger-column strip → packaging → vignette), each guarded by `pixi run test` and a clean `R CMD check`.

**Tech Stack:** R 4.5 via pixi (see Plan 1A); `testthat` (3e), `devtools`, `roxygen2`.

---

## Context for the engineer (zero-context assumptions)

- **No system R.** Prefix EVERY R command with `pixi run` (e.g. `pixi run Rscript -e 'devtools::test()'`, `pixi run document`). `lomb` is CRAN-only — `pixi run setup-cran` restores it if the env is rebuilt.
- **`R CMD check` cannot run in-place** (the in-repo `.pixi/envs` symlink breaks `R CMD build`'s copy). Run it on a clean export:
  ```bash
  rm -rf /tmp/cdx-check && mkdir -p /tmp/cdx-check && git archive HEAD | tar -x -C /tmp/cdx-check
  pixi run bash -c 'cd /tmp/cdx-check && R CMD build . --no-build-vignettes && R CMD check circadiandynamics_*.tar.gz --no-manual --no-examples'
  ```
- **Authoritative decisions** are in `docs/superpowers/canonical-decisions-track1.md` (the "Decisions RESOLVED at the Gate A checkpoint" block). Read it first.
- **Breaking changes are authorized.** Bump the version in `DESCRIPTION` to `3.0.0` as part of Task 7.
- After changing roxygen/exports, run `pixi run document` and stage `NAMESPACE` + `man/`.
- All work continues on `refactor/r-api-cleanup`. Commit per task.

---

### Task 1: Reachability manifest (lead pass — gates all deletions)

**Files:** Create `docs/superpowers/track1b-manifest.md`

The canonical surface (the reachable, supported API) is:
- Readers used by README/Example + actogram: `read_csv_data`, `read_trikinetics`/`_2`, `read_trikinetics_folder*`, `read_clocklab*`, `read_vitalpatch`, `read_satellite`.
- Pipeline: `process_timeseries.main`/`.core`/`.waveform`/`.rmv_gaps`/`.na_to_zero`, `analyze_timeseries.acf`/`.lomb`/`.cosinor`, `lsp_mod`, `lsp_peaks`, `find_gaps`, `smooth_and_detrend`, `butterworth_filter_2`.
- Tidy/plot used by README/Example: `simplify_data` (the adopted `simplify_data2`), `detailed_plots`, `plot_actogram_windows`, `plot_raw_values`, `plot_acf_results`, `plot_lsp_results`, `actogram`, `plot_actogram` (used by `export_all`? verify), `adjust_pvalues`.
- Kept extras: `plot_actogram2` (documented, not wired), `plot_window_data`/`_app`, `shiny_plot_window_data`, `crop_data`, `rescale`, `define_event`, `average_of_group`, managing-inactives helpers, `overplot_actogram`.

- [ ] **Step 1: Build the dependency graph**

For every file in `R/`, list the functions it defines and the package-internal functions it calls. Run:
```bash
pixi run Rscript -e '
files <- list.files("R", full.names=TRUE)
defs <- lapply(files, function(f){ t <- readLines(f, warn=FALSE); list(file=f, defs=grep("<- function|<-function", t, value=TRUE)) })
for (d in defs) { cat("\n==", d$file, "==\n"); cat(d$defs, sep="\n") }
'
grep -rn -oE "[a-zA-Z_.][a-zA-Z0-9_.]*\(" R/ | sort | uniq -c | sort -rn | head -80
```
Then, starting from the canonical surface above, trace which files are reachable.

- [ ] **Step 2: Write the manifest**

Create `docs/superpowers/track1b-manifest.md` with a table classifying EVERY `R/*.R` file as one of:
KEEP / KEEP+RENAME (give new name) / DELETE (legacy or dead). For each DELETE, name the function(s) and confirm (with the grep evidence) that no KEEP file calls them. Seed it from the canonical-decisions table; expected DELETE candidates to verify: `process_timeseries.R`, `rythm_analysis_by_window.R`, `rythm_analysis_by_window_2.R`, `cosinor_lm.R`, `acf_window.R`, `lsp_by_window.R`, `multivariate_rythm_analysis.R`, `multivariate_process_timeseries.R`, `export_data.R`, `export_all.R`, `plot_figures.R`, `format_data_for_export.R`, `generate_plots_no_windows.R`, `generate_plots_with_windows.R`, `smooth_detrend_by_windows.R`, `smooth_detrend_by_windows_2.R`, `make_time_windows_2.R`, `downsample_time_series.R`, `butterworth_filter.R`, `simplify_data.R` (replaced in Task 3), `analyze_timeseries.grangertest.R`. Expected KEEP-but-shared: `make_time_windows.R` (plotting uses it).

- [ ] **Step 3: Verify no KEEP→DELETE edges**

For each function in the DELETE set, run `grep -rn "<fnname>(" R/` and confirm every caller is itself in the DELETE set. If a KEEP file calls a DELETE function, STOP and escalate — the classification is wrong.

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/track1b-manifest.md
git commit -m "docs: Track 1B reachability manifest (keep/delete/rename classification)"
```

---

### Task 2: Remove the legacy pipeline

**Files:** Delete the files marked DELETE-legacy in the manifest; regenerate `NAMESPACE`.

- [ ] **Step 1: Add a guard test that the canonical pipeline is intact**

Create `tests/testthat/test-canonical-pipeline.R`:
```r
test_that("the canonical pipeline runs end-to-end after legacy removal", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries.main(
    df = df, make_windows = FALSE, sampling_rate = "1 hour",
    detrend_data = FALSE, butterworth = FALSE, from = 18, to = 30
  )
  expect_true(is.list(out))
  expect_equal(out$ind1$lomb$results$period, 24, tolerance = 1)
})
```
Run it (`pixi run Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-canonical-pipeline.R")'`); expect PASS (this guards the deletions).

- [ ] **Step 2: Delete the legacy files**

`git rm` each file the manifest marks DELETE-legacy (the `process_timeseries`/`rythm_analysis_by_window`/`cosinor_lm`/`acf_window`/`lsp_by_window`/`multivariate_*`/`export_*`/`plot_figures`/`format_data_for_export`/`generate_plots_*` set, plus dead `*_2`/non-`_2` duplicates per the manifest). Do NOT delete `make_time_windows.R` (shared with plotting).

- [ ] **Step 3: Regenerate NAMESPACE and confirm load**

```bash
pixi run Rscript -e 'devtools::document(); devtools::load_all(".")'
```
Expected: loads with no error; removed functions no longer exported. If `load_all` errors with "could not find function X", a KEEP file still references a deleted function — fix the manifest/caller before continuing.

- [ ] **Step 4: Run full suite + commit**

```bash
pixi run Rscript -e 'devtools::test()'   # expect 0 failures
git add -A && git commit -m "refactor: remove the legacy analysis pipeline (process_timeseries/rythm_analysis_by_window/cosinor_lm)"
```

---

### Task 3: Adopt simplify_data2; strip Granger columns

**Files:** Delete `R/simplify_data.R` (old), rename `R/simplify_data2.R` → `R/simplify_data.R` with function `simplify_data`; create `tests/testthat/test-simplify-data.R`; update plot consumers.

- [ ] **Step 1: Failing integration test**

Create `tests/testthat/test-simplify-data.R`:
```r
test_that("simplify_data tidies pipeline output with canonical column names and no Granger cols", {
  df <- list(ind1 = make_sine(period_h = 24, sampling_min = 60, n_days = 6))
  out <- process_timeseries.main(df, make_windows = FALSE, sampling_rate = "1 hour",
                                 detrend_data = FALSE, butterworth = FALSE)
  tidy <- simplify_data(out)
  expect_named(tidy, c("data", "autocorrelation", "lombscargle", "utils"))
  expect_true("acf_peak" %in% names(tidy$autocorrelation))
  expect_true("lsp_peak" %in% names(tidy$lombscargle))
  expect_true("lsp_power" %in% names(tidy$utils))
  # Granger columns must be gone
  expect_false(any(grepl("^gc_|_gc$", names(tidy$autocorrelation))))
  expect_false(any(grepl("^gc_|_gc$", names(tidy$utils))))
})
```
Run it; expect FAIL (current `simplify_data` lacks `acf_peak`/etc. and still has `gc_*`).

- [ ] **Step 2: Replace simplify_data with simplify_data2's body, minus Granger**

`git rm R/simplify_data.R`; rename `R/simplify_data2.R` to `R/simplify_data.R`; rename the function to `simplify_data`; add a proper roxygen header with `@export` and `@return`. Remove every Granger column from all four tibbles in BOTH branches: delete the `gc_raw_to_cos`, `gc_cos_to_raw`, `lsp_gc`, `acf_gc` lines. Replace the bare `plan(multisession)`/`plan(sequential)` at the top with the save/restore pattern from Plan 1A (`oplan <- future::plan(); on.exit(future::plan(oplan), add=TRUE); if (big_data) future::plan(future::multisession)`).

- [ ] **Step 3: Update plot consumers to canonical names**

Per the manifest, update any KEEP plot function that referenced `acf_peak_power`/`lsp_peak_power`/`lsp_powers` (e.g. `plot_lsp_results`, `plot_acf_results`, `plot_window_data`) to `acf_peak`/`lsp_peak`/`lsp_power`. Grep first: `grep -rn "acf_peak_power\|lsp_peak_power\|lsp_powers" R/`.

- [ ] **Step 4: document, test, commit**

```bash
pixi run Rscript -e 'devtools::document()'
pixi run Rscript -e 'devtools::test()'   # expect PASS incl. new test
git add -A && git commit -m "refactor: adopt simplify_data2 column names as canonical; strip dead Granger columns"
```

---

### Task 4: De-duplicate remaining `_2` twins and rename rythm→rhythm

**Files:** per manifest — `butterworth_filter_2.R`, `downsample_time_series_2.R`, `make_time_windows_2.R`, `smooth_detrend_by_windows_2.R`, `read_trikinetics*`, and a repo-wide `rythm`→`rhythm` rename.

- [ ] **Step 1: Resolve the surviving `_2` names**

For each surviving `_2` function whose non-`_2` twin was deleted in Task 2, rename the function and file to drop the suffix and update all callers:
- `butterworth_filter_2` → `butterworth_filter` (file `R/butterworth_filter.R`); update the call in `process_timeseries.waveform` (`R/main.R`).
- `downsample_time_series_2` → `downsample_time_series`; update caller in `R/managing_inactives.R`.
- Delete `R/make_time_windows_2.R` and `R/smooth_detrend_by_windows_2.R` if not already deleted in Task 2 (they were dead).
- `read_trikinetics` (long form, for `actogram`) and `read_trikinetics_2` (nested, for analysis) → rename to intent-revealing names `read_trikinetics_long` and `read_trikinetics_nested`; update README/Example references and `read_trikinetics_folder*`.

- [ ] **Step 2: rythm → rhythm**

Repo-wide, rename the `rythm` misspelling in identifiers, arguments, list element names, and docs. Grep: `grep -rn "rythm" R/ man/ DESCRIPTION`. Note the data column/list name `rythm_strength` is produced in `analyze_timeseries.acf`/`.lomb` and consumed in `simplify_data`/plot functions — rename ALL of them together to `rhythm_strength` and update the Plan 1A tests in `tests/testthat/test-rhythm-strength.R` accordingly. Update the `DESCRIPTION` Title ("Rythm"→"Rhythm").

- [ ] **Step 3: document, test, commit**

```bash
pixi run Rscript -e 'devtools::document()'
pixi run Rscript -e 'devtools::test()'   # fix any test referencing old names; expect PASS
git add -A && git commit -m "refactor: de-duplicate _2 twins, intent-revealing reader names, rythm->rhythm"
```

---

### Task 5: Remove the Granger function and any remaining references

**Files:** `R/analyze_timeseries.grangertest.R`, `NAMESPACE`, plus any KEEP file still referencing `$grangercausal`.

- [ ] **Step 1: Confirm no references remain**

```bash
grep -rn "grangercausal\|grangertest\|grangertest(" R/ tests/
```
Expected: only `R/analyze_timeseries.grangertest.R` itself (Plan 1A already unwired the pipeline; Task 3 stripped the simplify_data columns). If a KEEP file still references `$grangercausal`, remove that reference.

- [ ] **Step 2: Delete the function + export**

`git rm R/analyze_timeseries.grangertest.R`; `pixi run Rscript -e 'devtools::document()'` to drop `export(analyze_timeseries.grangertest)` and the `@importFrom lmtest grangertest`. Remove `lmtest` from `DESCRIPTION` Imports if nothing else uses it (`grep -rn "lmtest\|grangertest" R/`).

- [ ] **Step 3: load, test, commit**

```bash
pixi run Rscript -e 'devtools::document(); devtools::load_all("."); devtools::test()'
git add -A && git commit -m "refactor: remove the deprecated Granger causality function and its dependency"
```

---

### Task 6: Non-interactive entry points

**Files:** the readers that call `file.choose()` / `rstudioapi::selectDirectory()` (e.g. `read_trikinetics*`, `read_vitalpatch`, `read_clocklab*`).

- [ ] **Step 1: Make `file`/`folder` required; add thin interactive wrappers**

For each reader, change the core function so the path argument is required (error clearly if missing) instead of silently launching a GUI. Provide a thin `*_interactive()` wrapper (exported) that does the `file.choose()`/`rstudioapi::selectDirectory()` and calls the core. Example pattern for `read_vitalpatch`:
```r
read_vitalpatch <- function(folder) {
  if (missing(folder) || is.null(folder)) stop("`folder` is required. Use read_vitalpatch_interactive() to pick one.")
  # ... existing body ...
}
read_vitalpatch_interactive <- function() read_vitalpatch(rstudioapi::selectDirectory())
```

- [ ] **Step 2: Sweep residual `future::plan()` mutations**

`grep -rn "plan(multisession)\|plan(sequential)\|future::plan(" R/`. For each KEEP file that still mutates the global plan unconditionally (e.g. `make_time_windows.R`, `plot_actogram.R`, `overplot_actogram.R`), apply the save/restore `on.exit` pattern or remove it.

- [ ] **Step 3: document, test, commit**

```bash
pixi run Rscript -e 'devtools::document(); devtools::test()'
git add -A && git commit -m "refactor: non-interactive readers (+ *_interactive wrappers); sweep global future::plan mutations"
```

---

### Task 7: Packaging polish

**Files:** `DESCRIPTION`, roxygen headers with missing `@return`/`@param`, `R/plot_actogram2.R` (doc only).

- [ ] **Step 1: DESCRIPTION metadata**

Convert `Author:`/`Maintainer:` to `Authors@R` (use `person()` with roles `c("aut","cre")` for Eddie Pérez Claudio and `"aut"` for the co-authors); fix the Title to "Period and Rhythm Analysis of Timeseries Data"; tidy the Description grammar; bump `Version:` to `3.0.0`.

- [ ] **Step 2: Fix the roxygen issues that R CMD check flagged**

From the Gate A check, resolve: missing documentation entries, `@return` missing values (e.g. `simplify_data`, `adjust_pvalues` already OK), Rd `\usage` mismatches (remove hand-written `@usage` lines that disagree with the actual signatures — let roxygen generate usage), and code/documentation mismatches. Document `plot_actogram2` properly (it consumes `simplify_data()$data`; note in its `@description` that it overlaps with `plot_actogram_windows` and is not yet wired into the default workflow).

- [ ] **Step 3: document, test, commit**

```bash
pixi run Rscript -e 'devtools::document(); devtools::test()'
git add -A && git commit -m "docs: Authors@R, fix Title/Description, resolve roxygen usage/return/doc mismatches; bump to 3.0.0"
```

---

### Task 8: Vignette

**Files:** Create `vignettes/circadiandynamics.Rmd`; update `DESCRIPTION` (`VignetteBuilder: knitr`, `Suggests: knitr, rmarkdown`).

- [ ] **Step 1: Write a workflow vignette**

Mirror the README's current workflow against the cleaned API: load → read (use `read_trikinetics_nested`) → `process_timeseries.main` → `detailed_plots` → `simplify_data` → `adjust_pvalues`. Use `eval=FALSE` chunks for the file-reading steps (no bundled raw file) and an `eval=TRUE` chunk on the bundled `data/trikinetics.rda` for at least one real analysis + plot.

- [ ] **Step 2: Build the vignette, commit**

```bash
pixi run Rscript -e 'devtools::build_vignettes()'
git add -A && git commit -m "docs: add workflow vignette"
```

---

### Task 9: Final clean R CMD check (Track 1 done)

**Files:** none (verification)

- [ ] **Step 1: Full suite**

`pixi run Rscript -e 'devtools::test()'` — expect 0 failures.

- [ ] **Step 2: Clean R CMD check**

```bash
rm -rf /tmp/cdx-check && mkdir -p /tmp/cdx-check && git archive HEAD | tar -x -C /tmp/cdx-check
pixi run bash -c 'cd /tmp/cdx-check && R CMD build . && R CMD check circadiandynamics_*.tar.gz --no-manual 2>&1 | tail -40'
```
Target: 0 ERRORS, 0 WARNINGS. Record any residual NOTEs. (Examples may now run since readers are non-interactive — if any example still needs a file, wrap it in `\dontrun{}`.)

- [ ] **Step 3: Report**

Report the test counts and the check summary. Track 1 (R cleanup) is complete; the package is now the validated source-of-truth for Track 2 (Python redesign). STOP for the Track-1-complete checkpoint.

---

## Self-Review (completed during authoring)

- **Spec coverage:** legacy removal (Task 2), simplify_data2 adoption + Granger-column strip (Task 3), `_2` de-dup + rhythm rename + reader names (Task 4), Granger function removal (Task 5), non-interactive entry points + plan sweep (Task 6), Authors@R/Title/roxygen (Task 7), vignette (Task 8), clean check (Task 9) — all map to the canonical-decisions doc and the Plan 1A Gate A follow-ups. `binning_n` propagation and nested-parallelism follow-ups are addressed within Tasks 3/6 (plan save/restore) and should be picked up explicitly if not — note: thread `binning_n` from `.core`/`.main` in Task 4 Step 1 when touching `process_timeseries.waveform`'s caller.
- **Placeholder scan:** the only deferred specifics are the exact DELETE file list, which Task 1 produces as a concrete manifest before any deletion — by design, not a placeholder.
- **Consistency:** column names `acf_peak`/`lsp_peak`/`lsp_power` and `rhythm_strength` are used consistently across Tasks 3 and 4; the canonical pipeline guard test (Task 2) and simplify_data test (Task 3) protect the deletions.
