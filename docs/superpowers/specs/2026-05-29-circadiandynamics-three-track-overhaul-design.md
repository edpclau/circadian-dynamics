# Design: circadiandynamics three-track overhaul

*Date: 2026-05-29 · Status: approved design, pending spec review · Author: brainstorming session with Eddie Pérez Claudio*

## Goal

Execute three large changes to `circadiandynamics`, driven as a single dynamic workflow with
agent-swarm parallelism where it pays off:

1. **Track 1 — R code review & cleanup** (breaking changes allowed): polish the API to best practices,
   remove duplicates, streamline architecture, polish roxygen docs, fix misspecifications and
   implementation errors. On its own branch.
2. **Track 2 — Python redesign**: re-implement the library as an idiomatic, modern Python package
   (not a 1:1 port). On a branch in this repo.
3. **Track 3 — Route B capabilities**: add CWT (continuous wavelet transform) + a joint state-space
   cyclic HMM (the "Route B" design from `research/05-dynamic-period-hmm.md`), in Python.

Background and rationale live in [`research/`](../../../research/) (library review, cross-language
landscape, cutting-edge gaps, HMM/Markov methods, dynamic-period HMMs).

## Decisions (locked 2026-05-29)

| Decision | Choice | Consequence |
|---|---|---|
| Sequencing | **Phased + swarm-within** | Clean R → port → Route B; parallelize *inside* each phase, not across. |
| Route B ecosystem | **Python** | Track 3 builds on Track 2; uses ssm / dynamax / JAX / NumPyro. |
| Python scope | **Redesign for Python** | Not 1:1. Depends on Track 1's *corrected method semantics*, not its exact API → looser coupling, more early parallelism. |
| Python location | **Branch in this repo** | Branch `python`; added to `.Rbuildignore` so it never disturbs `R CMD check`. |

**Key implication of "redesign":** Track 2 is gated on Track 1's **method-correctness decisions**
(Gate A), *not* on the full R cleanup being finished. Track 2's scaffolding/architecture can begin in
parallel with Track 1.

## Workflow shape

```
PHASE 0 (now)   brainstorm → spec → per-track implementation plans (writing-plans)
                       │
   ┌────────────────────┴─────────────────────────┐
PHASE 1   Track 1: R cleanup ──► GATE A ────────►  Track 2a: Python scaffolding
          (swarm by subsystem)   (method            + architecture (parallel; no
                                  semantics frozen)  dependency on R's exact API)
                       │
PHASE 2   Track 2b: Python implementation swarm (corrected algorithms → Pythonic API)
                       │
                    GATE B (Python core validated vs R + reference tools)
                       │
PHASE 3   Track 3: Route B (sequential research, on Python branch)
          CWT → oscillator SSM → switching layer → inference → identifiability/sim → integrate
```

- **Orchestration model:** Claude acts as orchestrator — dispatches agent swarms within a phase,
  runs a code-review + verification pass, surfaces a human checkpoint, then advances. This is the
  "dynamic" element: self-paced across phases, gated at each boundary.
- **Gates are human-review checkpoints.** **Gate A is the contract** Track 2 builds against and is
  the most important sign-off.

## Track 1 — R cleanup

**Branch:** `refactor/r-api-cleanup` (off `stable`). Breaking changes allowed; major version bump.

**Structure** (de-dup decisions must be centralized *before* fan-out, or parallel agents collide):

1. **Lead pass (sequential):** produce a "canonical decisions" doc —
   - Resolve every `_2` twin (pick the winner, final name): `make_time_windows`/`_2`,
     `butterworth_filter`/`_2`, `downsample_time_series`/`_2`, `read_trikinetics`/`_2`,
     `rythm_analysis_by_window`/`_2`, `smooth_detrend_by_windows`/`_2`,
     `simplify_data`/`simplify_data2`, `plot_actogram`/`2`, and the two cosinor implementations
     (`cosinor_lm` vs `analyze_timeseries.cosinor`).
   - Decide canonical entry points and naming convention; non-interactive APIs (remove implicit
     `file.choose()`/`rstudioapi` from core paths).
   - **This doc is also Gate A's input.**
2. **Cleanup swarm (parallel, 1 agent per subsystem, isolated git worktrees, disjoint file sets):**
   - IO/readers · Preprocessing/filters · **Analysis methods (correctness-critical)** ·
     Orchestration · Plotting/Shiny · Package-meta + roxygen + typos.
3. **Integration pass (sequential):** merge worktrees, add `testthat` + GitHub Actions CI, switch to
   `Authors@R`, vignette, achieve clean `R CMD check`.

**Misspecifications / errors to fix** (from `research/01-library-review.md`):
- Cosinor `amplitude_se` formula (divides by `amplitude^2`) — re-derive against reference.
- LSP `ofac` guidance in README (conflates oversampling factor with sampling rate).
- Granger pipeline — remove it from the core pipeline (README claims removed; still wired in
  `R/main.R`).
- `binning_n` latent bug in `process_timeseries.waveform` (`R/main.R:98`).
- `future::plan()` side effects inside functions.
- `rythm_strength` — validate definition or label clearly as experimental.
- No multiple-testing correction across individual×window grid — add FDR control.
- Typos throughout; `Author:`/`Maintainer:` → `Authors@R`.

**Deliverables:** clean, tested R package on its branch; canonical-decisions doc; passing CI.

## Track 2 — Python redesign

**Branch:** `python` (in this repo; `.Rbuildignore`d).

**Stack:** numpy, scipy, pandas, `astropy.timeseries.LombScargle`, statsmodels;
`pywt`/`ssqueezepy` reserved for Track 3 wavelets; packaging via `pyproject.toml`, `pytest`,
type hints, `ruff`.

**Architecture (modern, Pythonic — explicitly not 1:1):**
- A `Recording`/`TimeSeries` data model as the core object.
- Composable analyzers (`LombScargle`, `Autocorrelation`, `Cosinor`) behind one small common
  interface; easy to add `Wavelet`/Route-B later.
- Readers as plugins (Trikinetics/DAM, ClockLab, VitalPatch, satellite, CSV); non-interactive.
- Plotting separated from analysis.

**Swarm:** once architecture + data model are fixed, fan out by module — io · preprocessing ·
periodicity · cosinor/stats · orchestration · viz · packaging.

**Validation strategy:** redesign ⇒ no strict 1:1 numerical parity. Validate **both** R and Python
against (a) synthetic signals with known period/phase/amplitude and (b) reference tools
(MetaCycle / CosinorPy / pyBOAT). **Gate B = Python core validated.**

## Track 3 — Route B (CWT + joint state-space cyclic HMM)

**Branch:** `feature/route-b` (off `python`). Mostly sequential research; output **labeled
research-grade until the validation gate passes**.

**Sub-components:**
1. **CWT module** — ridge extraction → instantaneous period/phase. (Parallelizable with #5.)
2. **Latent oscillator state-space model** — time-varying frequency as a random walk (smoothness prior).
3. **Switching/emission layer** — discrete rest/active states gated by oscillator phase.
4. **Inference engine** — particle filter / HMC (ssm / dynamax / NumPyro).
5. **Identifiability + simulation validation harness** — recover known time-varying τ; graceful
   degradation on weak/short data. **This gate decides trustworthiness.**
6. **Integration** into the Python package as a clearly-labeled module.

Caveats carried from `research/05-dynamic-period-hmm.md`: circularity/double-dipping, CWT-phase
reliability inside the cone of influence, over-flexibility masking arrhythmicity, data-hunger /
identifiability. Mitigations: smoothness priors, ridge-power weighting, piecewise-constant τ fallback.

## Swarm / orchestration mechanics

- **Agent types:** `Explore` for read-only reconnaissance fan-out; `general-purpose`/`claude` in
  **git worktrees** for parallel code-writing on disjoint files; `code-simplifier` for cleanup
  passes; a code-review pass at each gate.
- **Conflict avoidance:** parallel agents only ever touch non-overlapping file sets; shared
  decisions (de-dup, API, architecture) are made *before* fan-out, never inside it.
- **Per-gate ritual:** code review → `verification-before-completion` (tests / `R CMD check` /
  `pytest` actually executed) → human checkpoint → advance.

## Non-goals / YAGNI

- No 1:1 Python parity with the messy current R API.
- No attempt to ship Route B as validated production capability before its identifiability gate.
- No unrelated refactoring beyond what serves these three tracks.
- No merge of any track to `stable` without explicit user approval at its gate.

## Open items to resolve in implementation plans

- Exact final names in the canonical-decisions doc (Track 1 lead pass).
- Python package name and module layout (Track 2 architecture step).
- Specific inference backend for Route B (ssm vs dynamax vs NumPyro) — decided during Track 3 #4.
