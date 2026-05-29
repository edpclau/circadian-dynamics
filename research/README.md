# `circadiandynamics` — Research Dossier

> Compiled 2026-05-29. A review of the `circadiandynamics` R package plus a survey
> of the surrounding field (equivalent tools, the state of the art, the implementation
> gap), and a focused methods thread on Hidden Markov / Markov models for rhythm analysis.
> These documents capture a working research conversation; treat statistical claims as
> literature-grounded starting points, not final validation.

## Contents

| # | Document | What it covers |
|---|----------|----------------|
| 01 | [Library review](01-library-review.md) | Goals, architecture, methods, supported instruments, strengths, and concrete code/statistical issues found in `circadiandynamics`. |
| 02 | [Cross-language landscape](02-cross-language-landscape.md) | Equivalent / neighboring tools in R, Python, MATLAB/Java, and web platforms, with a capability comparison. |
| 03 | [Cutting edge & implementation gap](03-cutting-edge-and-gaps.md) | State of the art in circadian analysis, what researchers need, where this library sits, and prioritized recommendations. |
| 04 | [HMM & Markov models for rhythms](04-hmm-and-markov-models.md) | Role of (cyclic) Hidden Markov Models and Markov chains; how they compare to Lomb–Scargle, STFT, and wavelets. |
| 05 | [Dynamic-period HMMs](05-dynamic-period-hmm.md) | Can an HMM be given a time-varying (e.g. wavelet-derived) period? Three routes, caveats, and the applied-vs-research distinction. |

## One-paragraph synthesis

`circadiandynamics` is a sliding-window, multi-individual rhythm-analysis pipeline (Lomb–Scargle
+ autocorrelation + single-component cosinor, with Butterworth/moving-average conditioning) whose
distinctive assets are (a) per-window *time-resolved* tracking of period/phase/amplitude and (b) an
unusually broad reader set spanning insect behavior (Trikinetics/DAM, ClockLab) and human wearables
(VitalPatch). Its analysis menu is conventional and well-covered by validated tools elsewhere
(MetaCycle, DiscoRhythm, BioDare2, CosinorPy, pyBOAT). The field has moved toward (1) *time-varying*
analysis (wavelets), (2) *between-group* differential rhythmicity, and (3) *state-based* models
(cyclic HMMs) that fit square-wave behavioral data better than cosinor. The clearest opportunities
for this library are a wavelet backend, mixed-effects/differential-rhythmicity statistics, FDR
control across the individual×window grid, a cyclic-HMM module matched to its wearable data, and
basic software hygiene (de-duplication, tests/CI, validation).
