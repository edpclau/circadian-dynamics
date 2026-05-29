# 05 — Can an HMM have a *dynamic* period? (e.g. a wavelet-derived one)

*Compiled 2026-05-29. Follow-on to [04 — HMM & Markov models](04-hmm-and-markov-models.md).*

**Short answer: yes — and it is the natural way to make a cyclic HMM honest about a free-running or
drifting clock.** The "cyclic" part of a cyclic HMM is just a **time-varying covariate** feeding the
transition probabilities through a link function. In Huang & Finkenstädt that covariate is
`cos(2πt/24)` with the period hard-wired at 24 h. Nothing forces it to be fixed — you can swap in a
covariate whose cycle length changes over time, including one derived from a wavelet.

## Key reframing: feed **phase**, not period

Do **not** plug instantaneous period `τ(t)` directly into `cos(2πt/τ)` — that assumes the period was
constant up to time *t*. Instead use the **instantaneous phase**:

- The CWT ridge gives instantaneous angular frequency `ω(t) = 2π/τ(t)`.
- Integrate to an unwrapped phase clock: `φ(t) = φ(0) + ∫₀ᵗ 2π/τ(s) ds`.
- A clock that ticks faster when τ shrinks, slower when τ grows.

The HMM then never sees "period" — it sees a phase that advances non-uniformly. That is the clean
encoding of a dynamic period.

## Route A — Two-stage plug-in (most practical)

1. Run a wavelet transform (pyBOAT / WaveletComp) → extract the maximal-power **ridge** →
   instantaneous period `τ(t)` and phase `φ(t)`.
2. Build covariates `c₁(t)=cos φ(t)`, `c₂(t)=sin φ(t)`, ideally **weighted by ridge power and masked
   inside the cone of influence**.
3. Fit a **non-homogeneous HMM** where transition log-odds are a function of those covariates via a
   multinomial-logit link:
   `logit P(rest→active at t) = β₀ + β₁ c₁(t) + β₂ c₂(t)`.

Squarely within the established **covariate-dependent / non-homogeneous HMM** framework — supported
by **hmmTMB**, **depmixS4**, **momentuHMM** (R) or `ssm`/`dynamax` (Python). The HMM now has a
"dynamic period" because its switching propensity follows a clock whose rate the wavelet sets.
**This is an engineering/integration task**: known, validated building blocks composed in a known recipe.

## Route B — Joint state-space oscillator + switching states (most principled)

Instead of freezing an externally-estimated period, let a **latent oscillator with time-varying
frequency** live inside the model and drive the discrete activity states: a state-space harmonic
oscillator whose instantaneous frequency follows a random walk (smoothness prior), generating a phase
that gates a discrete rest/active HMM — a **switching state-space model**, fit by Kalman/particle
filtering or HMC. This is the machinery behind real-time instantaneous-frequency/phase trackers (the
eLife state-space phase estimator) and particle-filter circadian phase estimation. It estimates the
dynamic period *and* the states together, propagating uncertainty correctly.

**This is a methods-research project, not a packaging job.** No off-the-shelf validated
implementation of *this exact coupling* exists; the ingredients exist in the literature but
assembling them into a trustworthy circadian tool requires: model specification + priors; a stable
inference algorithm (particle filter / HMC); **identifiability analysis** (can the dynamic period and
the discrete states be recovered separately?); simulation validation (recover known time-varying τ;
degrade gracefully when rhythm is weak or data short); and benchmarking vs cosinor, wavelets, and the
fixed-period HMM. Natural output is a peer-reviewed method — *then* it is trustworthy enough to ship.
It also has a real chance of not working out (identifiability and data-hunger are genuine risks).
Prototyping is feasible, but its output should be labeled **research-grade and unvalidated** until
that work is done.

## Route C — Dynamic dwell times (HSMM angle)

A period also lives in **how long you stay** in each state. A **hidden semi-Markov model** with
time-varying sojourn-time distributions expresses a changing cycle length through dwell durations
rather than transition odds — useful if the biology is "longer/shorter bouts" rather than "shifted
transition timing." Non-homogeneous HSMMs with covariates exist (e.g. for toroidal/directional data).

## Caveats that make this nontrivial

- **Circularity / double-dipping.** If the wavelet estimates the period from the same activity bouts
  the HMM then models, the two-stage plug-in ignores feedback and **understates uncertainty**. Route
  B avoids this; Route A should at least propagate wavelet error and not treat `φ(t)` as exact.
- **Reliability where it matters least.** CWT phase is trustworthy only on a strong ridge and outside
  the cone of influence. In arrhythmic/low-power stretches the instantaneous period is noisy —
  exactly when rhythm is weak. Weight by ridge power; fall back to homogeneous transitions there.
- **Over-flexibility hides biology.** A fully free period can absorb genuine **arrhythmicity** into
  "the period just changed," masking loss-of-rhythm. **Regularize** the rate of frequency change
  (smoothness prior / small random-walk variance) or use piecewise-constant τ.
- **Data hunger.** Resolving a time-varying frequency needs many cycles. With a few days, a
  slowly-varying or piecewise-constant period is far more defensible than a freely wandering one, and
  identifiability degrades as flexibility grows.

## Applied vs research summary

| | Route A (plug-in) | Route B (joint state-space) | Route C (HSMM dwell) |
|---|---|---|---|
| Maturity | existing, validated tools | research-grade, novel coupling | established class, less common for this |
| Effort | integration / engineering | methods development + validation | moderate–high |
| Trust on day 1 | shippable feature | unvalidated until benchmarked | depends |
| Best when | wavelet τ is reliable; want a real feature now | want joint estimation + honest uncertainty | period ≈ bout-length changes |

**Practical read:** Route A now, as a library feature; Route B as a separate exploratory track if
stats/methods expertise is available.

## Tie-back to `circadiandynamics`

This is the concrete bridge between the two missing axes from [03](03-cutting-edge-and-gaps.md): the
library's eventual **wavelet** module (instantaneous τ/φ) becomes the **covariate generator** for a
**cyclic HMM** module. For Trikinetics/DAM and VitalPatch (square-wave, fragmented, gappy) signals, a
wavelet-driven non-homogeneous HMM would track a free-running or jet-lagged clock *and* emit
clinically meaningful state metrics (fragmentation, dichotomy index, Rhythm Index) — something
neither cosinor nor a fixed-24h HMM can do.

## Sources
- [hmmTMB: HMMs with flexible covariate effects (arXiv 2211.14139)](https://arxiv.org/abs/2211.14139) · [JSS version](https://www.jstatsoft.org/article/view/v114i05/4748)
- [Non-homogeneous HMMs with covariate-driven transitions](https://www.sciencedirect.com/science/article/abs/pii/S0167947319301951) · [Nonhomogeneous hidden semi-Markov models (JRSS-C)](https://academic.oup.com/jrsssc/advance-article/doi/10.1093/jrsssc/qlae049/7821074)
- [A state-space modeling approach to real-time phase estimation (eLife)](https://elifesciences.org/articles/68803)
- [Model-based human circadian phase estimation using a particle filter](https://pubmed.ncbi.nlm.nih.gov/21257371/) · [Wearable data assimilation to estimate circadian phase (SIAM J. Appl. Math.)](https://epubs.siam.org/doi/10.1137/22M1509680)
- [Analysis of complex circadian time series using wavelets (ridge → instantaneous period/phase)](https://link.springer.com/protocol/10.1007/978-1-0716-2249-0_3)
