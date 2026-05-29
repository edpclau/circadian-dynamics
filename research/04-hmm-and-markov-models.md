# 04 — Markov chains & (cyclic) HMMs in rhythm analysis

*Compiled 2026-05-29. How (cyclic) Hidden Markov Models and Markov chains fit into rhythm analysis,
and how they compare to Lomb–Scargle, the Short-Time Fourier Transform (STFT), and wavelets.*

## The core conceptual split

Lomb–Scargle, STFT, and wavelets are **frequency-domain** tools. They decompose a signal onto an
oscillatory (sinusoidal/wavelet) basis and answer *"what period(s) are present, and — for
STFT/wavelet — when?"* They assume the rhythm **is** a (quasi-)sinusoidal waveform.

Markov chains and HMMs are **state / time-domain** tools. They model the organism as occupying
discrete latent states (rest/active, sleep stages, low/med/high activity) with probabilistic
transitions, and answer *"what state is the system in, and how do the switching dynamics carry the
rhythm?"* The rhythm is **not** a fitted sinusoid — it is encoded in **time-varying transition
probabilities**. That difference drives the whole comparison.

## Three roles they play

**1. State segmentation / decoding.** Classify each timepoint into a state while respecting temporal
dependence. Dominant in **automated sleep staging** from EEG (HMMs, HDP-HMMs with multitaper
spectral emissions, transition-constrained HMMs; ~80–87% accuracy, κ ≈ 0.7–0.85). The Markov
property regularizes noisy per-epoch classification, suppressing implausible state flips.

**2. Rhythm quantification via *cyclic* transition probabilities (the genuinely circadian use).**
Transition-matrix entries become **periodic functions of time-of-day** — a 24-h cosine inside a
multinomial-logistic link, or a circadian oscillator entered as a covariate. The circadian signal
lives in *how the propensity to switch rest↔active waves over the day.* Anchor papers:
   - **Huang & Finkenstädt 2018** (J. R. Soc. Interface) — a 3-state *harmonic HMM* (Inactive /
     Moderately Active / Highly Active, Gaussian emissions on √-activity). Homogeneous version =
     fixed transitions; harmonic version makes each transition probability follow a 24-h cosine.
     Outputs interpretable state-based metrics: **rest amount**, **center of rest**, a novel
     **Rhythm Index** (0 = arrhythmic, 1 = perfect square-wave rest), activity amplitude, the
     **dichotomy index (I<O)**, and a **sleep-interruption rate**.
   - **Bayesian Circadian HMM (BCHMM, 2023)** — embeds state-specific sinusoidal transition
     functions, infers the 24-h rest-activity profile through time-varying state probabilities,
     reports better state separation (smaller KL divergence) than frequentist fits, and handles
     label-switching via the Bayesian prior.

**3. Generative models of sleep–wake architecture / bout structure.** Continuous- or discrete-time
Markov chains over sleep/wake (2-state: 4 params; 4-state mouse models with light/dark-dependent
rates) capture **ultradian bout dynamics** and how circadian/photic drive gates them.

## Strengths (vs cosinor & periodograms)

- **Non-sinusoidal / square-wave activity.** Rest↔active transitions are abrupt; "smooth functional
  forms such as harmonic functions or splines are not ideal for modelling the abrupt appearance of
  transitions." Exactly where cosinor/Fourier are mis-specified.
- **Heterogeneous variance across the cycle** (active phase noisier than rest) — modeled natively via
  state-specific emission variances.
- **Time-dependence/autocorrelation is the model**, not a nuisance.
- **Missing data** handled cleanly by propagating the last transition matrix (the forward algorithm
  marginalizes gaps) — no zero-stuffing.
- **Individual-specific, probabilistic thresholding** instead of universal activity cut-points.
- **Clinically interpretable outputs** (fragmentation, dichotomy index, transition rates) that a
  power spectrum does not produce.

## Weaknesses / limits

- **They usually *assume* the period rather than estimate it.** Most circadian HMMs impose a 24-h
  cosine in the transition link — **confirmatory** ("how strong/where is the ~24-h rhythm") more
  than **exploratory** ("what is the unknown free-running τ?"). To discover an unknown period or
  ultradian components you still need a periodogram or wavelet. (See [05](05-dynamic-period-hmm.md)
  for how to relax this.)
- **They need discrete, interpretable states** — natural for activity/sleep, awkward for continuous
  molecular/luminescence signals with no obvious "state."
- **Not a spectral decomposition** — won't separate co-existing periodic components or yield a power
  spectrum.
- **Modeling overhead** — state count, emission family (zero-inflation at short epochs),
  label-switching, EM local optima. Heavier than running `lsp()`.

## How they stand vs Lomb–Scargle / STFT / wavelets

They are **complementary, answering different questions** — not rungs on one ladder.

| Axis | Lomb–Scargle | STFT | Wavelet (CWT) | Cyclic HMM / Markov |
|---|---|---|---|---|
| Domain | frequency | time–frequency | time–frequency, multiresolution | latent state / transition dynamics |
| Core question | what period(s)? | when does a period appear? | instantaneous period/amp/phase over days | what state, and how does switching cycle? |
| Waveform assumption | sinusoidal | sinusoidal | wavelet (flexible) | **none** — arbitrary emissions, fits square wave |
| Time-varying period | ❌ (assumes stationary) | partial (fixed window) | ✅ best-in-class | indirect; period usually **imposed** at 24 h |
| Irregular sampling / gaps | ✅ its strength | needs even/interp | needs even | even grid; gaps handled by marginalization |
| Estimate unknown free-running τ | ✅ | ✅ | ✅ | ✗ not its job |
| Characteristic outputs | period, power, p-value | spectrogram | scalogram, ridge, instantaneous τ/amp/phase | states, transition rates, RI, dichotomy index, fragmentation |
| Best at | "is it rhythmic & at what period," noisy/gappy data | quick time-localized spectral look | **how period/amplitude drift across days** | **rest/sleep architecture, fragmentation, non-sinusoidal clinical monitoring** |

Two framing points:
- **STFT vs wavelet** is a *resolution* story: STFT uses a **fixed** window → constant
  time-frequency tradeoff (fixed Heisenberg box); wavelets **scale** the window (narrow at high
  frequency, wide at low) → better across the broad circadian+ultradian band. **Lomb–Scargle** is
  the uneven-sampling/gappy-data champion but assumes **stationarity** (one period for the whole series).
- **HMM is orthogonal to all three.** It is not doing frequency decomposition. Use it when the signal
  is fundamentally a *sequence of discrete states whose transition dynamics carry the circadian
  information*, and you want interpretable, time-dependence-respecting metrics with principled
  missing-data handling and (Bayesian) uncertainty.

**Practical heuristic:** periodogram/wavelet to *find and track* the period; cyclic HMM to *model
the state dynamics* and quantify rhythm robustness/fragmentation once the ~24-h structure is assumed.
Often used in sequence, not as substitutes.

## Relevance to `circadiandynamics`

The library has **no state-space/HMM capability** — it is entirely frequency-domain (windowed
LSP/ACF + sinusoidal cosinor) and stuffs gaps with zeros. Yet its **Trikinetics/DAM** and especially
**VitalPatch** (human wearable) inputs are exactly the abrupt, square-wave, fragmented, gap-laden
rest/active signals where cosinor is mis-specified and a **cyclic HMM is better matched** — and where
the clinically useful outputs live. This is a third missing axis alongside wavelets (time-varying
period) and mixed-effects/differential-rhythmicity statistics.

## Tooling

R — `depmixS4`, `momentuHMM`, `hmmTMB`, `msm` (continuous-time), plus the authors' MATLAB
Time-Varying HMM repo (oliver-carr). Python — `hmmlearn`, `pomegranate`, `ssm` (switching/state-space).

## Sources
- [Huang et al. 2018 — HMMs for monitoring circadian rhythmicity (J R Soc Interface)](https://pmc.ncbi.nlm.nih.gov/articles/PMC5832732/) · [journal version](https://royalsocietypublishing.org/doi/10.1098/rsif.2017.0885) · [Time-Varying HMM code](https://github.com/oliver-carr/Time-Varying-Hidden-Markov-Model)
- [Bayesian Circadian HMM (BCHMM) — arXiv 2307.03832](https://arxiv.org/abs/2307.03832)
- [HMM combining activity + self-reported sleep — arXiv 2212.11224](https://pmc.ncbi.nlm.nih.gov/articles/PMC10810673/)
- [Hidden Markov Models for Analysis of Biological Rhythm Data (Springer)](https://link.springer.com/chapter/10.1007/978-1-4613-0035-9_12)
- [Four-state Markov model of sleep-wakefulness in mice](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5755762/) · [Ultradian regulation of rest/activity bouts (bioRxiv)](https://www.biorxiv.org/content/10.1101/836478.full.pdf)
- [Transition-constrained discrete HMM for sleep staging](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3462123/) · [Multitaper-spectral HDP-HMMs for EEG sleep — arXiv 1805.07300](https://arxiv.org/pdf/1805.07300)
- [Wavelet analysis of circadian/ultradian rhythms (J Circadian Rhythms)](https://jcircadianrhythms.com/articles/10.1186/1740-3391-11-5) · [Lomb-Scargle for circadian (Smith College)](https://sites.smith.edu/circada/4-lomb-scargle/)
