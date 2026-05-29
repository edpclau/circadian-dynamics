# 02 — Equivalent libraries in other languages

*Compiled 2026-05-29.*

## The R neighborhood (closest competition)

- **MetaCycle** — the de-facto standard; runs JTK_CYCLE + Lomb–Scargle + ARSER and meta-combines
  them via Fisher's method.
- **DiscoRhythm** (Bioconductor + Shiny) — Cosinor, JTK, ARSER, Lomb–Scargle behind one UI; the
  polished, validated version of what `circadiandynamics` offers (JTK/LS/ARSER via MetaCycle).
- **RAIN** — nonparametric; detects asymmetric (non-sinusoidal) waveforms.
- **cosinor / cosinor2 / cosinoRmixedeffects** — population and **mixed-effects** cosinor (the
  multi-subject statistics `circadiandynamics` lacks).
- **rethomics** — gold-standard ecosystem for high-throughput *Drosophila* behavior (DAM data,
  sleep, actograms) — direct overlap with this library's behavioral side.
- **nparACT / GGIR / actiCircadian** — non-parametric actigraphy metrics (IS, IV, RA, M10/L5) for
  human wearables — the metrics this library does *not* compute.
- **LimoRhyde / dryR / compareRhythms / diffCircadian / DODR** — differential rhythmicity between
  conditions.

## Python

- **CosinorPy** — single- and multi-component cosinor, automatic best-period/best-model selection,
  population & count models, publication figures.
- **pyBOAT** — **wavelet** (continuous wavelet transform) toolkit giving *instantaneous*
  period/amplitude/phase that change over time — the rigorous version of the "track the clock over
  the recording" goal `circadiandynamics` approximates with sliding windows.
- **CosinorAge / actipy / scikit-digital-health** — wearable/accelerometer circadian feature extraction.

## MATLAB / Java / ImageJ

- **ClockLab** (Actimetrics, MATLAB) — commercial standard for actograms + chi-square periodogram.
- **ActogramJ** (ImageJ) — free actograms, periodograms.
- **ShinyR-DAM, VANESSA, Rtivity** — Shiny/GUI front-ends for DAM / behavioral data.

## Web platforms

- **BioDare2** (Edinburgh) — the FAIR community repository + analysis hub: Enright & Lomb–Scargle
  periodograms, **FFT-NLLS, mFourfit, MESA, Spectrum Resampling**, plus storage, sharing,
  visualization.

## Capability comparison

| Capability | circadiandynamics | MetaCycle/DiscoRhythm | pyBOAT | BioDare2 | rethomics |
|---|---|---|---|---|---|
| Lomb–Scargle | ✅ | ✅ | (wavelet) | ✅ | – |
| Autocorrelation | ✅ | – | – | – | ✅ |
| Cosinor | ✅ (1-comp) | ✅ | – | ✅ (FFT-NLLS) | – |
| **Sliding-window / time-resolved** | ✅ (its niche) | – | ✅ (wavelet, rigorous) | partial | – |
| Multi-individual fan-out + parallel | ✅ | ✅ | – | ✅ | ✅ |
| Reads behavioral *and* wearable formats | ✅ (rare) | – | – | – | behavioral |
| Differential rhythmicity stats | ❌ | partial | ❌ | ❌ | ❌ |
| Multiple-testing / mixed effects | ❌ | ✅ | ❌ | ✅ | – |
| Tests / CRAN / validation | ❌ | ✅ | ✅ | hosted | ✅ |

**Takeaway:** the LSP + ACF + cosinor menu is conventional and well-covered elsewhere. The
differentiators are per-window time-resolved tracking across many individuals and the broad reader
set spanning insect behavior and human wearables.

## Sources
- [CosinorPy (BMC Bioinformatics)](https://link.springer.com/article/10.1186/s12859-020-03830-w) · [CosinorPy GitHub](https://github.com/mmoskon/CosinorPy)
- [pyBOAT preprint](https://www.biorxiv.org/content/10.1101/2020.04.29.067744.full.pdf)
- [MetaCycle / DiscoRhythm (Bioinformatics)](https://academic.oup.com/bioinformatics/article/36/6/1952/5614815) · [cosinoRmixedeffects](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8590130/)
- [BioDare2](https://biodare2.ed.ac.uk/) · [Period estimation in BioDare2 (protocol)](https://link.springer.com/protocol/10.1007/978-1-0716-1912-4_2)
- [ShinyR-DAM (Comms Biology)](https://www.nature.com/articles/s42003-018-0031-9) · [VANESSA](https://www.researchgate.net/publication/358858659_VANESSA-Shiny_Apps_for_Accelerated_Time-series_Analysis_and_Visualization_of_Drosophila_Circadian_Rhythm_and_Sleep_Data)
- [nparACT](https://pmc.ncbi.nlm.nih.gov/articles/PMC4890079/) · [actiCircadian](https://github.com/nsrr/actiCircadian)
