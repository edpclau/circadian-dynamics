# 03 — Cutting edge & the implementation gap

*Compiled 2026-05-29.*

## State of the art / what researchers need

1. **Time-resolved over static.** The field has moved past "one period per recording." Real clocks
   change period/amplitude/phase across days (development, jet-lag, disease, aging). **Wavelets**
   (pyBOAT; Leise & Harrington) are the modern standard for this — directly relevant to what this
   library reaches toward with windows.
2. **No single best period method.** Systematic evaluations (Zielinski et al., PLOS ONE 2014; the
   2020/2021 genome-wide benchmark) show method choice depends on noise, gaps, waveform shape, and
   damping — hence ensemble tools (MetaCycle) and recommendation guidelines. Damped rhythms remain
   hard (ECHO was built specifically for them).
3. **Differential rhythmicity is the hot frontier.** A 2024 comparative study evaluated **seven**
   algorithms (DODR, LimoRhyde, CircaCompare, compareRhythms, diffCircadian, dryR,
   RepeatedCircadian) and found they disagree because they *define* "differentially rhythmic"
   differently. Comparing rhythms *between groups/conditions* is where method development is
   concentrated.
4. **Genome-wide / omics rhythmicity.** JTK_CYCLE, eJTK, RAIN, BIO_CYCLE, ECHO, likelihood-based
   tests for transcriptomics; pipelines like DiffCircaPipeline.
5. **Circadian medicine — phase from a single sample.** ZeitZeiger (~2.1 h error, 15 genes),
   TimeSignature, BodyTime/metabolomics, TimeTeller, and platform-independent predictors estimate
   **internal biological time from one blood draw** — a major translational thrust toward chronotherapy.
6. **Wearables at population scale.** Non-parametric actigraphy metrics (IS, IV, RA, M10/L5) as
   digital biomarkers of aging/health (npj Digital Medicine 2024); routinely-collected clinical data
   for circadian medicine.
7. **FAIR / reproducibility / standardization.** Repeatedly flagged as the bottleneck: no common
   interoperable format across organisms/instruments, weak metadata adoption, longitudinal-data
   sharing friction. Community responses include BioDare2 and README-template standards for chronobiology.

## The implementation gap

**In one line:** the field has *strong but fragmented* tools — rigorous methods (wavelets,
mixed-effects cosinor, differential rhythmicity, omics) live in separate packages and ecosystems,
while *broad, easy, multi-instrument, time-resolved* pipelines are rare, often unvalidated, and not
FAIR-integrated.

Mapped to unmet needs:
1. **Time-varying rigor.** Sliding windows are a coarse approximation of wavelets. The honest upgrade
   is a **CWT backend** (pyBOAT-style instantaneous period/amplitude) alongside windowed LSP/ACF.
2. **Between-group statistics.** Nobody in the behavioral/wearable space cleanly bridges
   single-individual period estimation → **population/mixed-effects cosinor → differential
   rhythmicity** (CircaCompare-style). That bridge, on top of this library's multi-individual
   fan-out, is a real opening.
3. **Multiple-testing discipline.** Scanning individuals × windows without FDR control invites false
   positives — a quick, high-value fix.
4. **Validation & trust.** Competitors win on tests, benchmarks, and CRAN/Bioconductor presence. The
   Granger episode shows the cost of shipping unvalidated novel statistics; the `rythm_strength`
   "beta" metric is the same risk live today.
5. **FAIR I/O.** Extend the broad reader set to **BioDare2 import/export** and a standardized
   metadata README to plug into the community standard.
6. **State-based models.** A **cyclic HMM** is methodologically better matched to the square-wave,
   fragmented, gappy rest/active signals from Trikinetics/DAM and VitalPatch than cosinor is — see
   [04 — HMM & Markov models](04-hmm-and-markov-models.md) and [05 — Dynamic-period HMMs](05-dynamic-period-hmm.md).

## Prioritized recommendations

1. **De-duplicate the `_2` twins**; pick one canonical function per job; remove or quarantine the
   Granger pipeline (still wired into core despite the README claiming removal).
2. **Add `testthat` + GitHub Actions CI**, and validate cosinor/LSP outputs against
   MetaCycle/DiscoRhythm/CosinorPy on a shared dataset.
3. **Fix** the `binning_n` latent bug, the `ofac` README guidance, and the `future::plan()` side effects.
4. **Add FDR correction** across the individual×window grid.
5. **Strategic features:** mixed-effects/population cosinor → differential-rhythmicity comparisons;
   a wavelet backend; a cyclic-HMM module for the wearable/behavioral use case.
6. **Polish for adoption:** fix typos, switch to `Authors@R`, write a vignette, pursue
   CRAN/Bioconductor or at least a BioDare2 bridge.

## Sources
- [Genome-wide circadian rhythm detection methods: evaluations & guidelines](https://pmc.ncbi.nlm.nih.gov/articles/PMC8138819/) · [ECHO (Bioinformatics)](https://academic.oup.com/bioinformatics/article/36/3/773/5544107) · [RAIN](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4266694/)
- [Strengths & limitations of period estimation methods (PLOS ONE 2014)](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0096462)
- [Comparative study of differential rhythmicity algorithms (2024)](https://pmc.ncbi.nlm.nih.gov/articles/PMC11440551/) · [DiffCircaPipeline](https://academic.oup.com/bioinformatics/article/39/1/btad039/6992656)
- [ZeitZeiger / blood clock](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5329904/) · [Internal circadian time from single blood sample (JCI)](https://www.jci.org/articles/view/120874) · [Body time from metabolomics (PNAS)](https://www.pnas.org/doi/10.1073/pnas.2212685120)
- [Accelerometry as aging biomarker (npj Digital Medicine 2024)](https://www.nature.com/articles/s41746-024-01111-x)
- [README standards in chronobiology](https://pmc.ncbi.nlm.nih.gov/articles/PMC10529918/) · [Routinely collected clinical data for circadian medicine (PLOS Digital Health)](https://journals.plos.org/digitalhealth/article?id=10.1371%2Fjournal.pdig.0000511)
- [Analysis of complex circadian time series using wavelets](https://link.springer.com/protocol/10.1007/978-1-0716-2249-0_3)
