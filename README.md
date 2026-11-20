# Fixed-reference Benefit-of-the-Doubt index on Alkire-Foster censored headcounts

Replication package for the article *Endogenous weights and a fixed reference in territorial
multidimensional poverty measurement* (Brandão, Mariano and
Rebelatto). The package reproduces the scores, tables, figure and checks of the article from the
public microdata of the Brazilian Demographic Census (IBGE). The main application uses the 2010 and
2022 censuses; an optional historical extension adds the 1991 and 2000 census samples.

## Method in short

1. **Identification (Alkire-Foster).** Twelve deprivation indicators in four dimensions, equal
   weights 1/12 and poverty cut-off k = 1/3, following the IPM-Brasil definitions (income line of
   one quarter of each year's minimum wage). Person-level indicators (education, work) are
   assigned to the household when at least one resident is deprived; every resident shares the
   household profile.
2. **Profiles.** For each federative unit (UF, i.e., state) and year, the censored headcount of each
   indicator: the share of persons who are poor and deprived in that indicator. M0 is their mean.
3. **Score.** Benefit of the Doubt with unit input against a fixed reference B, the 27 state
   profiles of 2010: S(h) = max w'h subject to Bw <= 1, w >= 0 and w_j >= (tau/12) sum(w). The
   restriction does not depend on the evaluated profile, so the admissible set of weights is the
   same in every evaluation. tau = 0 is the unrestricted model; tau = 1 gives S = M0 / max M0 of
   the reference. Main specification: tau = 0.5.
4. **Aggregation.** Population-weighted mean of the state scores (regions and Brazil).

## Requirements

* R 4.2 or later with `data.table`, `lpSolve`, `rDEA` (GLPK, second solver) and `Benchmarking`
  (check of the unrestricted model). The optional historical extension also requires `haven` and
  `tidyselect`.
  ```r
  install.packages(c("data.table", "lpSolve", "rDEA", "Benchmarking", "haven", "tidyselect"))
  ```
* The command-line tools `unzip` and `awk` (Linux, macOS or WSL).
* Full raw-data processing needs substantial memory and disk space. The package was validated with
  R 4.5.3 on macOS.

## Data

| Census | Source | Files |
|---|---|---|
| 2010 | IBGE, Censo Demográfico 2010, Resultados Gerais da Amostra, microdados (2010 Census sample microdata). <https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/> | one zip per state (São Paulo in two parts) and `Documentacao.zip` |
| 2022 | IBGE, Censo Demográfico 2022, microdados da amostra de acesso público (2022 Census public-use sample microdata). <https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Microdados_e_Areas_de_Ponderacao/Microdados_de_acesso_Publico/csv/> | `Todas_as_UFs.zip` |
| 1991, 2000 (optional) | IBGE census samples in SPSS format, obtained independently | see "Historical extension" below |

Stage 01 downloads the missing 2010 and 2022 files and records their SHA-256
(`output/inputs_sha256.csv`). IBGE releases corrections to the microdata; a different release may
change the results slightly. Paths can be set with `BOD_CENSUS2010_DIR`, `BOD_CENSUS2022_ZIP`,
`BOD_WORK_DIR` and `BOD_OUTPUT_DIR`. Indicator definitions and census variable codes are in
[`docs/indicators.md`](docs/indicators.md).

## Running

Three entry points, from the fastest to the most complete:

**1. Check the archived results (no microdata needed).**

```sh
Rscript tests/check_results.R
Rscript tests/review_checks.R
```

The second script checks the subgroup-consistency counterexample, the population-standardized
change, the 108 state-year AF profiles and 1,296 censored headcounts against independently computed
IPM-Brasil aggregates (`docs/af_reference_*.csv`), the 324 re-estimated scores and the territorial
subgroup decomposition. Its outputs are written to `output/audit/`.

**2. Re-estimate the model from the archived profiles.**

```sh
Rscript tests/prepare_aggregate_profiles.R
Rscript run_all.R 05 08 10 11
```

Stages 05 and 08 recompute the scores and the numerical example; stage 10 redraws the figure.
Stages 06 and 07 need the household files built by stages 02 and 03.

**3. Rebuild everything from the public census files.**

```sh
Rscript run_all.R            # all stages and the tests
Rscript run_all.R 05 06 07   # selected stages (list each stage)
```

| Stage | Script | What it does | Main outputs |
|---|---|---|---|
| 01 | `01_download.R` | downloads missing files; checksums | `inputs_sha256.csv` |
| 02 | `02_build_census2010.R` | households of 2010 with the 12 indicators, from the fixed-width files; checks person records against V0401 | `work/households_2010.rds`, `base_2010_counts.csv` |
| 03 | `03_build_census2022.R` | households of 2022 with the same indicators | `work/households_2022.rds`, `base_2022_counts.csv` |
| 04 | `04_af_profiles.R` | AF identification; censored headcounts, H, A and M0 by state, region and Brazil; robustness profiles | `af/` |
| 05 | `05_scores.R` | scores against the fixed 2010 reference and against each year's own reference, for every tau; aggregates; weights | `scores/` |
| 06 | `06_audit.R` | independent solvers; interference under moving and fixed references; monotonicity in the profile and at household level; componentwise within-state worsening | `audit/` |
| 07 | `07_robustness.R` | real income line, harmonized school delay, four dimensions, eleven indicators, suppressed ages; pooled, normative and leave-one-out references; rank ranges over tau; dominant indicator with its range over optimal solutions; bootstrap | `robustness/` |
| 08 | `08_numerical_example.R` | numerical example of the article | `numerical_example/` |
| 09 | `09_series_1991_2022.R` | optional historical extension (see below) | `series/` |
| 10 | `10_figure.R` | figure of state scores in 2010 and 2022 | `figure/figure_1.png` |
| 11 | `11_manifest.R` | parameters, session information, output checksums | `outputs_sha256.csv` |

### Historical extension (1991 and 2000)

Obtain the two SPSS census samples independently, install `haven` and `tidyselect`, then run:

```sh
export BOD_CENSUS1991_SAV="/path/to/Censo.1991.brasil.amostra.25porcento.sav"
export BOD_CENSUS2000_SAV="/path/to/Censo.2000.brasil.amostra.10porcento.sav"
Rscript run_all.R 09 11
```

Stage 09 uses the 2010 and 2022 profiles from stage 04 and skips the reconstruction when the files
are not available; the archived historical results remain available for inspection, but a skipped
stage does not certify their reconstruction. File names, sizes and SHA-256 of the inputs used are
listed in `output/audit/input_series_sha256.csv`. These files are not downloaded automatically and
are not redistributed.

## Interpretation and scope

The reference is a frozen copy of the 27 state profiles of 2010; changing a currently evaluated
state does not change it, and re-estimating the reference is a different experiment. A positive
coefficient restriction yields a strict response to an increase in a censored headcount; it does
not imply a response when an additional deprivation leaves a household non-poor.

Subgroup consistency applies to groups of whole states with fixed population shares. It is not
claimed for arbitrary partitions of persons within a state; an executable counterexample is
included. The national mean falls by 50.8% with each year's population shares and by 50.3% with
the 2010 shares held fixed. These are descriptive changes, not causal policy effects.

At tau = 1 the score equals normalized M0. The restriction concerns coefficients (equivalently, a
fixed unit profile), not state-specific virtual contributions. Indicator contributions are a
conditional diagnostic; they do not identify welfare weights or optimal budgets.

## Verification

The package includes code, documentation and aggregate results; it contains no person-level
microdata and no household intermediate files. The 324 scores agree with GLPK within 6.47e-13 and
the unrestricted scores agree with `Benchmarking` within 2.11e-15; AF comparisons differ by less
than 1e-10. `manifest_sha256.json` identifies every file of the package except itself.

## Citation and license

Cite this software using [`CITATION.cff`](CITATION.cff). Archived version 1.1.0 (Zenodo):
<https://doi.org/10.5281/zenodo.24407335>; version 1.0.0: <https://doi.org/10.5281/zenodo.23673342>. Code and documentation: MIT (see `LICENSE`). The census
microdata are public data distributed by IBGE and are not redistributed here.
