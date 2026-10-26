# Fixed-reference Benefit-of-the-Doubt index on Alkire-Foster censored headcounts

Replication package for manuscript v4 of “Pesos endógenos e referência fixa na mensuração territorial da pobreza multidimensional: evidência para o Brasil, 1991 a 2022”, by Moisés Luna Brandão, Enzo Barberio Mariano and Daisy Aparecida do Nascimento Rebelatto. Release 1.0.0, 26 October 2026. The manuscript is unpublished.

Download and extract `BOD-fixed-MPI_v1.0.0_artigo_v4.zip`. The archive contains the full English replication guide, R implementation, `run_all.R`, variable documentation, tests, aggregate results and SHA-256 manifest. Microdata and household intermediate bases are excluded. Code and documentation: MIT license; the full license and citation metadata are inside the archive.

## Requirements

R 4.2 or later; packages `data.table`, `lpSolve`, `rDEA` and `Benchmarking`. The optional historical extension additionally requires `haven` and `tidyselect`. Full raw-data processing uses `unzip` and `awk` on macOS, Linux or WSL and requires substantial memory and disk space. Validated with R 4.5.3.

## Replicate from the extracted archive

Run commands from the extracted package root.

```r
install.packages(c("data.table", "lpSolve", "rDEA", "Benchmarking", "haven", "tidyselect"))
```

Check archived aggregate results without downloading microdata:

```sh
Rscript tests/check_results.R
Rscript tests/review_v4.R
```

Re-estimate scores, the analytic example and the figure from archived profiles:

```sh
Rscript tests/prepare_aggregate_profiles.R
Rscript run_all.R 05 08 10 11
```

Rebuild the main 2010/2022 application from official public IBGE inputs:

```sh
Rscript run_all.R
```

Stage 01 downloads missing public inputs. Existing inputs may be supplied through `BOD_CENSUS2010_DIR` and `BOD_CENSUS2022_ZIP`. Stages 06 and 07 require household RDS intermediates built by stages 02 and 03.

The 1991/2000 extension requires independently obtained SPSS samples, identified by filename, size and SHA-256 in the archive. Set `BOD_CENSUS1991_SAV` and `BOD_CENSUS2000_SAV`, then run `Rscript run_all.R 09 11` after the main profiles exist. Stage 09 skips reconstruction if inputs are unavailable; this does not certify reproduction of historical results. See the complete archived README for source links, indicator definitions and the stage/output map.

## Interpretation

The index evaluates state AF censored headcount profiles against a frozen copy of the 27 state profiles of 2010, with a proportional coefficient restriction. Main tau = 0.5; tau = 1 recovers normalized M0. The admissible coefficient set is common to evaluations. Scalar subgroup consistency is claimed for groups of whole states with fixed population shares, not arbitrary within-state partitions of persons. The code includes a counterexample to the broader claim. Scores and indicator contributions are descriptive and do not identify causal policy effects or optimal budgets.

## Validation

324 scores checked against GLPK (maximum absolute difference 6.47e-13); unrestricted scores also checked with Benchmarking (2.11e-15). AF comparisons cover 108 state-year profiles and 1,296 headcounts from four censuses, with differences below 1e-10. The v4 review re-estimated aggregate scores and relevant tests; it did not rerun all raw-data processing. See archived audit outputs and English instructions for reproducibility limits.
