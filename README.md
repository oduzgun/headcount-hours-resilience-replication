# Headcount or Hours Resilience? — R3 Replication Code and Data

**Research status:** Software and frozen replication material for the unpublished study *Headcount or Hours Resilience? Measure-Dependent Regional Rankings during the COVID-19 Shock*. The manuscript and online appendix are not included in this code release.

This descriptive research compares regional labour-market resistance measured by **employed persons** versus **hours actually worked**, holding the 244 EU-27 NUTS-2 regions, sample, benchmarks, formulae, time windows and ranking rules fixed. The analysis **does not** identify causal policy or pandemic effects.

## Cite this replication release

The archived R3 replication package has DOI **[10.5281/zenodo.24093367](https://doi.org/10.5281/zenodo.24093367)**. Cite the research software separately from the unpublished manuscript:

> Düzgün, O. (2026). *Headcount or Hours Resilience? Replication Materials* (R3) [Software]. Zenodo. https://doi.org/10.5281/zenodo.24093367

The immutable GitHub source snapshot is [tag `R3`](https://github.com/oduzgun/headcount-hours-resilience-replication/releases/tag/R3). The `main` branch may receive documentation updates after archival without changing that tagged snapshot.

## Reproduce the results

Use **R 4.5.3** with `data.table`, `ggplot2` and `scales` installed. At the repository root:

```bash
Rscript --vanilla code/replication_244.R
```

The script uses the six frozen analytical inputs under `data/frozen_244/`, verifies source hashes, reconstructs the analytical outputs, and performs **28 independent analytical validation gates**. A separate clean GitHub Actions run with R 4.5.3 passed all 28 gates on 9 November 2026 and numerically reproduced the frozen CSVs to a declared `1e-8` tolerance.

Run the shipped source/manifest validator with Python 3.10 or later:

```bash
python scripts/verify_release.py
```

The repository's CI runs the R reproduction from an isolated temporary copy rather than overwriting the frozen output files; see `.github/workflows/replication.yml`.

## Package contents

- `code/`: authoritative R3 replication script and appendix builder
- `data/frozen_244/`: six frozen analytical inputs
- `figures/`: figure sources and underlying figure data
- `output/release_244/`: 22 archived R3 scientific output payloads plus their original output manifest
- `RELEASE_MANIFEST.json`: checksums for all distributed files, preserving 37 original-science SHA-256 fingerprints
- `DATA_ATTRIBUTION.md`: Eurostat sources, source vintages, transformation and reuse disclosures

The sectoral extension contains frozen, checked intermediate data and is **not** a full reconstruction of every sectoral series from the raw Eurostat downloads. The frozen output metadata preserve their historical pipeline-stage labels; these labels do not claim publication or acceptance.

## Source attribution and license

Source: Eurostat regional national accounts, series `nama_10r_2emhrw` and `nama_10r_3empers` (accessed 12 August 2026). The datasets in this release are author-generated derivatives; Eurostat does not endorse or accept responsibility for the transformations or interpretations. See `DATA_ATTRIBUTION.md` for the source terms and limitations.

Original code is distributed under `LICENSE` (MIT); the Eurostat-derived data remain subject to Eurostat's applicable reuse conditions. Citation metadata for this software release, including its Zenodo DOI, are in `CITATION.cff`. The accompanying manuscript remains unpublished.
