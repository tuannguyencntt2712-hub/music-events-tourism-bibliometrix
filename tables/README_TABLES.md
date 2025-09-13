# README_TABLES
This package reproduces Tables 1, 3, and 4 and the annual trends series from a pre-cleaned Scopus corpus.

## Software & parameters
- R packages: bibliometrix, readr, jsonlite, dplyr, tidyr, stringdist, data.table (optional: openxlsx).
- Analysis window: year_min = 2000, year_max = 2025.
- **Reference year for CPY and m-index: CPY_REF_YEAR = 2025**.
- Ranking rule (Table 4): TC ↓ → CPY ↓ → Year ↓ → DOI present → Title A–Z.
- Local citations are computed (DOI → EID → fuzzy, Jaro–Winkler ≥ 0.92) for diagnostics/coverage only and are NOT displayed in Table 4.

## Table 1 – Dataset-level metrics
- Timespan = min(PY)–max(PY); length = max(PY) - min(PY) + 1.
- Cited documents = count(TC > 0).
- Citations per document = mean(TC).
- Citations per cited document = sum(TC) / count(TC > 0).
- Citations per year = sum(TC) / length.
- Citations per author = sum(TC) / unique contributing authors.
- Authors per document = mean(authors in AU).
- h, g computed on corpus TC; m = h / length.
- h-core total citations = sum of TC among top-h documents.

## Table 3 – Year-level cohort metrics
- For each year y: TP, NCP (TC>0), NCA (unique authors), TC, C/P = TC/TP, C/CP = TC/NCP.
- m(y) = h(y) / age_years with age_years = max(1, CPY_REF_YEAR - y + 1).

## Table 4 – Top-K most cited (CPY displayed, LC suppressed)
- Default K = 40; coverage τ is reported at K and at Kτ in JSON.

All outputs are generated under `tables/`. The input file is assumed pre-cleaned and is not altered by this script.
