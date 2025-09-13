# Music Events & Tourism — Replication Package (2000–2024)

This repository publishes the **cleaned dataset** and **R/bibliometrix scripts** for the study
“Music Events & Tourism: insights from a scoping review and bibliometric performance analysis (2000–2024).”
**Reference year for CPY and the m-index is fixed at `CPY_REF_YEAR = 2025`** to ensure temporal comparability.

## How to reproduce
1. Requirements: R ≥ 4.3.0; packages: `bibliometrix`, `readr`, `jsonlite`, `dplyr`, `tidyr`, `stringdist`, `data.table`.
2. Input: `data_clean/scopus_clean.csv` (pre-cleaned; manual screening outside code).
3. Run:
   ```bash
   Rscript code/01_descriptive_tables.R
