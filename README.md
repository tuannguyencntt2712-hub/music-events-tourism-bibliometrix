# Music Events & Tourism — Replication Package (2000–2024)

This repository publishes the **cleaned dataset** and **R/bibliometrix scripts** for the study:
**“Music Events & Tourism: insights from a scoping review and bibliometric performance analysis (2000–2024)”**.

> **Reproducibility scope (for reviewers):**  
> - Cleaned data (with manual screening applied) is included at `data_clean/scopus_clean.csv`.  
> - R scripts (bibliometrix-based) are in `code/`.  
> - Exact software versions and key parameters are listed below.  
> - Outputs correspond to **Tables 3, 5, and 6** in the manuscript.

---

## How to reproduce

### Requirements
- **R 4.5.1** (RStudio 2025.05.1-513)
- Packages: `bibliometrix (5.1.0)`, `readr`, `jsonlite`, `dplyr`, `tidyr`, `stringdist`, `data.table`
- OS: any (tested on Windows)

### Inputs
- `data_clean/scopus_clean.csv` — **pre-cleaned** Scopus export (manual screening was done outside the code).

### Run
From the repository root:
```bash
Rscript code/Rsimple.R

