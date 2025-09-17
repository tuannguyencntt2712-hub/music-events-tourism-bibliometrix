# Data dictionary — scopus_clean.csv

| Column | Description |
|---|---|
| EID | Scopus record identifier. |
| DOI | Digital Object Identifier (normalized). |
| TI | Title. |
| AU | Authors (semicolon-separated). |
| AF | Affiliations (semicolon-separated). |
| SO | Source title (journal). |
| PY | Publication year. |
| DT | Document type (Article). |
| LA | Language (English). |
| DE | Author keywords. |
| ID | Index keywords. |
| TC | Total citations at the freeze date (10 July 2025). |
| CPY | Citations per year (reference year = 2025). |
| Country | Country/territory (if derived). |
| ASJC | Scopus All Science Journal Classification code(s). |

**Notes**
- TC and CPY reflect the **freeze date** and **reference year** above. 
- Some papers may belong to multiple ASJC fields (full counting).
