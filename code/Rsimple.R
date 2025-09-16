############################################################
# Rsimple.R
# Reproduces dataset-level (Table 3) and year-cohort metrics (Table 5),
# and constructs a Top-K ranking (Table 6).
# The corpus is assumed pre-cleaned (manual screening already applied).
# CPY and the m-index are fixed at CPY_REF_YEAR = 2025 for temporal comparability.
############################################################

# --- Packages & runtime setup -------------------------------------------------
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)

ensure_pkg <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}
ensure_pkg("bibliometrix")
ensure_pkg("readr")
ensure_pkg("jsonlite")
ensure_pkg("dplyr")
ensure_pkg("tidyr")

SEED_STABLE <- 20250710L
set.seed(SEED_STABLE)

# --- User parameters (repo-friendly) -----------------------------------------
PROJECT_ROOT   <- "D:/OneDrive - Trường Cao đẳng Du lịch Nha Trang/Tiến sĩ/Tự học/24.8.2025/R/Simple for reviewer/"
csv_path       <- file.path(PROJECT_ROOT, "data_clean", "scopus_clean.csv")
year_min       <- 2000L
year_max       <- 2024L          # analysis window 2000–2024
CPY_REF_YEAR   <- 2025L          # CPY/m-index anchored at 2025
REF_YEAR_FOR_M <- CPY_REF_YEAR
VERBOSE        <- TRUE
vmsg <- function(...) if (isTRUE(VERBOSE)) message(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...))

# --- Output folder ------------------------------------------------------------
OUTPUT_DIR <- file.path(PROJECT_ROOT, "tables")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
TS <- format(Sys.time(), "%Y%m%d-%H%M%S")

# --- Helpers ------------------------------------------------------------------
to_ascii <- function(x){
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("\uFF1A", ":", x, fixed = TRUE)
  x <- gsub("\u2013|\u2014", "-", x)
  x <- gsub("\u2044", "/", x, fixed = TRUE)
  x <- gsub("\uFF02", "\"", x, fixed = TRUE)
  x <- gsub("\uFF07", "'",  x, fixed = TRUE)
  x <- gsub("\u00A0", " ",  x, fixed = TRUE)
  x
}
normalize_doi <- function(x){
  x <- to_ascii(trimws(x))
  x <- gsub("^(?i)https?://(dx\\.)?doi\\.org/","", x, perl=TRUE)
  x <- gsub("^(?i)doi\\s*:?\\s*","", x, perl=TRUE)
  x <- sub("[?#].*$","",x); x <- URLdecode(x); x <- tolower(x)
  x <- sub("[\\.,;:\\)\\]\\'\"\\u2019]$","",x)
  ifelse(grepl("10\\.[0-9]{4,9}/[-._;()/:A-Z0-9]+", toupper(x)),
         sub(".*?(10\\.[0-9]{4,9}/[-._;()/:A-Z0-9]+).*","\\1", toupper(x)), x)
}
is_valid_doi <- function(x) grepl("^(?i)10\\.[0-9]{4,9}/[-._;()/:A-Z0-9]+$", x)
normalize_author_key <- function(x){
  x <- to_ascii(x); x <- toupper(trimws(x))
  x <- gsub("\\.+", "", x); x <- gsub("\\s+", " ", x)
  x
}
safe_chr <- function(x) { x <- ifelse(is.na(x), "", x); as.character(x) }
get_first_author <- function(au) {
  au <- safe_chr(au)
  first <- trimws(strsplit(au, ";", fixed = TRUE)[[1]][1])
  if (is.na(first) || !nzchar(first)) return("")
  if (grepl(",", first, fixed = TRUE)) trimws(strsplit(first, ",", fixed = TRUE)[[1]][1]) else strsplit(first, " +")[[1]][1]
}

# --- Import (pre-cleaned Scopus CSV) -----------------------------------------
if (!file.exists(csv_path)) stop(sprintf("CSV not found: %s", csv_path))
vmsg("Using CSV: ", normalizePath(csv_path, winslash="/"))
M <- bibliometrix::convert2df(file = csv_path, dbsource = "scopus", format = "csv")

# --- Year filter --------------------------------------------------------------
if (!"PY" %in% names(M)) stop("Column PY not found in the input data.")
M$PY <- suppressWarnings(as.integer(M$PY))
M <- M[!is.na(M$PY) & M$PY >= year_min & M$PY <= year_max, , drop = FALSE]
if (nrow(M) == 0) stop(sprintf("Empty corpus after year filter %d–%d.", year_min, year_max))

# --- Standardization ----------------------------------------------------------
if (!"TC" %in% names(M)) M$TC <- 0L
M$TC <- pmax(0L, suppressWarnings(as.integer(M$TC)))
if (!"C1" %in% names(M)) M$C1 <- ""

# --- Annual publications & citations -----------------------------------------
cit_per_year <- aggregate(TC ~ PY, data = M, sum, na.rm = TRUE)
names(cit_per_year) <- c("Year", "Total_Citations")
pubs_per_year <- as.data.frame(table(M$PY), stringsAsFactors = FALSE)
names(pubs_per_year) <- c("Year", "Total_Publications")
pubs_per_year$Year <- as.integer(as.character(pubs_per_year$Year))
years_full <- data.frame(Year = seq(year_min, year_max))
annual <- merge(years_full, pubs_per_year, by = "Year", all.x = TRUE)
annual <- merge(annual, cit_per_year,  by = "Year", all.x = TRUE)
annual$Total_Publications[is.na(annual$Total_Publications)] <- 0L
annual$Total_Citations[is.na(annual$Total_Citations)]     <- 0L
write.csv(annual, file.path(OUTPUT_DIR, paste0("annual_publications_and_citations_", TS, ".csv")), row.names = FALSE)

# --- Table 3 (dataset-level indicators) --------------------------------------
au_counts <- if ("AU" %in% names(M)) vapply(strsplit(M$AU, ";", fixed=TRUE), function(x) sum(nzchar(trimws(x))), integer(1)) else rep(NA_integer_, nrow(M))
authors_per_doc <- round(mean(au_counts, na.rm=TRUE), 2); if (is.nan(authors_per_doc)) authors_per_doc <- NA_real_

yrs <- suppressWarnings(as.integer(M$PY))
span_min <- min(yrs, na.rm=TRUE); span_max <- max(yrs, na.rm=TRUE)
span_len <- ifelse(is.finite(span_min) & is.finite(span_max), span_max - span_min + 1L, NA_integer_)
total_pubs <- nrow(M)

au_all <- if ("AU" %in% names(M)) M$AU else ""; au_all[is.na(au_all)] <- ""
toks_all <- unlist(strsplit(paste(au_all, collapse=";"), ";", fixed=TRUE))
authors_unique <- if (length(toks_all) == 0) 0L else length(unique(normalize_author_key(toks_all[nzchar(toks_all)])))

tc_vec <- pmax(0L, suppressWarnings(as.integer(if ("TC" %in% names(M)) M$TC else 0L)))
total_citations <- sum(tc_vec, na.rm=TRUE)
cited_docs <- sum(tc_vec > 0, na.rm=TRUE)
cit_per_doc <- round(mean(tc_vec, na.rm=TRUE), 2); if (is.nan(cit_per_doc)) cit_per_doc <- NA_real_
cit_per_cited_doc <- if (cited_docs > 0) round(total_citations / cited_docs, 2) else NA_real_
cit_per_year_overall <- if (!is.na(span_len) && span_len > 0) round(total_citations / span_len, 2) else NA_real_
cit_per_author <- if (authors_unique > 0) round(total_citations / authors_unique, 2) else NA_real_

hgm_one_dataset <- function(){
  tc <- sort(tc_vec, decreasing=TRUE)
  h <- sum(tc >= seq_along(tc))
  cs <- cumsum(tc); idx <- which(cs >= (seq_along(cs))^2); g <- if (length(idx)) max(idx) else 0L
  m <- ifelse(!is.na(span_len) && span_len > 0, round(h/span_len, 2), NA_real_)
  list(h=h, g=g, m=m)
}
hgm_all <- hgm_one_dataset()
h_core_h <- hgm_all$h
h_core_total_citations <- if (h_core_h > 0) sum(sort(tc_vec, decreasing = TRUE)[seq_len(h_core_h)]) else 0L

labels_tbl3 <- c(
  "Năm xuất bản","Tổng số ấn phẩm","Năm trích dẫn (độ dài giai đoạn)","Số lượng tác giả đóng góp",
  "Số lượng bài báo được trích dẫn","Tổng số trích dẫn",
  "Trích dẫn theo bài báo","Trích dẫn theo Bài báo được trích dẫn",
  "Trích dẫn mỗi năm","Trích dẫn theo tác giả","Tác giả mỗi bài báo",
  "Tổng trích dẫn trong h-Core","chỉ số h","chỉ số g","chỉ số m"
)
values_tbl3 <- c(
  sprintf("%d-%d", span_min, span_max), total_pubs, span_len, authors_unique,
  cited_docs, total_citations, cit_per_doc, cit_per_cited_doc,
  cit_per_year_overall, cit_per_author, authors_per_doc,
  h_core_total_citations, hgm_all$h, hgm_all$g, hgm_all$m
)
values_tbl3_chr <- vapply(values_tbl3, function(x) ifelse(length(x)==0 || is.na(x), NA_character_, as.character(x)), character(1))
TABLE3 <- data.frame("Thong tin chinh" = labels_tbl3, "Du lieu" = values_tbl3_chr, check.names = FALSE, stringsAsFactors = FALSE)
write.csv(TABLE3, file.path(OUTPUT_DIR, "TABLE3_citation_metrics.csv"), row.names = FALSE)

# --- Table 5 (year-level cohort metrics) -------------------------------------
count_authors_year <- function(sub){
  if (!"AU" %in% names(sub)) return(0L)
  au <- sub$AU; au[is.na(au)] <- ""
  toks <- unlist(strsplit(paste(au, collapse = ";"), ";", fixed = TRUE))
  if (length(toks) == 0) return(0L)
  toks <- normalize_author_key(toks); toks <- toks[nzchar(toks)]
  as.integer(length(unique(toks)))
}
hgm_one <- function(tc_vec){
  tc <- sort(ifelse(is.na(tc_vec),0,tc_vec), decreasing=TRUE)
  h <- sum(tc >= seq_along(tc)); cs <- cumsum(tc)
  g <- { idx <- which(cs >= (seq_along(cs))^2); if (length(idx)) max(idx) else 0L }
  list(h=h, g=g)
}
calc_year_row <- function(y, M, ref_year = REF_YEAR_FOR_M){
  sub <- M[M$PY == y, , drop = FALSE]
  tp  <- nrow(sub)
  tcv <- pmax(0L, suppressWarnings(as.integer(sub$TC)))
  tc  <- sum(tcv, na.rm = TRUE)
  ncp <- sum(tcv > 0L, na.rm = TRUE)
  nca <- count_authors_year(sub)
  c_per_p  <- if (tp  > 0) round(tc / tp,  2) else NA_real_
  c_per_cp <- if (ncp > 0) round(tc / ncp, 2) else NA_real_
  hg <- hgm_one(tcv)
  age_years <- max(1L, ref_year - as.integer(y) + 1L)
  m_year <- if (hg$h > 0) round(hg$h / age_years, 2) else 0
  data.frame(Nam = y, TP = tp, NCP = ncp, NCA = nca, TC = tc,
             `C/P` = c_per_p, `C/CP` = c_per_cp, h = hg$h, g = hg$g, m = m_year,
             check.names = FALSE)
}
years  <- sort(unique(yrs[is.finite(yrs)]))
TABLE5 <- do.call(rbind, lapply(years, calc_year_row, M = M, ref_year = REF_YEAR_FOR_M))
TABLE5 <- TABLE5[order(TABLE5$Nam), c("Nam","TP","NCP","NCA","TC","C/P","C/CP","h","g","m")]
write.csv(TABLE5, file.path(OUTPUT_DIR, "TABLE5_by_year_enhanced.csv"), row.names = FALSE)

# --- Table 6 (Top-K ranking) --------------------------------------------------
COVERAGE_TAU   <- 0.55
K_TARGET       <- 40L
RANK_TIE_DOI_1 <- TRUE

M$PY <- suppressWarnings(as.integer(M$PY))
M$TC <- pmax(0L, suppressWarnings(as.integer(M$TC)))
age_years_vec <- pmax(1L, CPY_REF_YEAR - M$PY + 1L)
CPY_vec <- round(M$TC / age_years_vec, 4)

DI_norm <- normalize_doi(M$DI); has_doi <- is_valid_doi(DI_norm)
TI_chr  <- if ("TI"  %in% names(M)) safe_chr(M$TI)  else ""
SO_chr  <- if ("SO"  %in% names(M)) safe_chr(M$SO)  else ""
AU_chr  <- if ("AU"  %in% names(M)) safe_chr(M$AU)  else ""
EID_chr <- if ("EID" %in% names(M)) safe_chr(M$EID) else ""
first_authors <- vapply(AU_chr, get_first_author, character(1))
authors_display <- ifelse(nzchar(first_authors), paste0(first_authors, " et al."), "")

RANKTAB <- data.frame(
  Authors_display = authors_display, PY = M$PY, Title = TI_chr, Journal = SO_chr,
  DOI = ifelse(has_doi, DI_norm, ""), EID = EID_chr, TC = M$TC, CPY = CPY_vec,
  has_doi = has_doi,
  stringsAsFactors = FALSE, check.names = FALSE
)
ord <- order(-RANKTAB$TC, -RANKTAB$CPY, -RANKTAB$PY,
             if (RANK_TIE_DOI_1) -as.integer(RANKTAB$has_doi) else 0L,
             tolower(RANKTAB$Title), na.last = TRUE)
RANKTAB <- RANKTAB[ord, , drop = FALSE]
RANKTAB$Rank <- seq_len(nrow(RANKTAB))
total_TC_all <- sum(RANKTAB$TC, na.rm = TRUE)
RANKTAB$cum_TC  <- cumsum(ifelse(is.na(RANKTAB$TC), 0L, RANKTAB$TC))
RANKTAB$cum_cov <- ifelse(total_TC_all > 0, RANKTAB$cum_TC / total_TC_all, 0)
K_tau <- which(RANKTAB$cum_cov >= COVERAGE_TAU)[1]; if (is.na(K_tau)) K_tau <- min(nrow(RANKTAB), K_TARGET)

TOPK <- utils::head(RANKTAB, K_TARGET)
TABLE6 <- TOPK[, c("Rank","Authors_display","PY","Title","Journal","DOI","EID","TC","CPY")]
names(TABLE6) <- c("Rank","Authors (First author et al.)","Year","Title","Source title (Journal)",
                   "DOI","EID","Global Citations (Scopus)","Citations/Year (CPY)")
out_tbl6_csv  <- file.path(OUTPUT_DIR, paste0("TABLE6_top", K_TARGET, "_", TS, ".csv"))
write.csv(TABLE6, out_tbl6_csv, row.names = FALSE, na = "")

TABLE6_COVERAGE_PATH <- file.path(OUTPUT_DIR, paste0("TABLE6_coverage_", TS, ".json"))
jsonlite::write_json(list(
  total_documents = nrow(RANKTAB),
  total_citations = unname(total_TC_all),
  coverage_tau    = COVERAGE_TAU,
  K_tau           = unname(K_tau),
  coverage_at_K_tau = if (is.finite(K_tau)) unname(RANKTAB$cum_cov[K_tau]) else NA_real_,
  K_selected      = K_TARGET,
  coverage_at_K_selected = unname(RANKTAB$cum_cov[K_TARGET]),
  CPY_ref_year    = unname(CPY_REF_YEAR),
  ranking_rule    = "TC desc → CPY desc → Year desc → DOI present → Title A–Z"
), TABLE6_COVERAGE_PATH, pretty = TRUE, auto_unbox = TRUE)

# --- README (formulas & CPY anchor = 2025) -----------------------------------
readme <- c(
  "# README_TABLES",
  "This package reproduces Tables 3, 5, and 6 and the annual trends series from a pre-cleaned Scopus corpus.",
  "",
  "## Software & parameters",
  "- R packages: bibliometrix, readr, jsonlite, dplyr, tidyr.",
  paste0("- Analysis window: year_min = ", year_min, ", year_max = ", year_max, "."),
  "- **Reference year for CPY and m-index: CPY_REF_YEAR = 2025**.",
  "- Ranking rule (Table 6): TC ↓ → CPY ↓ → Year ↓ → DOI present → Title A–Z.",
  "",
  "## Table 3 – Dataset-level metrics",
  "- Timespan = min(PY)–max(PY); length = max(PY) - min(PY) + 1.",
  "- Cited documents = count(TC > 0).",
  "- Citations per document = mean(TC).",
  "- Citations per cited document = sum(TC) / count(TC > 0).",
  "- Citations per year = sum(TC) / length.",
  "- Citations per author = sum(TC) / unique contributing authors.",
  "- Authors per document = mean(authors in AU).",
  "- h, g on corpus TC; m = h / length.",
  "- h-core total citations = sum of TC among top-h documents.",
  "",
  "## Table 5 – Year-level cohort metrics",
  "- For each year y: TP, NCP (TC>0), NCA (unique authors), TC, C/P = TC/TP, C/CP = TC/NCP.",
  "- m(y) = h(y) / age_years with age_years = max(1, CPY_REF_YEAR - y + 1).",
  "",
  "## Table 6 – Top-K most cited (CPY displayed)",
  "- Default K = 40; coverage τ is reported at K and Kτ in JSON.",
  "",
  "All outputs are generated under `tables/`. The input CSV is assumed pre-cleaned and is not altered by this script."
)
writeLines(readme, file.path(OUTPUT_DIR, "README_TABLES.md"))

# --- Manifest & session info --------------------------------------------------
manifest <- list(
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  seed = SEED_STABLE,
  tables = list(
    table3_csv = "TABLE3_citation_metrics.csv",
    table5_csv = "TABLE5_by_year_enhanced.csv",
    table6_csv = basename(out_tbl6_csv),
    table6_coverage = basename(TABLE6_COVERAGE_PATH),
    annual_series = paste0("annual_publications_and_citations_", TS, ".csv")
  ),
  params = list(
    year_min = year_min, year_max = year_max,
    CPY_REF_YEAR = CPY_REF_YEAR, REF_YEAR_FOR_M = REF_YEAR_FOR_M,
    COVERAGE_TAU = COVERAGE_TAU, K_TARGET = K_TARGET,
    ranking_rule = "TC desc → CPY desc → Year desc → DOI present → Title A–Z",
    ncp_def = "Number of Cited Publications (TC>0) per year",
    nca_def = "Number of Contributing Authors (unique) per year"
  )
)
jsonlite::write_json(manifest, file.path(OUTPUT_DIR, "MANIFEST_TABLES3_5_6.json"), pretty = TRUE, auto_unbox = TRUE)

writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, paste0("session_info_", TS, ".txt")))
vmsg("Done. Outputs in: ", normalizePath(OUTPUT_DIR, winslash="/"))
