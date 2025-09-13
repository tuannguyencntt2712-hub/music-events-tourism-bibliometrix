############################################################
# 01_descriptive_tables.R
#: Reproduces dataset-level (Table 1) and year-cohort metrics (Table 3),
#: and constructs a Top-K ranking with local citations (Table 4).
#: The corpus is assumed pre-cleaned (manual screening already applied).
#: CPY and the m-index are fixed at CPY_REF_YEAR = 2025 for temporal comparability.
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
ensure_pkg("stringdist")
ensure_pkg("data.table")
have_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)

SEED_STABLE <- 20250817L
set.seed(SEED_STABLE)

# --- User parameters (repo-root relative) ---
PROJECT_ROOT   <- normalizePath(getwd(), winslash = "/")
csv_path       <- file.path(PROJECT_ROOT, "data_clean", "scopus_clean.csv")
year_min       <- 2000L
year_max       <- 2024L
CPY_REF_YEAR   <- 2025L
REF_YEAR_FOR_M <- CPY_REF_YEAR

# --- Output folders under the same base --------------------------------------
OUTPUT_DIR <- file.path(PROJECT_ROOT, "tables")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
TS <- format(Sys.time(), "%Y%m%d-%H%M%S")

# sanity check
if (!file.exists(csv_path)) stop(sprintf("CSV not found at: %s", csv_path))
vmsg("Using CSV: ", normalizePath(csv_path, winslash="/"))


# --- Unicode helpers & DOI utilities (for LC matching; no data cleaning) -----
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

# --- Import (assumes pre-cleaned Scopus CSV) ----------------------------------
#: Ingest the cleaned CSV via bibliometrix; if it fails, stop with an informative error.
if (!file.exists(csv_path)) stop(sprintf("CSV not found: %s", csv_path))
vmsg("Using CSV: ", normalizePath(csv_path, winslash="/"))
M <- bibliometrix::convert2df(file = csv_path, dbsource = "scopus", format = "csv")

# --- Year filter (no dedup; dataset is assumed clean) -------------------------
#: Restrict analysis to [year_min, year_max]; rows with missing PY are dropped.
if (!"PY" %in% names(M)) stop("Column PY not found in the input data.")
M$PY <- suppressWarnings(as.integer(M$PY))
M <- M[!is.na(M$PY) & M$PY >= year_min & M$PY <= year_max, , drop = FALSE]
if (nrow(M) == 0) stop(sprintf("Empty corpus after year filter %d–%d.", year_min, year_max))

# --- Standardization ----------------------------------------------------------
#: Ensure TC is non-negative integer; ensure auxiliary fields exist.
if (!"TC" %in% names(M)) M$TC <- 0L
M$TC <- pmax(0L, suppressWarnings(as.integer(M$TC)))
if (!"C1" %in% names(M)) M$C1 <- ""

# --- Annual publications & citations (for trends figure) ----------------------
#: Aggregate publications and citations per year; export a machine-readable series.
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

# --- Table 1 (dataset-level indicators) --------------------------------------
#: Compute timespan, totals, per-doc/per-cited ratios, h/g/m at corpus level,
#: authorship density, and h-core total citations.
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

# human-readable 2-column (kept stable with prior pipeline)
labels_tbl1 <- c(
  "Năm xuất bản","Tổng số ấn phẩm","Năm trích dẫn (độ dài giai đoạn)","Số lượng tác giả đóng góp",
  "Số lượng bài báo được trích dẫn","Tổng số trích dẫn",
  "Trích dẫn theo bài báo","Trích dẫn theo Bài báo được trích dẫn",
  "Trích dẫn mỗi năm","Trích dẫn theo tác giả","Tác giả mỗi bài báo",
  "Tổng trích dẫn trong h-Core","chỉ số h","chỉ số g","chỉ số m"
)
values_tbl1 <- c(
  sprintf("%d-%d", span_min, span_max), total_pubs, span_len, authors_unique,
  cited_docs, total_citations, cit_per_doc, cit_per_cited_doc,
  cit_per_year_overall, cit_per_author, authors_per_doc,
  h_core_total_citations, hgm_all$h, hgm_all$g, hgm_all$m
)
values_tbl1_chr <- vapply(values_tbl1, function(x) ifelse(length(x)==0 || is.na(x), NA_character_, as.character(x)), character(1))
TABLE1 <- data.frame("Thong tin chinh" = labels_tbl1, "Du lieu" = values_tbl1_chr, check.names = FALSE, stringsAsFactors = FALSE)
write.csv(TABLE1, file.path(OUTPUT_DIR, "TABLE1_citation_metrics.csv"), row.names = FALSE)

# --- Table 3 (year-level cohort metrics) -------------------------------------
#: For each publication year y: TP, NCP (TC>0), NCA (unique authors), TC, C/P, C/CP,
#: and h/g/m computed on the cohort’s TC distribution; m = h / age_years, with
#: age_years = max(1, REF_YEAR_FOR_M - y + 1).
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
TABLE3 <- do.call(rbind, lapply(years, calc_year_row, M = M, ref_year = REF_YEAR_FOR_M))
TABLE3 <- TABLE3[order(TABLE3$Nam), c("Nam","TP","NCP","NCA","TC","C/P","C/CP","h","g","m")]
write.csv(TABLE3, file.path(OUTPUT_DIR, "BANG3_by_year_enhanced.csv"), row.names = FALSE)

# --- Local citations (LC) & Table 4 ranking ----------------------------------
#: Compute LC via staged matching (DOI → EID → fuzzy Title+Year±1+FirstSurname);
#: then rank by TC ↓, CPY ↓, Year ↓, DOI presence; export Top-K.
COVERAGE_TAU   <- 0.55
K_TARGET       <- 40L
RANK_TIE_DOI_1 <- TRUE

safe_chr <- function(x) { x <- ifelse(is.na(x), "", x); as.character(x) }
get_first_author <- function(au) {
  au <- safe_chr(au)
  first <- trimws(strsplit(au, ";", fixed = TRUE)[[1]][1])
  if (is.na(first) || !nzchar(first)) return("")
  if (grepl(",", first, fixed = TRUE)) trimws(strsplit(first, ",", fixed = TRUE)[[1]][1]) else strsplit(first, " +")[[1]][1]
}

# keys for matching
corpus <- M
corpus$DOI_norm <- normalize_doi(corpus$DI)
corpus$has_doi  <- is_valid_doi(corpus$DOI_norm)
corpus$EID_chr  <- if ("EID" %in% names(corpus)) safe_chr(corpus$EID) else ""
corpus$PY       <- suppressWarnings(as.integer(corpus$PY))
corpus$Title_norm <- tolower(gsub("\\s+"," ", to_ascii(trimws(ifelse(is.na(corpus$TI),"",corpus$TI)))))
corpus$FirstSurname <- vapply(ifelse(is.na(corpus$AU),"",corpus$AU), function(au){
  au <- trimws(strsplit(au, ";", fixed=TRUE)[[1]][1]); if (!nzchar(au)) return("")
  if (grepl(",", au, fixed=TRUE)) trimws(strsplit(au, ",", fixed=TRUE)[[1]][1]) else strsplit(au," +")[[1]][1]
}, character(1))
corpus_key <- data.frame(
  doc_id = seq_len(nrow(corpus)),
  DOI_norm = corpus$DOI_norm,
  EID_chr  = corpus$EID_chr,
  Title_norm = corpus$Title_norm,
  PY = corpus$PY,
  FirstSurname = toupper(corpus$FirstSurname),
  stringsAsFactors = FALSE
)
doi_index <- which(is_valid_doi(corpus_key$DOI_norm)); map_doi <- setNames(doi_index, corpus_key$DOI_norm[doi_index])
eid_index <- which(nzchar(corpus_key$EID_chr));      map_eid <- setNames(eid_index, corpus_key$EID_chr[eid_index])

# long-form references
CR_long <- data.table::data.table(src_id = seq_len(nrow(M)), CR = safe_chr(M$CR))
CR_long <- CR_long[nchar(CR) > 0]
CR_long <- CR_long[, .(ref = trimws(unlist(strsplit(CR, ";\\s*|\\n+|\\r+")))), by=.(src_id)]
CR_long <- CR_long[nchar(ref) > 0]

# extract fields
safe_extract_doi <- function(s){
  s <- to_ascii(s); if (!nzchar(s)) return("")
  s <- gsub("^(?i)https?://(dx\\.)?doi\\.org/","", s, perl=TRUE)
  m <- regexpr("10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+", s, perl=TRUE)
  if (m[1] == -1) return(""); tolower(regmatches(s, m)[1])
}
norm_title_from_ref <- function(s){
  s <- to_ascii(tolower(s))
  s <- sub("^\\s*\\[?\\d+\\]?\\s*", "", s)
  s <- sub("10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+.*$", "", s)
  s <- sub("2-s2\\.0-\\d+.*$", "", s)
  s <- gsub("[^a-z0-9 ]", " ", s); s <- gsub("\\s+", " ", s); trimws(s)
}
safe_extract_year <- function(s){
  s <- to_ascii(s); m <- regexpr("\\b(19|20)\\d{2}\\b", s, perl=TRUE)
  if (m[1] == -1) return(NA_integer_); suppressWarnings(as.integer(regmatches(s, m)[1]))
}
safe_extract_firstsurname <- function(s){
  s2 <- toupper(to_ascii(s))
  m <- regexpr("^\\s*([A-ZÀ-Ỵ'\\-]+)[^,]*,", s2, perl=TRUE)
  if (m[1] != -1) return(sub(",.*$","", sub("^\\s*","", regmatches(s2, m)[1])))
  tokens <- strsplit(s2, " +", fixed = FALSE)[[1]]; if (length(tokens)) tokens[1] else ""
}

CR_long[, DOI_ref := normalize_doi(vapply(ref, safe_extract_doi, FUN.VALUE = character(1)))]
CR_long[, has_doi := is_valid_doi(DOI_ref)]
CR_long[, EID_ref := { m <- regexpr("2-s2\\.0-\\d+", ref, perl=TRUE); ifelse(m == -1, "", regmatches(ref, m)) }]
CR_long[, Title_ref := vapply(ref, norm_title_from_ref, FUN.VALUE = character(1))]
CR_long[, Year_ref  := vapply(ref, safe_extract_year,  FUN.VALUE = integer(1))]
CR_long[, First_ref := vapply(ref, safe_extract_firstsurname, FUN.VALUE = character(1))]
CR_long[, match_doc := NA_integer_]

# staged matching: DOI → EID → fuzzy
idx_doi <- which(CR_long$has_doi & CR_long$DOI_ref %in% names(map_doi))
CR_long$match_doc[idx_doi] <- unname(map_doi[ CR_long$DOI_ref[idx_doi] ])

idx_eid <- which(is.na(CR_long$match_doc) & nzchar(CR_long$EID_ref) & CR_long$EID_ref %in% names(map_eid))
CR_long$match_doc[idx_eid] <- unname(map_eid[ CR_long$EID_ref[idx_eid] ])

need_fuzzy <- which(is.na(CR_long$match_doc))
if (length(need_fuzzy)) {
  cand <- corpus_key[, c("doc_id","Title_norm","PY","FirstSurname")]
  tmp <- as.data.frame(CR_long[need_fuzzy, .(row_id = .I, First_ref, Year_ref, Title_ref)])
  tmp$First_ref <- toupper(tmp$First_ref)
  cand_map <- dplyr::left_join(tmp, cand, by = c("First_ref" = "FirstSurname"))
  cand_map <- cand_map[!is.na(cand_map$doc_id) & !is.na(cand_map$PY), ]
  cand_map$year_ok <- is.na(tmp$Year_ref[cand_map$row_id]) | abs(cand_map$PY - tmp$Year_ref[cand_map$row_id]) <= 1
  cand_map <- cand_map[cand_map$year_ok, ]
  if (nrow(cand_map)) {
    sim <- stringdist::stringsim(cand_map$Title_norm, tmp$Title_ref[cand_map$row_id], method = "jw", p = 0.1)
    cand_map$sim <- sim; thr <- 0.92
    best <- cand_map %>% dplyr::group_by(row_id) %>% dplyr::filter(sim == max(sim, na.rm = TRUE)) %>%
      dplyr::slice_head(n = 1) %>% dplyr::ungroup()
    accept <- best$sim >= thr
    rows_accept <- need_fuzzy[ best$row_id[accept] ]
    docs_accept <- best$doc_id[accept]
    CR_long$match_doc[rows_accept] <- docs_accept
    near <- best %>% dplyr::filter(sim >= 0.85 & sim < thr)
    if (nrow(near)) {
      audit_near <- data.frame(
        src_id = CR_long$src_id[ need_fuzzy[near$row_id] ],
        ref    = CR_long$ref[    need_fuzzy[near$row_id] ],
        First_ref = near$First_ref, Year_ref = near$Year_ref,
        Title_ref = tmp$Title_ref[near$row_id],
        cand_doc_id = near$doc_id, cand_PY = near$PY,
        cand_Title  = corpus_key$Title_norm[near$doc_id],
        sim = round(near$sim, 4), stringsAsFactors = FALSE
      )
      write.csv(audit_near, file.path(OUTPUT_DIR, paste0("LC_fuzzy_candidates_", TS, ".csv")), row.names = FALSE)
    }
  }
}

# LC counts and coverage report
valid_edge <- CR_long[!is.na(match_doc) & CR_long$src_id != match_doc]
if (nrow(valid_edge) == 0) {
  LC_count <- data.frame(doc_id = integer(0), LC = integer(0))
} else {
  LC_count <- valid_edge[, .(LC = .N), by = .(doc_id = match_doc)]
}

LC_vec <- integer(nrow(corpus))
if (nrow(LC_count) > 0) {
  ok <- LC_count$doc_id >= 1 & LC_count$doc_id <= length(LC_vec)
  if (any(ok)) LC_vec[LC_count$doc_id[ok]] <- LC_count$LC[ok]
}

tot_refs <- nrow(CR_long)
matched_doi <- sum(!is.na(CR_long$match_doc) & CR_long$has_doi)
matched_eid <- sum(!is.na(CR_long$match_doc) & !CR_long$has_doi & nzchar(CR_long$EID_ref))
matched_fuz <- sum(!is.na(CR_long$match_doc) & !CR_long$has_doi & !nzchar(CR_long$EID_ref))
unmatched <- CR_long[is.na(match_doc)]
LC_COVERAGE_PATH <- file.path(OUTPUT_DIR, paste0("LC_coverage_", TS, ".json"))
jsonlite::write_json(list(
  total_references_rows = tot_refs,
  matched_via = list(doi = matched_doi, eid = matched_eid, fuzzy = matched_fuz),
  unmatched_rows = nrow(unmatched),
  match_rate = round((tot_refs - nrow(unmatched)) / max(1, tot_refs), 4),
  fuzzy_threshold = 0.92,
  notes = "LC via DOI→EID→Fuzzy (Title+Year±1+FirstAuthor)."
), LC_COVERAGE_PATH, pretty = TRUE, auto_unbox = TRUE)

# Ranking & CPY (anchored at 2025)
M$PY <- suppressWarnings(as.integer(M$PY))
M$TC <- pmax(0L, suppressWarnings(as.integer(M$TC)))
age_years_vec <- pmax(1L, CPY_REF_YEAR - M$PY + 1L)
CPY_vec <- round(M$TC / age_years_vec, 4)

safe_chr <- function(x) { x <- ifelse(is.na(x), "", x); as.character(x) }
get_first_author <- function(au) {
  au <- safe_chr(au)
  first <- trimws(strsplit(au, ";", fixed = TRUE)[[1]][1])
  if (is.na(first) || !nzchar(first)) return("")
  if (grepl(",", first, fixed = TRUE)) trimws(strsplit(first, ",", fixed = TRUE)[[1]][1]) else strsplit(first, " +")[[1]][1]
}
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
  LC = LC_vec, has_doi = has_doi,
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

TABLE4 <- TOPK[, c("Rank",
                   "Authors_display","PY","Title","Journal",
                   "DOI","EID","TC","CPY")]

names(TABLE4) <- c("Rank",
                   "Authors (First author et al.)","Year","Title","Source title (Journal)",
                   "DOI","EID","Global Citations (Scopus)","Citations/Year (CPY)")

out_tbl4_csv  <- file.path(OUTPUT_DIR, paste0("BANG4_top", K_TARGET, "_", TS, ".csv"))
write.csv(TABLE4, out_tbl4_csv, row.names = FALSE, na = "")


BANG4_COVERAGE_PATH <- file.path(OUTPUT_DIR, paste0("BANG4_coverage_", TS, ".json"))
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
), BANG4_COVERAGE_PATH, pretty = TRUE, auto_unbox = TRUE)

# --- README (formulas & CPY anchor explicitly set to 2025) -------------------
readme <- c(
  "# README_TABLES",
  "This package reproduces Tables 1, 3, and 4 and the annual trends series from a pre-cleaned Scopus corpus.",
  "",
  "## Software & parameters",
  "- R packages: bibliometrix, readr, jsonlite, dplyr, tidyr, stringdist, data.table (optional: openxlsx).",
  paste0("- Analysis window: year_min = ", year_min, ", year_max = ", year_max, "."),
  "- **Reference year for CPY and m-index: CPY_REF_YEAR = 2025**.",
  "- Ranking rule (Table 4): TC ↓ → CPY ↓ → Year ↓ → DOI present → Title A–Z.",
  "- Local citations are computed (DOI → EID → fuzzy, Jaro–Winkler ≥ 0.92) for diagnostics/coverage only and are NOT displayed in Table 4.",
  "",
  "## Table 1 – Dataset-level metrics",
  "- Timespan = min(PY)–max(PY); length = max(PY) - min(PY) + 1.",
  "- Cited documents = count(TC > 0).",
  "- Citations per document = mean(TC).",
  "- Citations per cited document = sum(TC) / count(TC > 0).",
  "- Citations per year = sum(TC) / length.",
  "- Citations per author = sum(TC) / unique contributing authors.",
  "- Authors per document = mean(authors in AU).",
  "- h, g computed on corpus TC; m = h / length.",
  "- h-core total citations = sum of TC among top-h documents.",
  "",
  "## Table 3 – Year-level cohort metrics",
  "- For each year y: TP, NCP (TC>0), NCA (unique authors), TC, C/P = TC/TP, C/CP = TC/NCP.",
  "- m(y) = h(y) / age_years with age_years = max(1, CPY_REF_YEAR - y + 1).",
  "",
  "## Table 4 – Top-K most cited (CPY displayed, LC suppressed)",
  "- Default K = 40; coverage τ is reported at K and at Kτ in JSON.",
  "",
  "All outputs are generated under `tables/`. The input file is assumed pre-cleaned and is not altered by this script."
)
writeLines(readme, file.path(OUTPUT_DIR, "README_TABLES.md"))


# --- Manifest & session info --------------------------------------------------
manifest <- list(
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  seed = SEED_STABLE,
  tables = list(
    table1_csv = "TABLE1_citation_metrics.csv",
    table1_numeric = "TABLE1_citation_metrics_numeric.csv",
    table3_csv = "BANG3_by_year_enhanced.csv",
    table4_csv = basename(out_tbl4_csv),
    lc_coverage = basename(LC_COVERAGE_PATH),
    bang4_coverage = basename(BANG4_COVERAGE_PATH),
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
jsonlite::write_json(manifest, file.path(OUTPUT_DIR, "MANIFEST_TABLES1_3_pubready.json"), pretty = TRUE, auto_unbox = TRUE)

writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, paste0("session_info_", TS, ".txt")))
vmsg("Done. Outputs in: ", normalizePath(OUTPUT_DIR, winslash="/"))


