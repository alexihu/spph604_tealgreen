# ------------------------------------------------------------------------------
# Purpose: Build a clean, analysis-ready NHANES dataset for polypharmacy & mortality
#          analyses (2003–2018 pooled).
#
# Output:
#   - analytic_raw.rds : R serialized dataframe for analysis
#
# Notes:
#   - Education (DMDEDUC2) is a **string**; we standardize case/spaces and map to
#     a 4-level factor. "Refused"/"Don't know" -> NA.
#   - Comorbidity count includes MCQ + DIQ010/DIQ080 + BPQ020 + KIQ_U:KIQ022.
#   - A simple Charlson-like index (cci_score) is constructed using NHANES items.
#   - Health utilization (visits & hospitalizations) harmonized across cycles:
#       * Visits: HUQ050 (0,1,2–3,4–9,10–12,≥13) or HUQ051 collapsed into same scale.
#       * Overnight stays: HUQ071 (Yes/No) → HUD080 (1..6, 6=6+). No → 0 stays.
# ------------------------------------------------------------------------------

# ---- Load packages ----------------------------------------------------------------
library(tidyverse)
library(nhanesA)
library(janitor)
library(haven)
library(readr)
library(stringr)

# ---- NHANES cycles used -------------------------------------------------------
cycle_meta <- tibble::tribble(
  ~cycle,        ~suffix,
  "2003-2004",   "_C",
  "2005-2006",   "_D",
  "2007-2008",   "_E",
  "2009-2010",   "_F",
  "2011-2012",   "_G",
  "2013-2014",   "_H",
  "2015-2016",   "_I",
  "2017-2018",   "_J"
)

# ---- Helpers: general ---------------------------------------------------------

# Clean numeric from char-like NHANES fields (turn "", ".", "..", "..." to NA)
to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", ".", "..", "...")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

# Sex (RIAGENDR → "Male"/"Female")
recode_sex <- function(x) {
  xn <- suppressWarnings(as.numeric(x))
  out <- dplyr::case_when(xn == 1 ~ "Male", xn == 2 ~ "Female", TRUE ~ NA_character_)
  factor(out, levels = c("Male","Female"))
}

# Race/ethnicity from RIDRETH1 **only** (string or factor), standardized
# RIDRETH1 has 5 labeled groups in public releases:
#   "Mexican American", "Other Hispanic",
#   "Non-Hispanic White", "Non-Hispanic Black",
#   "Other Race - Including Multi-Racial"
# We standardize "Other Race - Including Multi-Racial" to "Other/Multi".
derive_race_eth_r1 <- function(x) {
  s <- as.character(x)
  s <- trimws(s)
  sl <- stringr::str_to_lower(s)
  out <- dplyr::case_when(
    stringr::str_detect(sl, "^non-hispanic\\s+white") ~ "Non-Hispanic White",
    stringr::str_detect(sl, "^non-hispanic\\s+black") ~ "Non-Hispanic Black",
    stringr::str_detect(sl, "^mexican\\s+american")   ~ "Mexican American",
    stringr::str_detect(sl, "^other\\s+hispanic")     ~ "Other Hispanic",
    stringr::str_detect(sl, "other\\s+race") | stringr::str_detect(sl, "multi") ~ "Other/Multi",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Non-Hispanic White","Non-Hispanic Black",
                         "Mexican American","Other Hispanic","Other/Multi"))
}

# Education (DMDEDUC2 is a **string** with inconsistent case/spaces)
# Collapse to 4 levels: "Less than HS", "HS/GED", "Some college", "College+"
# Map:
#  - "Less than 9th grade", "9-11th grade (Includes 12th grade with no diploma)" -> Less than HS
#  - "High school graduate/GED or equivalent" -> HS/GED
#  - "Some college or AA degree" -> Some college
#  - "College graduate or above" -> College+
#  - "Refused", "Don't know" -> NA
derive_ed_cat <- function(x) {
  s <- as.character(x)
  s <- trimws(s)
  sl <- stringr::str_to_lower(s)
  sl <- stringr::str_replace_all(sl, "\\s+", " ") # normalize spaces
  out <- dplyr::case_when(
    # Missing-like
    is.na(sl) ~ NA_character_,
    stringr::str_detect(sl, "refused") | stringr::str_detect(sl, "don'?t\\s+know") ~ NA_character_,
    # Mappings
    stringr::str_detect(sl, "^college\\s+graduate") ~ "College+",
    stringr::str_detect(sl, "^some\\s+college")     ~ "Some college",
    stringr::str_detect(sl, "^high\\s+school") | stringr::str_detect(sl, "ged") ~ "HS/GED",
    stringr::str_detect(sl, "^less\\s+than\\s+9") |
      stringr::str_detect(sl, "^9-11th") | stringr::str_detect(sl, "^9–11th") |
      stringr::str_detect(sl, "no\\s+diploma") ~ "Less than HS",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Less than HS","HS/GED","Some college","College+"))
}

# Insurance (HIQ011): robust Yes/No recode (returns 1/0/NA)
recode_hiq011_yesno <- function(x) {
  xs <- trimws(as.character(x))
  xs[xs %in% c("", ".", "..", "...")] <- NA_character_
  lead_num <- suppressWarnings(as.numeric(stringr::str_extract(xs, "^[0-9]+")))
  out_num <- dplyr::case_when(
    !is.na(lead_num) & lead_num == 1 ~ 1L,
    !is.na(lead_num) & lead_num == 2 ~ 0L,
    TRUE ~ NA_integer_
  )
  need_fallback <- is.na(out_num) & !is.na(xs)
  xs_up <- toupper(xs)
  out_fallback <- dplyr::case_when(
    stringr::str_detect(xs_up, "\\bYES\\b|\\bCOVERED\\b")       ~ 1L,
    stringr::str_detect(xs_up, "\\bNO\\b|\\bNOT\\s+COVERED\\b") ~ 0L,
    TRUE ~ NA_integer_
  )
  ifelse(need_fallback, out_fallback, out_num)
}

# MCQ/yes-no items: 1 = Yes → 1; everything else (No/Refused/DK/blank) → 0
yes1_else0 <- function(x) {
  xs <- trimws(as.character(x))
  xs[xs %in% c("", ".", "..", "...")] <- NA_character_
  xn <- suppressWarnings(as.numeric(xs))
  out <- ifelse(!is.na(xn), ifelse(xn == 1, 1L, 0L),
                ifelse(grepl("\\bYES\\b", toupper(xs)), 1L, 0L))
  out[is.na(out)] <- 0L
  as.integer(out)
}

# nhanesA getter → clean_names + zap_labels
nhanes_get <- function(table_base, suffix, quiet = TRUE) {
  nm <- paste0(table_base, suffix)
  out <- tryCatch(suppressWarnings(nhanesA::nhanes(nm)), error = function(e) NULL)
  if (is.null(out)) return(NULL)
  out %>% janitor::clean_names() %>% haven::zap_labels()
}

# Safe pull: always returns at least SEQN (integer)
safe_get <- function(base, suf) {
  df <- nhanes_get(base, suf, quiet = TRUE)
  if (is.null(df)) return(tibble(seqn = integer()))
  df <- janitor::clean_names(df)
  if (!"seqn" %in% names(df)) df$seqn <- NA
  dplyr::mutate(df, seqn = as.integer(seqn))
}

# ---- DEMO: demographics, weights, design vars --------------------------------
demo_all <- purrr::map_dfr(cycle_meta$suffix, ~{
  df <- nhanes_get("DEMO", .x, quiet = TRUE); if (is.null(df)) return(tibble())
  df %>%
    dplyr::select(dplyr::any_of(c(
      "seqn","sddsrvyr","ridageyr","riagendr","ridreth1",
      "dmdeduc2","indfmpir","wtint2yr","sdmvpsu","sdmvstra"
    )))
})
stopifnot(nrow(demo_all) > 0)

# Roster (one row per participant)
demo_roster <- demo_all %>%
  transmute(seqn = as.integer(seqn)) %>%
  distinct()

# ---- RX: polypharmacy counts (prefer official RXDCOUNT; fallback RXQ rows) ----
rx_tables <- tryCatch(
  nhanesA::nhanesSearchVarName(varname = "RXDCOUNT", ystart = 2003, ystop = 2018, namesonly = TRUE),
  error = function(e) character(0)
)
stopifnot(length(rx_tables) > 0)

# Official RXDCOUNT
rx_official <- purrr::map_dfr(rx_tables, function(tbl) {
  df <- tryCatch(suppressWarnings(nhanesA::nhanes(tbl)), error = function(e) NULL)
  if (is.null(df)) return(tibble())
  df %>%
    janitor::clean_names() %>%
    transmute(seqn = as.integer(seqn),
              rxdcount = suppressWarnings(as.numeric(rxdcount))) %>%
    group_by(seqn) %>%
    summarise(rxdcount = dplyr::coalesce(dplyr::first(na.omit(rxdcount)), NA_real_), .groups = "drop")
})

# Fallback count from RXQ_RX*
rx_alt <- purrr::map_dfr(cycle_meta$suffix, function(suf) {
  tbl <- paste0("RXQ_RX", suf)
  df <- tryCatch(suppressWarnings(nhanesA::nhanes(tbl)), error = function(e) NULL)
  if (is.null(df)) return(tibble())
  df <- janitor::clean_names(df)
  df %>%
    mutate(
      seqn         = as.integer(seqn),
      rxduse_n     = suppressWarnings(as.numeric(rxduse)),
      rxddrgid_n   = suppressWarnings(as.numeric(rxddrgid)),
      has_drugcode = !is.na(rxddrgid_n),
      has_drugname = !is.na(rxddrug) & nzchar(trimws(rxddrug)),
      med_row      = (rxduse_n == 1) & (has_drugcode | has_drugname)
    ) %>%
    group_by(seqn) %>%
    summarise(rxdcount_alt = sum(med_row, na.rm = TRUE), .groups = "drop")
})

# Prefer official; else fallback; else 0
rx_all <- demo_roster %>%
  left_join(rx_official, by = "seqn") %>%
  left_join(rx_alt,      by = "seqn") %>%
  mutate(rxdcount = coalesce(as.numeric(rxdcount), as.numeric(rxdcount_alt), 0)) %>%
  select(seqn, rxdcount)

# ---- Comorbidities: MCQ + DIQ010/DIQ080 + BPQ020 + KIQ022 --------------------
comorb_vars <- c(
  # MCQ (ever told …)
  "mcq010","mcq080","mcq160n","mcq160b","mcq160c","mcq160d",
  "mcq160e","mcq160f","mcq160m","mcq160g","mcq160k","mcq160l","mcq220",
  # Other modules
  "diq010",  # diabetes
  "bpq020",  # hypertension
  "kiq022",  # kidney disease
  # Some additions
  "diq080",  # diabetes retinopathy
  "mcq190", "mcq191", "mcq195"  # RA variants across cycles
)

comorb_vars_reduced <- c(
  "mcq010","mcq080","mcq160n","mcq160b","mcq160c","mcq160d",
  "mcq160e","mcq160f","mcq160m","mcq160g","mcq160k","mcq160l","mcq220",
  "bpq020","kiq022","diq_noret","diq_ret","mcq_ra"
)

## Weights for a simple Charlson-like score (proxy)
comorb_weights <- c(
  mcq010 = 0,  mcq080 = 0,  mcq160n = 0,
  mcq160b = 1, mcq160c = 0, mcq160d = 0,
  mcq160e = 1, mcq160f = 1, mcq160m = 0,
  mcq160g = 1, mcq160k = 1, mcq160l = 2,
  mcq220 = 2,
  diq_noret = 1,  # diabetes w/o retinopathy
  diq_ret   = 2,  # diabetes with retinopathy
  bpq020 = 0,
  kiq022 = 2,
  mcq_ra = 1
)

# Pull each module across cycles
mcq_wide <- purrr::map_dfr(cycle_meta$suffix, ~{
  df <- safe_get("MCQ", .x)
  keep <- intersect(names(df), c("seqn", comorb_vars))
  dplyr::select(df, dplyr::all_of(keep))
})
diq_wide <- purrr::map_dfr(cycle_meta$suffix, ~{
  df <- safe_get("DIQ", .x)
  if ("diq010" %in% names(df)) dplyr::select(df, seqn, diq010, diq080) else tibble(seqn = df$seqn)
})
bpq_wide <- purrr::map_dfr(cycle_meta$suffix, ~{
  df <- safe_get("BPQ", .x)
  if ("bpq020" %in% names(df)) dplyr::select(df, seqn, bpq020) else tibble(seqn = df$seqn)
})
kiq_wide <- purrr::map_dfr(cycle_meta$suffix, ~{
  df <- safe_get("KIQ_U", .x)
  if ("kiq022" %in% names(df)) dplyr::select(df, seqn, kiq022) else tibble(seqn = df$seqn)
})

# Start from roster; left-join modules
health_wide <- demo_roster %>%
  dplyr::left_join(mcq_wide, by = "seqn") %>%
  dplyr::left_join(diq_wide, by = "seqn") %>%
  dplyr::left_join(bpq_wide, by = "seqn") %>%
  dplyr::left_join(kiq_wide, by = "seqn")

# Ensure all comorbidity columns exist
for (v in comorb_vars) if (!v %in% names(health_wide)) health_wide[[v]] <- NA

# Create diabetes split (retinopathy vs not) and RA flag from MCQ19x text
health_wide <- health_wide %>%
  mutate(
    diq_noret = case_when(
      diq010 == "Yes" & diq080 == "No"  ~ "Yes",
      diq010 == "Yes" & diq080 == "Yes" ~ "No",
      diq010 == "No"                    ~ "No",
      TRUE                              ~ NA_character_
    ),
    diq_ret = case_when(
      diq010 == "Yes" & diq080 == "Yes" ~ "Yes",
      diq010 == "Yes" & diq080 == "No"  ~ "No",
      diq010 == "No"                    ~ "No",
      TRUE                              ~ NA_character_
    ),
    mcq_ra = case_when(
      mcq190 == "Rheumatoid arthritis" |
        mcq191 == "Rheumatoid arthritis" |
        mcq195 == "Rheumatoid arthritis" ~ "Yes",
      TRUE ~ NA_character_
    )
  )

# Recode to 1/0 then compute counts and weighted sum
health_num <- health_wide %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(comorb_vars_reduced), yes1_else0))

comorb_matrix <- as.matrix(health_num[dplyr::all_of(comorb_vars_reduced)])
weights_vec   <- unlist(comorb_weights[comorb_vars_reduced])

mcq_clean <- health_num %>%
  dplyr::mutate(
    comorbidity_n = rowSums(comorb_matrix, na.rm = TRUE),
    cci_score     = as.numeric(comorb_matrix %*% weights_vec)
  ) %>%
  dplyr::select(seqn, comorbidity_n, cci_score)


# HIQ011 → Insurance: Yes / No / Don't know /Refused 
hiq_all <- purrr::map_dfr(cycle_meta$suffix, function(suf) {
  df <- nhanes_get("HIQ", suf, quiet = TRUE)
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(seqn = integer(), hiq011_raw = character()))
  }
  nm   <- names(df)
  cand <- nm[grepl("^hiq011", nm)]
  # keep a column even if that cycle lacks HIQ011
  tibble::tibble(
    seqn       = as.integer(df$seqn),
    hiq011_raw = if (length(cand)) as.character(df[[cand[1]]]) else NA_character_
  )
}) %>%
  dplyr::mutate(
    txt = stringr::str_to_lower(stringr::str_trim(hiq011_raw)),
    insured = dplyr::case_when(
      txt == "yes"                          ~ "Yes",
      txt == "no"                           ~ "No",
      txt %in% c("don't know","refused")  ~ "Don't know/refused",
      is.na(txt)                            ~ NA_character_,
      TRUE                                  ~ NA_character_
    ),
    insured = factor(insured, levels = c("No","Yes", "Don't know/refused"))
  ) %>%
  dplyr::select(seqn, insured)

# ---- HUQ harmonization: office visits & overnight hospital stays --------------
.norm <- function(x) {
  y <- toupper(trimws(as.character(x)))
  y[y %in% c("", ".", "..", "...", "NA")] <- NA_character_
  y
}

# HUQ050 → 0..5 scale
parse_huq050_to_050 <- function(x) {
  s <- .norm(x)
  out <- dplyr::case_when(
    grepl("\\bNONE\\b", s) ~ 0L,
    grepl("^0\\b", s)      ~ 0L,
    grepl("^1\\b|\\b1\\b|\\b1\\s+VIS", s) ~ 1L,
    grepl("2\\s*[-–]\\s*3|\\b2\\s*TO\\s*3\\b", s) ~ 2L,
    grepl("4\\s*[-–]\\s*9|\\b4\\s*TO\\s*9\\b", s) ~ 3L,
    grepl("10\\s*[-–]\\s*12|\\b10\\s*TO\\s*12\\b", s) ~ 4L,
    grepl("13\\s*OR\\s*MORE|13\\+|16\\s*OR\\s*MORE", s) ~ 5L,
    TRUE ~ suppressWarnings(as.integer(readr::parse_number(s)))
  )
  out[!(out %in% 0:5)] <- NA_integer_
  as.integer(out)
}

# HUQ051 (0,1, 2=2–3, 3=4–5, 4=6–7, 5=8–9, 6=10–12, 7=13–15, 8=16+) → HUQ050 scale
parse_huq051_to_050 <- function(x) {
  s <- .norm(x)
  v <- dplyr::case_when(
    grepl("\\bNONE\\b", s) ~ 0L,
    grepl("^0\\b", s)      ~ 0L,
    grepl("^1\\b|\\b1\\b|\\b1\\s+VIS", s) ~ 1L,
    grepl("2\\s*[-–]\\s*3|\\b2\\s*TO\\s*3\\b", s) ~ 2L,
    grepl("4\\s*[-–]\\s*5|\\b4\\s*TO\\s*5\\b", s) ~ 3L,
    grepl("6\\s*[-–]\\s*7|\\b6\\s*TO\\s*7\\b", s) ~ 3L,
    grepl("8\\s*[-–]\\s*9|\\b8\\s*TO\\s*9\\b", s) ~ 3L,
    grepl("10\\s*[-–]\\s*12|\\b10\\s*TO\\s*12\\b", s) ~ 4L,
    grepl("13\\s*[-–]\\s*15|\\b13\\s*TO\\s*15\\b", s) ~ 5L,
    grepl("16\\s*\\+|16\\s*OR\\s*MORE", s) ~ 5L,
    TRUE ~ suppressWarnings(as.integer(readr::parse_number(s)))
  )
  v <- dplyr::case_when(
    v %in% c(0L) ~ 0L,
    v %in% c(1L) ~ 1L,
    v %in% c(2L) ~ 2L,
    v %in% c(3L,4L,5L) ~ 3L,
    v %in% c(6L) ~ 4L,
    v %in% c(7L,8L) ~ 5L,
    TRUE ~ v
  )
  v[!(v %in% 0:5)] <- NA_integer_
  as.integer(v)
}

# HUQ071: 1=Yes, 2=No
parse_yesno_12 <- function(x) {
  s <- .norm(x)
  out <- dplyr::case_when(
    grepl("^1\\b|\\bYES\\b", s) ~ 1L,
    grepl("^2\\b|\\bNO\\b",  s) ~ 2L,
    TRUE ~ suppressWarnings(as.integer(readr::parse_number(s)))
  )
  out[!(out %in% c(1L,2L))] <- NA_integer_
  as.integer(out)
}

# HUD080: 1..6, where 6= “6 or more”
parse_hud080_1to6 <- function(x) {
  s <- .norm(x)
  out <- dplyr::case_when(
    grepl("6\\s*\\+|6\\s*OR\\s*MORE", s) ~ 6L,
    TRUE ~ suppressWarnings(as.integer(readr::parse_number(s)))
  )
  out[!(out %in% 1:6)] <- NA_integer_
  as.integer(out)
}

visits_lab   <- c("0 visits","1 visit","2–3 visits","4–9 visits","10–12 visits","≥13 visits")
hosp_labels  <- c("0 stays","1 stay","2 stays","3 stays","4 stays","5 stays","≥6 stays")

huq_util <- purrr::map_dfr(cycle_meta$suffix, function(suf) {
  df <- safe_get("HUQ", suf)
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(
      seqn = integer(),
      visits_cat_num = integer(),
      visits_cat = factor(levels = visits_lab),
      hosp_stays_num = integer(),
      hosp_stays_cat = factor(levels = hosp_labels)
    ))
  }
  nm <- names(df)
  get_chr <- function(v) if (v %in% nm) as.character(df[[v]]) else rep(NA_character_, nrow(df))
  
  # Visits
  v050 <- parse_huq050_to_050(get_chr("huq050"))
  v051 <- parse_huq051_to_050(get_chr("huq051"))
  visits_cat_num <- ifelse(!is.na(v050), v050, v051)
  visits_cat     <- factor(visits_cat_num, levels = 0:5, labels = visits_lab)
  
  # Hospital stays
  yn  <- parse_yesno_12(get_chr("huq071"))      # overnight stays? (1/2)
  h80 <- parse_hud080_1to6(get_chr("hud080"))   # if yes: how many (1..6)
  hosp_stays_num <- dplyr::case_when(
    yn == 2 ~ 0L,
    yn == 1 & !is.na(h80) ~ as.integer(h80),
    is.na(yn) & !is.na(h80) ~ as.integer(h80),   # gentle inference if HUQ071 missing
    TRUE ~ NA_integer_
  )
  hosp_stays_cat <- factor(hosp_stays_num, levels = 0:6, labels = hosp_labels)
  
  tibble::tibble(
    seqn = as.integer(df$seqn),
    visits_cat_num = visits_cat_num,
    visits_cat     = visits_cat,
    hosp_stays_num = hosp_stays_num,
    hosp_stays_cat = hosp_stays_cat
  )
}) %>%
  dplyr::distinct(seqn, .keep_all = TRUE) %>%
  select(seqn, visits_cat, hosp_stays_cat)

# ---- Mortality (public-use fixed-width files) --------------------------------
files <- list.files("data/mortality", full.names = TRUE, pattern = "\\.dat$", ignore.case = TRUE)

get_cycle <- function(path) {
  fn <- basename(path)
  m  <- stringr::str_match(fn, "NHANES_(\\d{4})_(\\d{4})")
  if (is.na(m[1,2])) NA_character_ else paste0(m[1,2], "/", m[1,3])
}

spec <- fwf_cols(
  seqn         = c(1, 6),
  eligstat     = c(15, 15),
  mortstat     = c(16, 16),
  ucod_leading = c(17, 19),
  diabetes     = c(20, 20),
  hyperten     = c(21, 21),
  permth_int   = c(43, 45),
  permth_exm   = c(46, 48)
)

read_one <- function(path) {
  cy <- get_cycle(path)
  readr::read_fwf(
    file          = path,
    col_positions = spec,
    col_types     = "iiiiiiii",
    na            = c("", ".")
  ) %>% mutate(cycle = cy, .before = 1)
}

nhanes_mortality <- if (length(files) > 0) {
  purrr::map_dfr(files, read_one) %>% dplyr::filter(eligstat == 1)
} else {
  warning("No mortality .dat files found under data/mortality; analytic dataset will be saved without time/event.")
  tibble(seqn = integer(), mortstat = integer(), permth_int = integer(), permth_exm = integer(),
         ucod_leading = integer())
} %>% janitor::clean_names()

# ---- Assemble analysis-ready dataset ----------------------------------------
analytic_raw <- demo_all %>%
  mutate(seqn = as.integer(seqn)) %>%
  left_join(rx_all, by = "seqn") %>%
  left_join(mcq_clean, by = "seqn") %>%
  left_join(hiq_all, by = "seqn") %>%
  left_join(huq_util, by = "seqn") %>%
  left_join(nhanes_mortality %>% select(seqn, mortstat, permth_int, permth_exm, ucod_leading), by = "seqn") %>%
  mutate(
    rxdcount = as.numeric(rxdcount),
    # Exposure (polypharmacy categories)
    poly_cat = case_when(
      !is.na(rxdcount) & rxdcount == 0                 ~ "0",
      !is.na(rxdcount) & rxdcount >= 1 & rxdcount <= 4 ~ "1–4",
      !is.na(rxdcount) & rxdcount >= 5 & rxdcount <= 9 ~ "5–9",
      !is.na(rxdcount) & rxdcount >= 10                ~ "≥10",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("0","1–4","5–9","≥10")),
    
    # Demographics & SES
    sex       = recode_sex(riagendr),
    race_eth  = derive_race_eth_r1(ridreth1),  # <-- RIDRETH1 only
    ed_cat    = derive_ed_cat(dmdeduc2),       # robust string mapping; Refused/DK -> NA
    indfmpir  = to_num(indfmpir),
    indfmpir_cat = case_when(
      is.na(indfmpir)              ~ NA_character_,
      indfmpir < 1                 ~ "<1.00 (below poverty)",
      indfmpir >= 1 & indfmpir < 2 ~ "1.00–1.99",
      indfmpir >= 2 & indfmpir < 4 ~ "2.00–3.99",
      indfmpir >= 4                ~ "≥4.00"
    ) %>% factor(levels = c("<1.00 (below poverty)","1.00–1.99","2.00–3.99","≥4.00")),
    insured   = factor(insured, levels = c("No","Yes", "Don't know/refused")),
    
    # Survival time/event
    time_m    = suppressWarnings(as.numeric(permth_int)),
    time_y    = time_m / 12,
    event     = suppressWarnings(as.integer(mortstat))
  )

# ---- Save --------------------------------------------------------------------
# saveRDS(analytic_raw, file = "data/analytic_raw.rds")