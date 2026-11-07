# =============================================================================
# 01_fetch_iucn.R
# Fetch IUCN Red List data (API v4, via rredlist) for animals and build the
# freshwater / invasive-species summary tables used by 02_make_maps.R.
#
# Before running: get an IUCN API token and save it (see README.md, "Setup").
# Run from the project root (open fwise-iucn-maps.Rproj, then source this file).
# First run takes a while (roughly 1-2 h): ~1,000 paged list calls plus one call
# per freshwater species threatened by invasives. Results are cached in
# data/cache/<Red List version>/, so an interrupted run resumes where it stopped,
# and a new Red List release starts a fresh download.
#
# Definitions
#   Animals        kingdom Animalia
#   Assessments    latest global assessment per species (latest = TRUE, scope_code = 1)
#   Freshwater     "Freshwater (=Inland waters)" among the species' systems
#                  (includes species also listed as terrestrial and/or marine)
#   Invasives      IUCN threat 8.1 "Invasive non-native/alien species/diseases"
#                  (8.1, 8.1.1 and 8.1.2 combined)
#   Country range  origin Native or Reintroduced; presence Extant, Probably Extant
#                  or Possibly Extinct, except EX/EW species, which are counted
#                  where they used to occur (presence Extinct)
#
# Outputs (data/)
#   iucn_table1_by_category.csv          n species per category: all / freshwater / FW + invasives
#   iucn_table2_by_group.csv             same, for EX / Threatened / Threatened + EW / Threatened + EW + DD, with %
#   iucn_table3_country_by_category.csv  FW + invasives species per country x category
#   iucn_table4_country_by_group.csv     FW + invasives species per country x group
#   iucn_fw_invasive_species_countries.csv  long table: one row per species x country kept
#   iucn_query_log.txt                   Red List version, run date, filters, counts
#
# Functions
#   check_token()                        stop early if no token / token rejected
#   cached(name, expr)                   evaluate expr once, cache result as RDS
#   assessments_df(x)                    pull the $assessments data.frame from an rredlist result
#   fmt_time(sec)                        seconds -> "1h 5m" style text for progress messages
#   fetch_list(fun, code, label)         paged list of latest global assessments for one code
#   fetch_locations(ids)                 per-assessment locations (countries), cached and resumable
#   tidy_locations(loc_list)             list of locations -> one data.frame
#   keep_range(locs, category)           apply the origin / presence rules above
#   order_categories(x)                  factor with IUCN category order (other codes, e.g. LR/cd, after DD)
#   count_by_category(species)           -> table 1
#   count_by_group(table1)               -> table 2
#   count_country(sp_countries, cols, all_cols) -> tables 3 and 4
# =============================================================================

# install only the packages that are missing, then load them
pkgs    <- c("rredlist", "dplyr", "tidyr", "purrr", "countrycode")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))

data_dir <- "data"

wait_time   <- 0.5                          # seconds between per-assessment calls (IUCN recommendation)
kingdom     <- "ANIMALIA"                  # as spelled by rl_kingdom()
fw_code     <- "1"                         # rl_systems(): 1 = Freshwater (=Inland waters)
threat_code <- c("8_1", "8_1_1", "8_1_2")   # invasive non-native/alien species/diseases
# sensitivity check: add "8_4", "8_4_1", "8_4_2" (problematic species/diseases of unknown origin)
categories  <- c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD")

# Old Lower Risk codes (IUCN v2.3, pre-2001). Species keep them until reassessed, and the
# Red List flags these assessments "Needs updating". Following IUCN's own summary statistics:
#   LR/nt -> NT and LR/lc -> LC (merged);
#   LR/cd (conservation dependent) is kept as its own category, listed after DD.
# None of these enter the groups below (EX / Threatened / +EW / +DD).
# The original code is kept in `category_original`, and the log reports how many were merged.
lr_merge    <- c("LR/nt" = "NT", "LR/lc" = "LC")
groups <- list(
  "EX"                          = "EX",
  "Threatened"                  = c("CR", "EN", "VU"),
  "Threatened + EW"             = c("EW", "CR", "EN", "VU"),
  "Threatened + EW + DD"        = c("EW", "CR", "EN", "VU", "DD")
)
keep_origin          <- c("Native", "Reintroduced")
keep_presence_living <- c("Extant", "Probably Extant", "Possibly Extinct")

# ---- helpers ----------------------------------------------------------------

check_token <- function() {
  key <- Sys.getenv("IUCN_REDLIST_KEY")
  if (!nzchar(key)) {
    stop("No IUCN API token found (IUCN_REDLIST_KEY is empty).\n",
         "See README.md, 'Setup', to get a token and save it, then restart R.",
         call. = FALSE)
  }
  ok <- tryCatch({ rl_version(); TRUE }, error = function(e) FALSE)
  if (!ok) {
    stop("The IUCN API rejected the token or could not be reached.\n",
         "Check the token (README.md, 'Setup') and your connection.",
         call. = FALSE)
  }
  invisible(TRUE)
}

cached <- function(name, expr) {
  path <- file.path(cache_dir, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  value <- expr
  saveRDS(value, path)
  value
}

assessments_df <- function(x) {
  df <- if (is.data.frame(x)) x else x$assessments
  if (!is.data.frame(df)) stop("Unexpected API result: no assessments table.", call. = FALSE)
  df %>%
    as_tibble() %>%
    select(any_of(c("assessment_id", "sis_taxon_id", "taxon_scientific_name",
                    "red_list_category_code", "year_published", "latest")))
}

fmt_time <- function(sec) {
  sec <- round(sec)
  if (sec < 60) return(paste0(sec, "s"))
  h <- sec %/% 3600; m <- (sec %% 3600) %/% 60
  if (h > 0) paste0(h, "h ", m, "m") else paste0(m, "m ", sec %% 60, "s")
}

fetch_list <- function(fun, code, label) {
  cached(label, {
    message("Fetching ", label, " (paging through the API, 100 records per page) ...")
    t0 <- Sys.time()
    out <- fun(code, latest = TRUE, scope_code = 1, quiet = FALSE)
    message("  done in ", fmt_time(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    out %>%
      assessments_df()
  })
}

fetch_locations <- function(ids) {
  path <- file.path(cache_dir, "locations.rds")
  done <- if (file.exists(path)) readRDS(path) else list()
  failed_before <- names(done)[map_lgl(done, ~ inherits(.x, "fetch_error"))]
  todo <- c(setdiff(as.character(ids), names(done)), intersect(failed_before, as.character(ids)))
  message(length(done), " assessments cached; ", length(todo), " to fetch.")

  t0 <- Sys.time()
  for (i in seq_along(todo)) {
    id  <- todo[i]
    res <- tryCatch(rl_assessment(as.integer(id))$locations,
                    error = function(e) structure(conditionMessage(e), class = "fetch_error"))
    done[[id]] <- res
    Sys.sleep(wait_time)
    if (i %% 25 == 0 || i == length(todo)) {
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      left    <- elapsed / i * (length(todo) - i)
      message(sprintf("  countries: %d / %d (%d%%) | elapsed %s | ~%s left",
                      i, length(todo), round(100 * i / length(todo)),
                      fmt_time(elapsed), fmt_time(left)))
    }
    if (i %% 100 == 0 || i == length(todo)) saveRDS(done, path)
  }
  done[as.character(ids)]
}

tidy_locations <- function(loc_list) {
  imap_dfr(loc_list, function(loc, id) {
    if (inherits(loc, "fetch_error") || is.null(loc) || length(loc) == 0) {
      return(tibble(assessment_id = as.integer(id), fetch_ok = !inherits(loc, "fetch_error")))
    }
    loc  <- as_tibble(loc)
    name <- if (is.data.frame(loc$description)) loc$description$en else if (!is.null(loc$description)) loc$description else loc$en
    tibble(assessment_id = as.integer(id),
           fetch_ok      = TRUE,
           location_code = loc$code,
           location_name = name,
           presence      = loc$presence,
           origin        = loc$origin)
  })
}

keep_range <- function(locs, category) {
  extinct_cat <- category %in% c("EX", "EW")
  locs$origin %in% keep_origin &
    ifelse(extinct_cat,
           # EX/EW: any extinct record, incl. "Possibly Extinct" (extinct everywhere anyway)
           grepl("Extinct", locs$presence, ignore.case = TRUE),
           locs$presence %in% keep_presence_living)
}

order_categories <- function(x) {
  factor(x, levels = c(categories, sort(setdiff(unique(x), categories))))
}

count_by_category <- function(species) {
  species %>%
    group_by(category = order_categories(red_list_category_code)) %>%
    # count fw + invasive BEFORE `freshwater` is overwritten by its own count
    summarise(all_species         = n_distinct(sis_taxon_id),
              freshwater_invasive = n_distinct(sis_taxon_id[freshwater & invasive]),
              freshwater          = n_distinct(sis_taxon_id[freshwater]),
              .groups = "drop") %>%
    relocate(freshwater, .before = freshwater_invasive) %>%
    arrange(category) %>%
    mutate(category = as.character(category)) %>%
    bind_rows(summarise(., category = "Total", across(where(is.numeric), sum)))
}

count_by_group <- function(table1) {
  imap_dfr(groups, function(cats, grp) {
    table1 %>%
      filter(category %in% cats) %>%
      summarise(group = grp, across(where(is.numeric), sum))
  }) %>%
    mutate(pct_freshwater_of_all       = round(100 * freshwater / all_species, 1),
           pct_fw_invasive_of_fw       = round(100 * freshwater_invasive / freshwater, 1),
           pct_fw_invasive_of_all      = round(100 * freshwater_invasive / all_species, 1))
}

count_country <- function(sp_countries, cols, all_cols) {
  out <- sp_countries %>%
    group_by(iso3, country, col = cols) %>%
    summarise(n = n_distinct(sis_taxon_id), .groups = "drop") %>%
    pivot_wider(names_from = col, values_from = n, values_fill = 0)
  out[setdiff(all_cols, names(out))] <- 0L   # keep every column, even if all zero
  out
}

# ---- 1. token and version ---------------------------------------------------

check_token()
rl_vers  <- rl_version()
run_date <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
message("IUCN Red List version: ", rl_vers)

# cache per Red List version: a new IUCN release triggers a fresh download
cache_dir <- file.path(data_dir, "cache", gsub("[^0-9A-Za-z.-]", "_", rl_vers))
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 2. lists: animals, freshwater, invasives -------------------------------

message("\n[Step 1/3] Species lists")

animals <- fetch_list(rl_kingdom, kingdom, "list_animals")

freshwater <- fetch_list(rl_systems, fw_code, "list_freshwater")

invasive <- map_dfr(threat_code, ~ fetch_list(rl_threats, .x, paste0("list_threat_", .x))) %>%
  distinct(assessment_id, .keep_all = TRUE)

# All latest global animal assessments are kept. The list contains no subspecies or
# subpopulations; names with 3+ words are undescribed/provisional species
# (e.g. "Horatia sp. nov. 'haasei'"), which IUCN counts as species, so they stay in.
species <- animals %>%
  distinct(assessment_id, .keep_all = TRUE) %>%
  mutate(category_original      = red_list_category_code,
         red_list_category_code = recode(red_list_category_code, !!!lr_merge)) %>%
  mutate(freshwater  = assessment_id %in% freshwater$assessment_id,
         invasive    = assessment_id %in% invasive$assessment_id,
         provisional = lengths(strsplit(trimws(taxon_scientific_name), "\\s+")) > 2)

# ---- 3. tables 1 and 2 ------------------------------------------------------

table1 <- count_by_category(species)
stopifnot("Table 1: freshwater_invasive > freshwater" = all(table1$freshwater_invasive <= table1$freshwater),
          "Table 1: freshwater > all_species"         = all(table1$freshwater <= table1$all_species))
table2 <- count_by_group(table1)

# ---- 4. countries for freshwater species threatened by invasives ------------

fw_inv <- species %>%
  filter(freshwater, invasive)

message("\n[Step 2/3] Countries for ", nrow(fw_inv), " freshwater species threatened by invasives (longest step)")

locs <- fetch_locations(fw_inv$assessment_id) %>%
  tidy_locations()

failed <- locs %>%
  filter(!fetch_ok) %>%
  pull(assessment_id)

# Location codes: 2-letter codes are countries (ISO 3166-1 alpha-2). Longer codes
# (e.g. "KAN-OO" Kansas, "QLD-QU" Queensland) are TDWG sub-national regions, NOT
# ISO-based, so they are not used; each species normally also has a country record.
loc_kept <- locs %>%
  filter(fetch_ok, !is.na(location_code)) %>%
  inner_join(fw_inv, by = "assessment_id") %>%
  filter(keep_range(., red_list_category_code)) %>%
  mutate(national = grepl("^[A-Z]{2}$", location_code))

sp_countries <- loc_kept %>%
  filter(national) %>%
  mutate(iso2    = location_code,
         iso3    = countrycode(iso2, "iso2c", "iso3c", warn = FALSE),
         country = countrycode(iso2, "iso2c", "country.name", warn = FALSE)) %>%
  distinct(sis_taxon_id, iso3, .keep_all = TRUE) %>%
  select(assessment_id, sis_taxon_id, scientific_name = taxon_scientific_name,
         category = red_list_category_code, location_code, location_name,
         presence, origin, iso2, iso3, country)

# species with sub-national records but no country record (would be missing from tables 3/4)
subnational_only <- loc_kept %>%
  filter(!national) %>%
  distinct(sis_taxon_id, taxon_scientific_name) %>%
  filter(!sis_taxon_id %in% sp_countries$sis_taxon_id)

unmatched <- sp_countries %>%
  filter(is.na(iso3)) %>%
  distinct(location_code, location_name)

table3 <- sp_countries %>%
  filter(!is.na(iso3)) %>%
  count_country(cols = order_categories(.$category), all_cols = categories) %>%
  select(iso3, country, any_of(categories), everything()) %>%
  arrange(country)

table4 <- imap_dfr(groups, function(cats, grp) {
  sp_countries %>%
    filter(!is.na(iso3), category %in% cats) %>%
    mutate(group = grp)
}) %>%
  count_country(cols = factor(.$group, levels = names(groups)), all_cols = names(groups)) %>%
  select(iso3, country, any_of(names(groups))) %>%
  arrange(country)

# ---- 5. write ---------------------------------------------------------------

message("\n[Step 3/3] Writing tables to ", data_dir, "/\n")

write.csv(table1,       file.path(data_dir, "iucn_table1_by_category.csv"),            row.names = FALSE)
write.csv(table2,       file.path(data_dir, "iucn_table2_by_group.csv"),               row.names = FALSE)
write.csv(table3,       file.path(data_dir, "iucn_table3_country_by_category.csv"),    row.names = FALSE)
write.csv(table4,       file.path(data_dir, "iucn_table4_country_by_group.csv"),       row.names = FALSE)
write.csv(sp_countries, file.path(data_dir, "iucn_fw_invasive_species_countries.csv"), row.names = FALSE)

log_lines <- c(
  paste("IUCN Red List version:", rl_vers),
  paste("Run date:", run_date),
  "Filters: kingdom ANIMALIA; latest = TRUE; scope_code = 1 (Global)",
  paste0("Freshwater: system code ", fw_code, " (Freshwater (=Inland waters), any listing)"),
  paste("Invasives: threat codes", paste(gsub("_", ".", threat_code), collapse = ", ")),
  paste("Country range: origin", paste(keep_origin, collapse = "/"),
        "; presence", paste(keep_presence_living, collapse = "/"),
        "(EX/EW: presence Extinct or Possibly Extinct)"),
  "",
  paste("Animal species:", n_distinct(species$sis_taxon_id)),
  paste("Undescribed/provisional species included (3+ word names):", sum(species$provisional),
        "| freshwater:", sum(species$provisional & species$freshwater),
        "| freshwater + invasives:", sum(species$provisional & species$freshwater & species$invasive)),
  paste("Old Lower Risk codes merged:",
        paste(names(lr_merge), "->", lr_merge, ":",
              vapply(names(lr_merge), function(x) sum(species$category_original == x), integer(1)),
              collapse = "; "),
        "| LR/cd kept separate:", sum(species$category_original == "LR/cd")),
  paste("Freshwater species:", sum(table1$freshwater[table1$category != "Total"])),
  paste("Freshwater species threatened by invasives:", nrow(fw_inv)),
  paste("  with no countries kept after range rules:",
        length(setdiff(fw_inv$sis_taxon_id, sp_countries$sis_taxon_id))),
  paste("  assessments that failed to download:", length(failed),
        if (length(failed)) paste0("(", paste(failed, collapse = ", "), "; rerun to retry)") else ""),
  paste("Sub-national (TDWG) records ignored:", sum(!loc_kept$national), "rows"),
  paste("  species with only sub-national records (missing from tables 3/4):", nrow(subnational_only),
        if (nrow(subnational_only)) paste0("(", paste(head(subnational_only$taxon_scientific_name, 20), collapse = "; "), ")") else ""),
  paste("Location codes not matched to a country:",
        if (nrow(unmatched)) paste(paste0(unmatched$location_code, " (", unmatched$location_name, ")"), collapse = "; ") else "none"),
  "",
  paste0("Citation: IUCN (", substr(rl_vers, 1, 4), "). The IUCN Red List of Threatened Species. Version ",
         rl_vers, ". https://www.iucnredlist.org. Accessed ", format(Sys.Date(), "%d %B %Y"), ".")
)
writeLines(log_lines, file.path(data_dir, "iucn_query_log.txt"))

message(paste(log_lines, collapse = "\n"))
