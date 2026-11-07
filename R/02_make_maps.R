# =============================================================================
# 02_make_maps.R
# Fig. 1 (two maps) and Table 5, from the outputs of 01_fetch_iucn.R and the
# latest FWISE export:
#   (a) threatened (CR/EN/VU) freshwater species threatened by invasives, per country (IUCN)
#   (b) threatened (CR/EN/VU) freshwater species protected by successful eradications (FWISE)
#
# Run from the project root after 01_fetch_iucn.R. No API calls.
#
# Inputs
#   data/iucn_fw_invasive_species_countries.csv   from 01 (species x country)
#   data/cache/<Red List version>/list_*.rds      from 01 (species lists)
#   data/fwise/fwise-attempts_*.xlsx              newest FWISE export (see README)
#   data/fwise/fwise_species_iucn_*.csv           newest FWISE species-IUCN lookup
#
# Outputs (output/)
#   fig1_threatened_vs_protected.pdf / .png       Nature size, 183 x 170 mm; PNG at 900 dpi
#   table5_fwise_by_country.csv                   attempts, successful attempts, threatened fw species protected
#   fwise_threatened_fw_protected_species.csv     the protected species (for checking)
#   summary.txt                                   key numbers for the text
#
# Rules for FWISE beneficiaries (as in fwise_threatened_species_plot.R, gf 06.10.26)
#   successful attempts only; names to species level only (no spp., hybrids, vague groups);
#   subspecies counted once as their species; IUCN status from the FWISE lookup (VU/EN/CR);
#   plus: freshwater species only (IUCN system "Freshwater", from the API list fetched by 01)
#
# Functions
#   newest(pattern)                     newest file in data/fwise/ matching pattern
#   read_cache(name)                    read a cached API list from the newest cache folder
#   fwise_beneficiaries(raw)            one row per attempt x beneficiary string
#   threatened_fw_protected(...)        apply the rules above -> species x country table
#   build_base()                        Natural Earth map units with ISO3 join keys
#   map_panel(df, palette, tag, title, show_dots = FALSE, grey_rules = NULL)  one choropleth (log scale) + legend
#   grey_detached(p, rules)             panel b: grey listed pieces without FWISE sites (Canary Islands)
#   bar                                 colour bar guide (as in the Yosemite figure)
#   nice_breaks(maxn)                   1, 2, 5, 10, ... up to max
# =============================================================================

pkgs    <- c("dplyr", "tidyr", "stringr", "readr", "readxl", "purrr", "sf", "ggplot2",
             "patchwork", "rnaturalearth", "rnaturalearthdata", "systemfonts")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))
sf_use_s2(FALSE)

data_dir  <- "data"
fwise_dir <- file.path(data_dir, "fwise")
out_dir   <- "output"
dir.create(out_dir, showWarnings = FALSE)

# ---- style ------------------------------------------------------------------

font <- if ("Arial" %in% systemfonts::system_fonts()$family) "Arial" else "sans"
if (font != "Arial") message("Arial not found - using the default sans font (see README).")

navy      <- "#191144"
teal      <- "#A0D1CB"
land_grey <- "#EBEBEB"
line_grey <- "#B4B4B4"
ramp_iucn  <- c("#F6E08A", "#F2B05E", "#E2704C", "#A23A3A", "#3A1A1A")   # threat: yellow -> red -> dark brown
ramp_fwise <- c(teal, "#5FA9AF", "#2F7290", "#263C70", navy)             # action: teal -> navy
robinson   <- "+proj=robin +lon_0=0 +datum=WGS84"
pt         <- function(x) x / .pt                                        # font size in points

threatened <- c("CR", "EN", "VU")

# ---- helpers ----------------------------------------------------------------

newest <- function(pattern) {
  f <- list.files(fwise_dir, pattern = pattern, full.names = TRUE)
  if (!length(f)) stop("No file matching '", pattern, "' in ", fwise_dir, " (see README).", call. = FALSE)
  f[which.max(file.mtime(f))]
}

read_cache <- function(name) {
  dirs <- list.dirs(file.path(data_dir, "cache"), recursive = FALSE)
  if (!length(dirs)) stop("No cache found - run 01_fetch_iucn.R first.", call. = FALSE)
  readRDS(file.path(dirs[which.max(file.mtime(dirs))], paste0(name, ".rds")))
}

fwise_beneficiaries <- function(raw) {
  ben_cols <- grep("^beneficiary_species_[0-9]+$", names(raw), value = TRUE)
  if (length(ben_cols)) {                                  # older export: one column per species
    raw %>%
      select(attempt_id, iso3, outcome, all_of(ben_cols)) %>%
      pivot_longer(all_of(ben_cols), values_to = "species") %>%
      select(-name)
  } else {                                                 # current export: "a; b; c"
    raw %>%
      select(attempt_id, iso3, outcome, species = beneficiary_species) %>%
      separate_rows(species, sep = ";\\s*")
  }
}

threatened_fw_protected <- function(raw, lookup, fw_names) {
  vague_starts <- c("Alpine", "Aquatic", "Chironomidae", "Endemic", "Fishery", "Game", "Migratory", "Native")

  spp_specific <- lookup %>%
    filter(kind == "Beneficiary") %>%
    mutate(sci       = coalesce(str_match(species, "\\(([^()]*)\\)$")[, 2], species),
           has_paren = str_detect(species, "\\)$"),
           specific  = str_detect(sci, "^[A-Z][a-z]+ [a-z]+( [a-z]+)?$") &
                       !str_detect(sci, "hybrid") &
                       (has_paren | !word(sci, 1) %in% vague_starts)) %>%
    filter(specific, iucn_status %in% threatened) %>%
    mutate(binomial  = word(sci, 1, 2),
           iucn_name = coalesce(iucn_matched_name, sci),
           # freshwater per IUCN: matched name, then binomial
           freshwater = iucn_name %in% fw_names | word(iucn_name, 1, 2) %in% fw_names | binomial %in% fw_names)

  fwise_beneficiaries(raw) %>%
    filter(outcome %in% "Successful", !is.na(species)) %>%
    inner_join(spp_specific %>%
                 select(species, binomial, iucn_name, iucn_status, taxa, freshwater),
               by = "species")
}

build_base <- function() {
  rnaturalearth::ne_countries(scale = 50, type = "map_units", returnclass = "sf") %>%
    filter(adm0_a3 != "ATA") %>%
    mutate(key = if_else(iso_a3 != "-99", iso_a3, adm0_a3),          # ISO3 where NE has one
           key = recode(key, PSX = "PSE", SOL = "SOM", CYN = "CYP", NJM = "SJM", NOR = "NOR")) %>%
    mutate(key = if_else(gu_a3 == "NJM", "SJM", key)) %>%            # Jan Mayen with Svalbard (ISO SJ)
    group_by(key) %>%
    summarise(.groups = "drop") %>%                                  # dissolve internal borders (UK, BEL, ...)
    st_make_valid() %>%
    mutate(area_km2 = as.numeric(st_area(st_transform(., "+proj=moll"))) / 1e6)
}

map_panel <- function(df, palette, tag, title, show_dots = FALSE, grey_rules = NULL) {
  p <- base %>%
    left_join(df, by = "key")
  if (!is.null(grey_rules)) p <- grey_detached(p, grey_rules)
  # optional: small countries (total area < 15,000 km2) get a dot so they are visible (off by default;
  # they are still coloured, and their numbers are in Table 3)
  dots <- p %>%
    filter(!is.na(n), area_km2 < 15000) %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    st_cast("POLYGON", warn = FALSE) %>%
    mutate(a = as.numeric(st_area(st_transform(., "+proj=moll")))) %>%
    group_by(key) %>%
    slice_max(a, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    st_point_on_surface()

  ggplot() +
    geom_sf(data = p %>%
              filter(is.na(n)),
            fill = land_grey, colour = line_grey, linewidth = 0.08) +
    geom_sf(data = p %>%
              filter(!is.na(n)),
            aes(fill = n), colour = navy, linewidth = 0.06) +
    { if (show_dots) geom_sf(data = dots, aes(fill = n), shape = 21, size = 1.1, colour = navy, stroke = 0.2) } +
    scale_fill_gradientn(colours = palette, trans = "log10", limits = c(1, max(df$n)),
                         breaks = nice_breaks(max(df$n)), labels = scales::label_comma(),
                         name = title, guide = bar) +
    coord_sf(crs = robinson, expand = FALSE) +
    labs(tag = tag) +
    theme_void(base_family = font) +
    theme(plot.tag          = element_text(face = "bold", size = 8, colour = "black", vjust = 1),
          plot.tag.position = c(0, 1),
          plot.margin       = margin(t = 3, r = side_mm, b = 1, l = side_mm, unit = "mm"),
          legend.position   = "bottom",
          legend.title      = element_text(face = "bold", size = 7, hjust = 0.5),
          legend.text       = element_text(size = 6),
          legend.margin     = margin(0, 0, 0, 0),
          legend.box.margin = margin(t = 1))
}

# Panel b only: grey named pieces of a country where FWISE has no relevant site
# (rule = function of each piece's lon/lat). Currently only the Canary Islands:
# Spain's protected species come from mainland eradications.
detached_rules_b <- list(
  ESP = function(lon, lat) lat < 30    # Canary Islands
)

grey_detached <- function(p, rules = detached_rules_b) {
  parts <- p %>%
    filter(key %in% names(rules), !is.na(n)) %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    st_cast("POLYGON", warn = FALSE)
  if (!nrow(parts)) return(p)
  xy <- suppressWarnings(st_coordinates(st_point_on_surface(parts)))
  for (k in names(rules)) {
    i <- parts$key == k
    parts$n[i & rules[[k]](xy[, 1], xy[, 2])] <- NA
  }
  bind_rows(p %>%
              filter(!(key %in% names(rules) & !is.na(n))),
            parts)
}

nice_breaks <- function(maxn) {
  b <- c(1, 2, 5, 10, 20, 50, 100, 200, 500)
  unique(c(b[b < maxn * 0.7], maxn))
}

# colour bar as in the Yosemite figure (04_fig1_maps.R): ggplot2's own legend, thin frame,
# small black ticks hanging below the bar (needs ggplot2 >= 3.5)
bar <- guide_colourbar(display = "gradient",   # vector gradient: end ticks line up exactly with the frame
                       theme = theme(legend.title.position = "top",
                                     legend.key.width    = unit(7, "cm"),
                                     legend.key.height   = unit(0.25, "cm"),
                                     legend.frame        = element_rect(colour = "black", linewidth = 0.25),
                                     legend.ticks        = element_line(colour = "black", linewidth = 0.25),
                                     legend.ticks.length = unit(c(-0.08, 0), "cm")))

# ---- data -------------------------------------------------------------------

sp_countries <- read_csv(file.path(data_dir, "iucn_fw_invasive_species_countries.csv"),
                         na = "", show_col_types = FALSE) %>%      # na = "": keep Namibia's iso2 "NA"
  mutate(iso3 = na_if(iso3, "NA"))                                   # ...but iso3 "NA" is a real NA (e.g. Disputed Territory)
animals    <- read_cache("list_animals")
fw_list    <- read_cache("list_freshwater")
inv_list   <- bind_rows(lapply(c("list_threat_8_1", "list_threat_8_1_1", "list_threat_8_1_2"), read_cache))

raw_file    <- newest("^fwise-attempts_.*\\.xlsx$")
lookup_file <- newest("^fwise_species_iucn_.*\\.csv$")
raw    <- read_excel(raw_file, sheet = 1)
lookup <- read_csv(lookup_file, show_col_types = FALSE)

fw_names <- unique(fw_list$taxon_scientific_name)

# (a) IUCN: threatened freshwater species threatened by invasives, per country
iucn_thr <- sp_countries %>%
  filter(category %in% threatened, !is.na(iso3)) %>%
  distinct(iso3, sis_taxon_id) %>%
  count(key = iso3, name = "n")

# (b) FWISE: threatened freshwater species protected by successful eradications, per country
prot <- threatened_fw_protected(raw, lookup, fw_names)
prot_fw <- prot %>%
  filter(freshwater)
fwise_prot <- prot_fw %>%
  distinct(iso3, binomial) %>%
  count(key = iso3, name = "n")

# ---- Table 5 and protected-species list -------------------------------------

table5 <- raw %>%
  group_by(iso3, country) %>%
  summarise(attempts            = n_distinct(attempt_id),
            successful_attempts = n_distinct(attempt_id[outcome %in% "Successful"]),
            .groups = "drop") %>%
  left_join(fwise_prot %>%
              rename(iso3 = key, threatened_fw_species_protected = n),
            by = "iso3") %>%
  mutate(threatened_fw_species_protected = coalesce(threatened_fw_species_protected, 0L)) %>%
  arrange(desc(attempts))

# IUCN freshwater species threatened by invasives (any category) - to check overlap
fw_inv_thr_names <- animals %>%
  filter(assessment_id %in% fw_list$assessment_id,
         assessment_id %in% inv_list$assessment_id,
         red_list_category_code %in% threatened) %>%
  pull(taxon_scientific_name)

prot_species <- prot %>%
  group_by(binomial, iucn_name, iucn_status, taxa, freshwater) %>%
  summarise(countries          = paste(sort(unique(iso3)), collapse = ", "),
            successful_attempts = n_distinct(attempt_id),
            .groups = "drop") %>%
  mutate(iucn_codes_invasives = iucn_name %in% fw_inv_thr_names | binomial %in% fw_inv_thr_names) %>%
  arrange(desc(freshwater), iucn_status, binomial)

write_csv(table5,       file.path(out_dir, "table5_fwise_by_country.csv"))
write_csv(prot_species, file.path(out_dir, "fwise_threatened_fw_protected_species.csv"))

# ---- Fig. 1 -----------------------------------------------------------------

base <- build_base()

unjoined <- setdiff(c(iucn_thr$key, fwise_prot$key), base$key)

# layout (mm): map 160 mm wide, centred (side margins); legend below each map
fig_w <- 183; map_w <- 160; leg_h <- 11
side_mm <- (fig_w - map_w) / 2
bb    <- st_bbox(st_transform(base, robinson))
map_h <- map_w * (bb[["ymax"]] - bb[["ymin"]]) / (bb[["xmax"]] - bb[["xmin"]])
fig_h <- 2 * (map_h + 3 + 1 + leg_h)

fig <- wrap_plots(map_panel(iucn_thr,   ramp_iucn,  "a", "Threatened freshwater species impacted by invasives"),
                  map_panel(fwise_prot, ramp_fwise, "b", "Threatened freshwater species protected by eradications",
                            grey_rules = detached_rules_b),
                  ncol = 1)

ggsave(file.path(out_dir, "fig1_threatened_vs_protected.pdf"), fig,
       width = fig_w, height = fig_h, units = "mm", device = cairo_pdf, bg = "white")
ggsave(file.path(out_dir, "fig1_threatened_vs_protected.png"), fig,
       width = fig_w, height = fig_h, units = "mm", dpi = 900, bg = "white")

# ---- summary for the text ---------------------------------------------------

n_thr      <- n_distinct(sp_countries$sis_taxon_id[sp_countries$category %in% threatened])
n_prot     <- n_distinct(prot_fw$binomial)
n_overlap  <- sum(prot_species$freshwater & prot_species$iucn_codes_invasives)

summary_lines <- c(
  paste("FWISE export:", basename(raw_file), "| lookup:", basename(lookup_file)),
  paste("IUCN: threatened (CR/EN/VU) freshwater species threatened by invasives, with countries:", n_thr),
  paste("FWISE: threatened beneficiary species of successful eradications:", n_distinct(prot$binomial),
        "| freshwater (IUCN):", n_prot,
        "| not freshwater:", paste(sort(unique(prot$binomial[!prot$freshwater])), collapse = "; ")),
  paste("  of the", n_prot, "protected freshwater species, IUCN codes invasives (8.1) as a threat for:", n_overlap),
  paste0("  share of IUCN threatened fw species threatened by invasives that are protected: ",
         n_overlap, " / ", n_thr, " (", round(100 * n_overlap / n_thr, 1), "%)"),
  paste("Countries mapped: (a)", nrow(iucn_thr), "| (b)", nrow(fwise_prot)),
  paste("Country codes with no map polygon:", if (length(unjoined)) paste(unjoined, collapse = ", ") else "none"),
  paste("Panel b, pieces greyed (no FWISE site):", paste(names(detached_rules_b), collapse = ", "), "(Canary Islands)"),
  paste("Figure size:", fig_w, "x", round(fig_h), "mm"),
  paste("Font:", font)
)
writeLines(summary_lines, file.path(out_dir, "summary.txt"))
message(paste(summary_lines, collapse = "\n"))
