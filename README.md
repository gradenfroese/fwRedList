# fwise-iucn-maps

Nb. THIS IS A WIP V0 FIRST DRAFT AND SHOULD BE X2 CHECKED WHEN USING

Country-level maps and summary tables of freshwater animals threatened by invasive species (IUCN Red List) and of freshwater invasive species eradication attempts (FWISE).

| Script | What it does | Needs |
|---|---|---|
| `R/01_fetch_iucn.R` | Downloads IUCN Red List data through the API and builds tables 1–4 | IUCN API token, internet |
| `R/02_make_maps.R` | Builds Fig. 1 (two maps), Table 5 and a summary for the text | Outputs of `01`, FWISE export and species lookup |

## Setup

### 1. R packages

The script installs any missing packages it needs (`rredlist`, `dplyr`, `tidyr`, `purrr`, `countrycode`). For step 2 you may also want `usethis`:

```r
install.packages("usethis")
```

### 2. Get an IUCN Red List API token

1. Request a free token at <https://api.iucnredlist.org/>. Tokens are for non-commercial use under the [IUCN Red List Terms of Use](https://www.iucnredlist.org/terms/terms-of-use).
2. Save it so R can find it. Either run:

   ```r
   rredlist::rl_use_iucn()
   ```

   or open your `.Renviron` file:

   ```r
   usethis::edit_r_environ()
   ```

   and add this line (with your own token):

   ```
   IUCN_REDLIST_KEY=your_token_here
   ```

3. Restart R and check that it is set (this prints the token, so don't share the output):

   ```r
   nzchar(Sys.getenv("IUCN_REDLIST_KEY"))   # should be TRUE
   ```

The script reads the token from `IUCN_REDLIST_KEY` and never prints or saves it. **Never commit your `.Renviron` file.**

### 3. Get the latest FWISE data

1. Go to <https://fwise.org> and download the attempts export (an `.xlsx` file named like `fwise-attempts_YYYYMMDD-HHMMSS.xlsx`).
2. Save it in `data/fwise/`, together with the FWISE species–IUCN lookup (`fwise_species_iucn_YYYY-MM-DD.csv`). You can keep older files there; script `02` uses the most recent of each.

### 4. Arial font (for journal figures)

Script `02` uses Arial if it is installed, otherwise the default sans font. On Ubuntu:

```bash
sudo add-apt-repository multiverse
sudo apt update
sudo apt install ttf-mscorefonts-installer
fc-cache -f
```

Then restart R.

## Run

Open `fwise-iucn-maps.Rproj` in RStudio, then:

```r
source("R/01_fetch_iucn.R")   # IUCN data (slow the first time, cached after)
source("R/02_make_maps.R")    # figure, table 5, summary (no API calls)
```

The first run takes roughly 1–2 hours: it pages through all animal assessments, then makes one call per freshwater species threatened by invasives, with a 0.5 s pause between calls as IUCN recommends. Progress is cached in `data/cache/<Red List version>/`:

- **Interrupted?** Run it again; it resumes where it stopped and retries any failed downloads.
- **New Red List release?** Run it again; a new version gets its own cache, so everything is downloaded fresh.

## Definitions (script 01)

| Term | Rule |
|---|---|
| Animals | Kingdom ANIMALIA |
| Assessments | Latest global assessment per species (`latest = TRUE`, `scope_code = 1`) |
| Freshwater | System code 1, "Freshwater (=Inland waters)", among the species' systems, including species also listed as terrestrial or marine |
| Threatened by invasives | IUCN threat 8.1, *Invasive non-native/alien species/diseases* (8.1, 8.1.1, 8.1.2) |
| Threatened | CR, EN, VU |
| Undescribed species | Names with 3+ words (e.g. *Horatia* sp. nov. 'haasei') are kept: the API list has no subspecies, and IUCN counts these as species |
| Country range | Origin Native or Reintroduced. Presence Extant or Possibly Extinct; EX and EW species are counted where they used to occur (presence Extinct or Possibly Extinct) |

A species found in several countries counts once in each, so country rows add up to more than the species totals.

## Outputs (`data/`)

| File | Contents |
|---|---|
| `iucn_table1_by_category.csv` | Species per Red List category: all animals, freshwater, freshwater + threatened by invasives |
| `iucn_table2_by_group.csv` | Same for EX, Threatened, Threatened + EW, Threatened + EW + DD, with percentages |
| `iucn_table3_country_by_category.csv` | Freshwater species threatened by invasives, per country × category |
| `iucn_table4_country_by_group.csv` | Freshwater species threatened by invasives, per country × group |
| `iucn_fw_invasive_species_countries.csv` | One row per species × country kept (for checking) |
| `iucn_query_log.txt` | Red List version, run date, filters, counts, failed downloads, unmatched country codes, citation |

## Outputs (`output/`, script 02)

| File | Contents |
|---|---|
| `fig1_threatened_vs_protected.pdf` / `.png` (900 dpi) | (a) threatened freshwater species impacted by invasives, (b) threatened freshwater species protected by eradications, per country; 183 × 170 mm |
| `table5_fwise_by_country.csv` | FWISE attempts, successful attempts and threatened freshwater species protected, per country |
| `fwise_threatened_fw_protected_species.csv` | The protected threatened species, their countries, and whether IUCN codes invasives as a threat |
| `summary.txt` | Key numbers for the text |

### FWISE beneficiary rules (script 02)

Successful attempts only; beneficiary names to species level only (no "spp.", hybrids or vague groups such as "Native fish community"); subspecies counted once as their species; IUCN status (VU, EN, CR) from the FWISE species lookup, which includes checked synonyms; freshwater species only (IUCN system "Freshwater", from the list fetched by `01`).

## Functions

### `R/01_fetch_iucn.R`

| Function | Purpose |
|---|---|
| `check_token()` | Stops early if the token is missing or rejected |
| `cached(name, expr)` | Evaluates `expr` once and caches the result as RDS |
| `assessments_df(x)` | Pulls the `$assessments` table from an `rredlist` result |
| `fmt_time(sec)` | Formats seconds for progress messages |
| `fetch_list(fun, code, label)` | Paged list of latest global assessments for one code |
| `fetch_locations(ids)` | Per-assessment countries; cached, resumable, retries failures |
| `tidy_locations(loc_list)` | List of locations → one table |
| `keep_range(locs, category)` | Applies the origin and presence rules |
| `order_categories(x)` | Orders categories EX → DD (older codes such as LR/nt listed after) |
| `count_by_category(species)` | Table 1 |
| `count_by_group(table1)` | Table 2 |
| `count_country(sp_countries, cols, all_cols)` | Tables 3 and 4 |

### `R/02_make_maps.R`

| Function | Purpose |
|---|---|
| `newest(pattern)` | Newest file in `data/fwise/` matching a pattern |
| `read_cache(name)` | Reads a cached API list from the newest cache folder |
| `fwise_beneficiaries(raw)` | One row per attempt × beneficiary (handles old and new FWISE export formats) |
| `threatened_fw_protected(raw, lookup, fw_names)` | Applies the beneficiary rules above |
| `build_base()` | Natural Earth map units with ISO3 join keys (overseas territories separate; UK, Belgium etc. merged) |
| `map_panel(df, palette, tag, title, show_dots = FALSE, grey_rules = NULL)` | One log-scale choropleth with its colour bar below; small countries are coloured, dots optional |
| `grey_detached(p, rules)` | Panel b: greys listed pieces without FWISE sites (currently the Canary Islands) |
| `bar` | Colour bar guide, same as the Yosemite figure (ggplot2 ≥ 3.5) |
| `nice_breaks(maxn)` | Tick values 1, 2, 5, 10, ... up to the maximum |

## Data and citation

IUCN Red List data may not be redistributed as a dataset, so `data/` is not tracked in git. Anyone reproducing the outputs runs `01` with their own token.

Cite the IUCN data with the line written to `data/iucn_query_log.txt`, in the form:
IUCN ([year]). The IUCN Red List of Threatened Species. Version [version]. <https://www.iucnredlist.org>. Accessed [date].
