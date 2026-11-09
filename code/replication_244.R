# Headcount or labour-input resilience?
# Public R replication for the authoritative 244-region release
#
# This script reproduces the analysis originally implemented in Python.  It
# reads the frozen total-economy Eurostat panel and validated N244 sector
# intermediates, reconstructs every total-economy sample from explicit rules,
# and writes only publication-relevant tables, figures, diagnostics and
# provenance files. Sector-cell statistics are independently recomputed from
# the validated intermediate; country-sector aggregates and minimum-cell-size
# checks remain frozen release references until the complete sector raw file is
# re-acquired. The legacy `replication (2).R` is not an input and is deliberately
# left unchanged.

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("data.table", "ggplot2", "scales")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0L) {
  stop(
    "Install the required packages before running the replication: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

config <- list(
  hours_dataset = "nama_10r_2emhrw",
  persons_dataset = "nama_10r_3empers",
  years = 2000:2023,
  event_years = 2019:2021,
  legacy_years = c(2008, 2009, 2019:2023),
  balanced_years = 2000:2021,
  event_reference = 2019L,
  statuses = c("EMP", "SAL", "SELF"),
  comparison_statuses = c("SAL", "SELF"),
  sectors = c("A", "B-E", "F", "G-I", "J", "K", "L", "M_N", "O-Q", "R-U"),
  eu27 = c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
    "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
    "NL", "PL", "PT", "RO", "SE", "SI", "SK"
  ),
  capital_regions = c(
    "AT13", "BE10", "BG41", "CY00", "CZ01", "DE30", "DK01", "EE00",
    "EL30", "ES30", "FI1B", "FR10", "HR05", "HU11", "IE06", "ITI4",
    "LT01", "LU00", "LV00", "MT00", "NL32", "PL91", "PT1A", "RO32",
    "SE11", "SI04", "SK01"
  ),
  outermost_regions = c(
    "ES70", "FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "PT20", "PT30"
  ),
  nuts2024_territorial_change_regions = c(
    "ITH5", "ITI3", "NL35", "NL36", "PT19", "PT1A", "PT1B", "PT1C",
    "PT1D"
  ),
  identity_tolerance = 0.005,
  rank_cutoffs = c(0.25, 0.75),
  bootstrap_repetitions = 1999L,
  random_seed = 29061989L,
  expected_event_regions = 244L,
  expected_event_countries = 27L,
  expected_legacy_regions = 243L,
  expected_balanced_regions = 232L,
  expected_balanced_countries = 26L,
  frozen_dir = file.path("data", "frozen_244"),
  total_input = "total_panel_frozen_244.csv",
  sector_input = "sector_panel_frozen_244.csv",
  sector_cell_input = "sector_cells_frozen_244.csv",
  sector_summary_reference = "sector_summary_reference_244.csv",
  minimum_size_reference = "minimum_size_reference_244.csv",
  input_manifest = "input_manifest_244.csv",
  output_dir = file.path("output", "release_244")
)

periods <- data.table(
  period = c("acute", "adaptation", "pandemic_net"),
  start = c(2019L, 2020L, 2019L),
  end = c(2020L, 2021L, 2021L),
  label = c(
    "Acute pandemic shock: 2019-2020",
    "Pandemic adaptation: 2020-2021",
    "Pandemic net change: 2019-2021"
  )
)

sector_labels <- c(
  "A" = "Agriculture, forestry and fishing",
  "B-E" = "Industry excluding construction",
  "F" = "Construction",
  "G-I" = "Trade, transport, accommodation and food",
  "J" = "Information and communication",
  "K" = "Financial and insurance activities",
  "L" = "Real estate activities",
  "M_N" = "Professional and administrative services",
  "O-Q" = "Public administration, education and health",
  "R-U" = "Arts, recreation and other services"
)

country_labels <- c(
  "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria",
  "CY" = "Cyprus", "CZ" = "Czechia", "DE" = "Germany",
  "DK" = "Denmark", "EE" = "Estonia", "EL" = "Greece",
  "ES" = "Spain", "FI" = "Finland", "FR" = "France",
  "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland",
  "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg",
  "LV" = "Latvia", "MT" = "Malta", "NL" = "Netherlands",
  "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania",
  "SE" = "Sweden", "SI" = "Slovenia", "SK" = "Slovakia"
)

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(config$random_seed)
run_started_utc <- format(
  Sys.time(),
  tz = "UTC",
  format = "%Y-%m-%dT%H:%M:%SZ"
)

sha256_file <- function(path) {
  if (nzchar(Sys.which("sha256sum"))) {
    line <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  } else if (nzchar(Sys.which("shasum"))) {
    line <- system2("shasum", c("-a", "256", path), stdout = TRUE, stderr = TRUE)
  } else {
    stop("Neither sha256sum nor shasum is available for provenance hashing.")
  }
  strsplit(line[1L], "[[:space:]]+")[[1L]][1L]
}

is_nuts2_geography <- function(x) {
  grepl("^[A-Z]{2}[A-Z0-9]{2}$", x) &
    !grepl("ZZ$", x)
}

input_paths <- file.path(
  config$frozen_dir,
  c(
    config$total_input,
    config$sector_input,
    config$sector_cell_input,
    config$sector_summary_reference,
    config$minimum_size_reference,
    config$input_manifest
  )
)
if (!all(file.exists(input_paths))) {
  stop(
    "Frozen replication inputs are missing: ",
    paste(input_paths[!file.exists(input_paths)], collapse = ", ")
  )
}

total_input <- fread(input_paths[1L], na.strings = c("", "NA"))
sector_input <- fread(input_paths[2L], na.strings = c("", "NA"))
sector_cell_input <- fread(input_paths[3L], na.strings = c("", "NA"))
sector_summary_reference <- fread(input_paths[4L], na.strings = c("", "NA"))
minimum_size_reference <- fread(input_paths[5L], na.strings = c("", "NA"))
input_manifest <- fread(input_paths[6L], na.strings = c("", "NA"))

required_columns <- c(
  "geo", "country", "year", "wstatus", "nace_r2",
  "hours", "persons", "hours_flag", "persons_flag"
)
for (object_name in c("total_input", "sector_input")) {
  object <- get(object_name)
  missing_columns <- setdiff(required_columns, names(object))
  if (length(missing_columns) > 0L) {
    stop(
      object_name, " is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

required_sector_cell_columns <- c(
  "sample", "period", "geo", "country", "nace_r2", "SAL", "SELF", "D"
)
missing_sector_cell_columns <- setdiff(
  required_sector_cell_columns,
  names(sector_cell_input)
)
if (length(missing_sector_cell_columns) > 0L) {
  stop(
    "sector_cell_input is missing columns: ",
    paste(missing_sector_cell_columns, collapse = ", ")
  )
}

required_sector_summary_columns <- c(
  "nace_r2", "region_sector_pairs", "cell_median_D_logpp",
  "cell_share_positive_pct", "countries", "country_median_D_logpp",
  "country_median_stability_low_logpp",
  "country_median_stability_high_logpp", "countries_positive",
  "eu_aggregate_D_logpp", "period", "sector_name"
)
if (!all(required_sector_summary_columns %in% names(sector_summary_reference))) {
  stop("sector_summary_reference does not have the validated release schema.")
}

required_minimum_size_columns <- c(
  "period", "minimum_self_employed_thousand", "region_sector_pairs",
  "median_D_logpp", "share_positive_pct", "countries_positive",
  "sectors_positive"
)
if (!all(required_minimum_size_columns %in% names(minimum_size_reference))) {
  stop("minimum_size_reference does not have the validated release schema.")
}

normalise_panel <- function(x) {
  x <- copy(x)
  x[, `:=`(
    geo = as.character(geo),
    country = as.character(country),
    year = as.integer(year),
    wstatus = as.character(wstatus),
    nace_r2 = as.character(nace_r2),
    hours_flag = fifelse(is.na(hours_flag), "", as.character(hours_flag)),
    persons_flag = fifelse(is.na(persons_flag), "", as.character(persons_flag))
  )]
  x[
    country %in% config$eu27 &
      is_nuts2_geography(geo) &
      year %in% config$years &
      wstatus %in% config$statuses &
      is.finite(hours) & is.finite(persons) &
      hours > 0 & persons > 0
  ]
}

total_all <- normalise_panel(total_input)[nace_r2 == "TOTAL"]
sector_all <- normalise_panel(sector_input)[
  nace_r2 %in% config$sectors & wstatus %in% config$comparison_statuses
]

if (anyDuplicated(total_all, by = c("geo", "year", "wstatus", "nace_r2"))) {
  stop("Duplicate total-economy keys found in the frozen input.")
}
if (anyDuplicated(sector_all, by = c(
  "geo", "year", "wstatus", "nace_r2"
))) {
  stop("Duplicate sector keys found in the frozen input.")
}

eligible_geographies <- function(dt, years, statuses) {
  coverage <- dt[
    year %in% years & wstatus %in% statuses,
    .(years_observed = uniqueN(year)),
    by = .(geo, wstatus)
  ]
  coverage[
    , .(
      statuses_observed = uniqueN(wstatus),
      complete = all(years_observed == length(years))
    ),
    by = geo
  ][
    statuses_observed == length(statuses) & complete,
    geo
  ]
}

event_geographies <- eligible_geographies(
  total_all,
  config$event_years,
  "EMP"
)
event_status_geographies <- eligible_geographies(
  total_all,
  config$event_years,
  config$statuses
)
legacy_geographies <- eligible_geographies(
  total_all,
  config$legacy_years,
  config$statuses
)
balanced_historical_geographies <- eligible_geographies(
  total_all,
  config$balanced_years,
  "EMP"
)

if (length(event_geographies) != config$expected_event_regions) {
  stop("Expected 244 event regions; found ", length(event_geographies), ".")
}
if (!setequal(event_geographies, event_status_geographies)) {
  stop("EMP, SAL and SELF event coverage is not complete for all 244 regions.")
}
if (uniqueN(total_all[geo %in% event_geographies, country]) !=
    config$expected_event_countries) {
  stop("The event universe does not contain all 27 EU countries.")
}
if (length(legacy_geographies) != config$expected_legacy_regions) {
  stop("Expected 243 legacy regions; found ", length(legacy_geographies), ".")
}
if (length(balanced_historical_geographies) !=
    config$expected_balanced_regions) {
  stop(
    "Expected 232 balanced historical regions; found ",
    length(balanced_historical_geographies), "."
  )
}
if (uniqueN(total_all[geo %in% balanced_historical_geographies, country]) !=
    config$expected_balanced_countries) {
  stop("The balanced historical panel must contain 26 countries.")
}

sector_cells <- copy(sector_cell_input)[
  sample == "S2_event_all_statuses_2019_2021" &
    geo %in% event_geographies &
    period %in% periods$period &
    nace_r2 %in% config$sectors
]
sector_cells[, `:=`(
  geo = as.character(geo),
  country = as.character(country),
  nace_r2 = as.character(nace_r2),
  SAL = as.numeric(SAL),
  SELF = as.numeric(SELF),
  D = as.numeric(D)
)]
if (anyDuplicated(sector_cells, by = c("period", "geo", "nace_r2"))) {
  stop("Duplicate period-region-sector keys found in sector_cell_input.")
}
if (max(abs(sector_cells$D - (sector_cells$SELF - sector_cells$SAL))) > 1e-12) {
  stop("The frozen sector-cell identity D = SELF - SAL does not hold.")
}
expected_sector_pairs <- data.table(
  period = c("acute", "adaptation", "pandemic_net"),
  expected_pairs = c(2071L, 2070L, 2070L)
)
observed_sector_pairs <- sector_cells[, .(observed_pairs = .N), by = period]
sector_pair_gate <- merge(
  expected_sector_pairs,
  observed_sector_pairs,
  by = "period",
  all = TRUE
)
if (
  nrow(sector_pair_gate) != 3L ||
    any(sector_pair_gate$expected_pairs != sector_pair_gate$observed_pairs)
) {
  stop("The validated sector-cell counts are not 2071/2070/2070.")
}

panel <- rbindlist(list(
  total_all[geo %in% event_geographies],
  sector_all[geo %in% event_geographies]
), use.names = TRUE, fill = TRUE)
panel[, log_inverse_average_hours := log(persons) - log(hours)]

manifest_value <- function(field_name) {
  out <- input_manifest[field == field_name, value]
  if (length(out) != 1L) {
    stop("Manifest field missing or duplicated: ", field_name)
  }
  as.character(out)
}

replication_script_sha256 <- sha256_file("replication_244.R")

hours_retrieval_date <- manifest_value("retrieval_date")
persons_retrieval_date <- manifest_value("retrieval_date")

input_hash_gate <- data.table(
  path = input_paths[1:5],
  field = c(
    "total_panel_sha256",
    "sector_panel_sha256",
    "sector_cells_sha256",
    "sector_summary_reference_sha256",
    "minimum_size_reference_sha256"
  )
)
input_hash_gate[, `:=`(
  expected = vapply(field, manifest_value, character(1L)),
  observed = vapply(path, sha256_file, character(1L))
)]
if (any(input_hash_gate$expected != input_hash_gate$observed)) {
  failed <- input_hash_gate[expected != observed, basename(path)]
  stop("Frozen-input SHA-256 verification failed for: ", paste(failed, collapse = ", "))
}

check_identity <- function(dt, variable, tolerance) {
  wide <- dcast(
    dt[nace_r2 == "TOTAL"],
    geo + country + year ~ wstatus,
    value.var = variable
  )
  wide[, relative_error := abs(EMP - SAL - SELF) / EMP]

  if (max(wide$relative_error, na.rm = TRUE) > tolerance) {
    stop(
      "The accounting identity EMP = SAL + SELF fails for ", variable,
      " beyond the configured tolerance."
    )
  }
  invisible(wide)
}

persons_identity_audit <- check_identity(
  panel,
  "persons",
  config$identity_tolerance
)
hours_identity_audit <- check_identity(
  panel,
  "hours",
  config$identity_tolerance
)
event_persons_identity_audit <- persons_identity_audit[
  year %in% config$event_years
]
event_hours_identity_audit <- hours_identity_audit[
  year %in% config$event_years
]

make_change <- function(dt, start_year, end_year, groups) {
  selected <- dt[year %in% c(start_year, end_year)]
  cast_formula <- as.formula(
    paste(paste(groups, collapse = " + "), "~ year")
  )

  wide <- dcast(
    selected,
    formula = cast_formula,
    value.var = c("hours", "persons")
  )

  required <- c(
    paste0("hours_", c(start_year, end_year)),
    paste0("persons_", c(start_year, end_year))
  )
  wide <- wide[complete.cases(wide[, ..required])]

  wide[, dlog_hours :=
    log(get(paste0("hours_", end_year))) -
    log(get(paste0("hours_", start_year)))]
  wide[, dlog_persons :=
    log(get(paste0("persons_", end_year))) -
    log(get(paste0("persons_", start_year)))]
  wide[, wedge := dlog_persons - dlog_hours]
  wide[, `:=`(start = start_year, end = end_year)]
  wide
}

pair_statuses <- function(changes, groups) {
  cast_formula <- as.formula(
    paste(paste(groups, collapse = " + "), "~ wstatus")
  )
  out <- dcast(
    changes[wstatus %in% config$comparison_statuses],
    formula = cast_formula,
    value.var = c("wedge", "dlog_hours", "dlog_persons")
  )
  out <- out[complete.cases(out[, .(wedge_SAL, wedge_SELF)])]
  out[, D := wedge_SELF - wedge_SAL]
  out
}

aggregate_values <- function(dt, groups) {
  dt[
    , .(hours = sum(hours), persons = sum(persons)),
    by = c(groups, "wstatus", "year")
  ]
}

cluster_bootstrap_median <- function(
    dt,
    value = "D",
    cluster = "geo",
    repetitions = config$bootstrap_repetitions,
    seed = config$random_seed) {

  set.seed(seed)
  values_by_cluster <- split(dt[[value]], dt[[cluster]])
  ids <- names(values_by_cluster)
  draws <- replicate(repetitions, {
    sampled_ids <- sample(ids, length(ids), replace = TRUE)
    sampled_values <- unlist(values_by_cluster[sampled_ids], use.names = FALSE)
    median(sampled_values, na.rm = TRUE)
  })
  unname(quantile(draws, c(0.025, 0.975), na.rm = TRUE))
}

clustered_mean_se <- function(dt, value = "D", cluster = "geo") {
  n <- nrow(dt)
  g <- uniqueN(dt[[cluster]])
  residual_mean <- mean(dt[[value]])
  scores <- tapply(
    dt[[value]] - residual_mean,
    dt[[cluster]],
    sum
  )

  sqrt((g / (g - 1)) * sum(scores^2) / n^2)
}

summarise_paired <- function(dt, cluster = "geo") {
  ci <- cluster_bootstrap_median(dt, cluster = cluster)
  country_medians <- dt[, .(country_median = median(D)), by = country]

  data.table(
    observations = nrow(dt),
    countries = uniqueN(dt$country),
    mean_D_logpp = 100 * mean(dt$D),
    mean_se_logpp = 100 * clustered_mean_se(dt, cluster = cluster),
    median_D_logpp = 100 * median(dt$D),
    median_ci_low_logpp = 100 * ci[1L],
    median_ci_high_logpp = 100 * ci[2L],
    share_positive_pct = 100 * mean(dt$D > 0),
    countries_positive = sum(country_medians$country_median > 0)
  )
}

rank_category <- function(x, cutoffs = config$rank_cutoffs) {
  percentile <- frank(x, ties.method = "average") / length(x)
  fifelse(
    percentile <= cutoffs[1L],
    "bottom quartile",
    fifelse(percentile > cutoffs[2L], "top quartile", "middle half")
  )
}

bootstrap_rank_correlation <- function(
    dt,
    cluster = "country",
    repetitions = config$bootstrap_repetitions,
    seed = config$random_seed) {

  set.seed(seed)
  values_by_cluster <- split(
    dt[, .(dlog_persons, dlog_hours)],
    dt[[cluster]]
  )
  ids <- names(values_by_cluster)
  draws <- replicate(repetitions, {
    sampled_ids <- sample(ids, length(ids), replace = TRUE)
    sampled <- rbindlist(values_by_cluster[sampled_ids], use.names = TRUE)
    cor(sampled$dlog_persons, sampled$dlog_hours, method = "spearman")
  })
  unname(quantile(draws, c(0.025, 0.975), na.rm = TRUE))
}

bootstrap_eu_disagreement <- function(
    dt,
    start_year,
    end_year,
    cluster = "country",
    repetitions = config$bootstrap_repetitions,
    seed = config$random_seed) {

  set.seed(seed)
  values_by_cluster <- split(dt, dt[[cluster]])
  ids <- names(values_by_cluster)
  draws <- replicate(repetitions, {
    sampled_ids <- sample(ids, length(ids), replace = TRUE)
    sampled <- rbindlist(values_by_cluster[sampled_ids], use.names = TRUE)
    eu_dlog_persons <- log(sum(sampled[[paste0("persons_", end_year)]])) -
      log(sum(sampled[[paste0("persons_", start_year)]]))
    eu_dlog_hours <- log(sum(sampled[[paste0("hours_", end_year)]])) -
      log(sum(sampled[[paste0("hours_", start_year)]]))
    mean(
      (sampled$dlog_persons >= eu_dlog_persons) !=
        (sampled$dlog_hours >= eu_dlog_hours)
    )
  })
  unname(quantile(draws, c(0.025, 0.975), na.rm = TRUE))
}

shapley_average_hours <- function(
    share_0,
    share_1,
    self_hours_0,
    self_hours_1,
    employee_hours_0,
    employee_hours_1) {

  factors <- c("share", "self_hours", "employee_hours")
  orders <- list(
    c("share", "self_hours", "employee_hours"),
    c("share", "employee_hours", "self_hours"),
    c("self_hours", "share", "employee_hours"),
    c("self_hours", "employee_hours", "share"),
    c("employee_hours", "share", "self_hours"),
    c("employee_hours", "self_hours", "share")
  )
  initial <- c(
    share = share_0,
    self_hours = self_hours_0,
    employee_hours = employee_hours_0
  )
  final <- c(
    share = share_1,
    self_hours = self_hours_1,
    employee_hours = employee_hours_1
  )
  evaluate <- function(x) {
    log(x["share"] * x["self_hours"] +
      (1 - x["share"]) * x["employee_hours"])
  }

  contribution <- setNames(numeric(length(factors)), factors)
  for (order in orders) {
    state <- initial
    old_value <- evaluate(state)
    for (factor in order) {
      state[factor] <- final[factor]
      new_value <- evaluate(state)
      contribution[factor] <- contribution[factor] +
        (new_value - old_value) / length(orders)
      old_value <- new_value
    }
  }

  data.table(
    composition = -unname(contribution["share"]),
    self_employed_hours = -unname(contribution["self_hours"]),
    employee_hours = -unname(contribution["employee_hours"])
  )
}

make_status_levels <- function(dt, start_year, end_year, groups) {
  make_level <- function(selected_year, suffix) {
    selected <- dt[year == selected_year]
    if (length(groups) == 0L) {
      if (uniqueN(selected$wstatus) != 2L) {
        stop("Both SAL and SELF are required for the aggregate decomposition.")
      }
      out <- selected[, .(
        hours_SAL = hours[wstatus == "SAL"],
        hours_SELF = hours[wstatus == "SELF"],
        persons_SAL = persons[wstatus == "SAL"],
        persons_SELF = persons[wstatus == "SELF"]
      )]
    } else {
      formula <- as.formula(paste(paste(groups, collapse = " + "), "~ wstatus"))
      out <- dcast(
        selected,
        formula = formula,
        value.var = c("hours", "persons")
      )
    }
    setnames(
      out,
      c("hours_SAL", "hours_SELF", "persons_SAL", "persons_SELF"),
      paste0(
        c("hours_SAL", "hours_SELF", "persons_SAL", "persons_SELF"),
        suffix
      )
    )
    out
  }

  level_0 <- make_level(start_year, "_0")
  level_1 <- make_level(end_year, "_1")
  if (length(groups) == 0L) {
    if (nrow(level_0) != 1L || nrow(level_1) != 1L) {
      stop("The aggregate status decomposition must contain one row per year.")
    }
    return(cbind(level_0, level_1))
  }
  merge(level_0, level_1, by = groups, all = FALSE)
}

decompose_status_hours <- function(levels) {
  levels <- copy(levels)
  levels[, `:=`(
    share_0 = persons_SELF_0 / (persons_SAL_0 + persons_SELF_0),
    share_1 = persons_SELF_1 / (persons_SAL_1 + persons_SELF_1),
    self_hours_0 = hours_SELF_0 / persons_SELF_0,
    self_hours_1 = hours_SELF_1 / persons_SELF_1,
    employee_hours_0 = hours_SAL_0 / persons_SAL_0,
    employee_hours_1 = hours_SAL_1 / persons_SAL_1
  )]

  components <- rbindlist(lapply(seq_len(nrow(levels)), function(i) {
    shapley_average_hours(
      levels$share_0[i],
      levels$share_1[i],
      levels$self_hours_0[i],
      levels$self_hours_1[i],
      levels$employee_hours_0[i],
      levels$employee_hours_1[i]
    )
  }))

  out <- cbind(levels, components)
  out[, total_wedge := composition + self_employed_hours + employee_hours]
  out[, self_employed_contribution_pct :=
    100 * self_employed_hours / total_wedge]
  out
}

total_panel <- panel[
  nace_r2 == "TOTAL" & wstatus %in% config$comparison_statuses
]
total_emp_panel <- panel[
  nace_r2 == "TOTAL" & wstatus == "EMP"
]
sector_panel <- panel[
  nace_r2 %in% config$sectors & wstatus %in% config$comparison_statuses
]

baseline_self <- sector_panel[
  year == config$event_reference & wstatus == "SELF",
  .(geo, country, nace_r2, self_2019_thousand = persons)
]

calculate_period_data <- function(period_row) {
  start_year <- period_row$start
  end_year <- period_row$end

  total_region <- pair_statuses(
    make_change(
      total_panel,
      start_year,
      end_year,
      c("geo", "country", "wstatus")
    ),
    c("geo", "country", "start", "end")
  )

  sector_region <- pair_statuses(
    make_change(
      sector_panel,
      start_year,
      end_year,
      c("geo", "country", "nace_r2", "wstatus")
    ),
    c("geo", "country", "nace_r2", "start", "end")
  )
  sector_region <- merge(
    sector_region,
    baseline_self,
    by = c("geo", "country", "nace_r2"),
    all.x = TRUE
  )

  country_total <- pair_statuses(
    make_change(
      aggregate_values(total_panel, "country"),
      start_year,
      end_year,
      c("country", "wstatus")
    ),
    c("country", "start", "end")
  )

  country_sector <- pair_statuses(
    make_change(
      aggregate_values(sector_panel, c("country", "nace_r2")),
      start_year,
      end_year,
      c("country", "nace_r2", "wstatus")
    ),
    c("country", "nace_r2", "start", "end")
  )

  eu_total <- pair_statuses(
    make_change(
      aggregate_values(total_panel, character(0L)),
      start_year,
      end_year,
      "wstatus"
    ),
    c("start", "end")
  )

  eu_sector <- pair_statuses(
    make_change(
      aggregate_values(sector_panel, "nace_r2"),
      start_year,
      end_year,
      c("nace_r2", "wstatus")
    ),
    c("nace_r2", "start", "end")
  )

  tag_period <- function(x) {
    x[, `:=`(
      period = period_row$period,
      period_label = period_row$label
    )]
    x
  }

  total_region <- tag_period(total_region)
  sector_region <- tag_period(sector_region)
  country_total <- tag_period(country_total)
  country_sector <- tag_period(country_sector)
  eu_total <- tag_period(eu_total)
  eu_sector <- tag_period(eu_sector)

  list(
    total_region = total_region,
    sector_region = sector_region,
    country_total = country_total,
    country_sector = country_sector,
    eu_total = eu_total,
    eu_sector = eu_sector
  )
}

period_data <- lapply(seq_len(nrow(periods)), function(i) {
  calculate_period_data(periods[i])
})

names(period_data) <- periods$period

audit_resilience <- function(
    start_year,
    end_year,
    period_name,
    inference = FALSE,
    seed = config$random_seed,
    analysis_panel = total_emp_panel) {

  regional <- make_change(
    analysis_panel,
    start_year,
    end_year,
    c("geo", "country", "wstatus")
  )
  if (nrow(regional) == 0L) {
    stop("No pair-complete regional observations for ", start_year, "-", end_year)
  }
  pair_geographies <- unique(regional$geo)
  pair_panel <- analysis_panel[
    geo %in% pair_geographies & year %in% c(start_year, end_year)
  ]
  regional[, `:=`(
    rank_persons = frank(-dlog_persons, ties.method = "average"),
    rank_hours = frank(-dlog_hours, ties.method = "average"),
    category_persons = rank_category(dlog_persons),
    category_hours = rank_category(dlog_hours)
  )]
  regional[, rank_displacement := abs(rank_persons - rank_hours)]

  eu_reference <- make_change(
    aggregate_values(pair_panel, character(0L)),
    start_year,
    end_year,
    "wstatus"
  )
  country_reference <- make_change(
    aggregate_values(pair_panel, "country"),
    start_year,
    end_year,
    c("country", "wstatus")
  )[, .(
    country,
    country_dlog_persons = dlog_persons,
    country_dlog_hours = dlog_hours
  )]

  regional <- merge(regional, country_reference, by = "country", all.x = TRUE)
  regional[, baseline_persons := get(paste0("persons_", start_year))]
  regional[, `:=`(
    eu_class_persons = dlog_persons >= eu_reference$dlog_persons,
    eu_class_hours = dlog_hours >= eu_reference$dlog_hours,
    country_class_persons = dlog_persons >= country_dlog_persons,
    country_class_hours = dlog_hours >= country_dlog_hours
  )]
  regional[, `:=`(
    eu_classification_disagreement = eu_class_persons != eu_class_hours,
    country_classification_disagreement =
      country_class_persons != country_class_hours,
    broad_category_disagreement = category_persons != category_hours,
    opposite_quartiles =
      (category_persons == "top quartile" &
        category_hours == "bottom quartile") |
      (category_persons == "bottom quartile" &
        category_hours == "top quartile")
  )]

  rho_ci <- c(NA_real_, NA_real_)
  eu_share_ci <- c(NA_real_, NA_real_)
  if (inference) {
    rho_ci <- bootstrap_rank_correlation(regional, seed = seed)
    eu_share_ci <- bootstrap_eu_disagreement(
      regional,
      start_year = start_year,
      end_year = end_year,
      seed = seed + 1L
    )
  }

  summary <- data.table(
    period = period_name,
    start = start_year,
    end = end_year,
    regions = nrow(regional),
    countries = uniqueN(regional$country),
    spearman_rho = cor(
      regional$dlog_persons,
      regional$dlog_hours,
      method = "spearman"
    ),
    spearman_ci_low = rho_ci[1L],
    spearman_ci_high = rho_ci[2L],
    kendall_tau = cor(
      regional$dlog_persons,
      regional$dlog_hours,
      method = "kendall"
    ),
    eu_classification_disagreement_pct =
      100 * mean(regional$eu_classification_disagreement),
    eu_classification_ci_low_pct = 100 * eu_share_ci[1L],
    eu_classification_ci_high_pct = 100 * eu_share_ci[2L],
    country_classification_disagreement_pct =
      100 * mean(regional$country_classification_disagreement),
    broad_category_disagreement_pct =
      100 * mean(regional$broad_category_disagreement),
    employment_weighted_eu_disagreement_pct = 100 * weighted.mean(
      regional$eu_classification_disagreement,
      regional$baseline_persons
    ),
    employment_weighted_broad_category_disagreement_pct = 100 * weighted.mean(
      regional$broad_category_disagreement,
      regional$baseline_persons
    ),
    employment_weighted_mean_rank_displacement = weighted.mean(
      regional$rank_displacement,
      regional$baseline_persons
    ),
    opposite_quartiles_pct = 100 * mean(regional$opposite_quartiles),
    median_rank_displacement = median(regional$rank_displacement),
    p90_rank_displacement = unname(
      quantile(regional$rank_displacement, 0.90)
    ),
    maximum_rank_displacement = max(regional$rank_displacement),
    country_relative_spearman_rho = cor(
      regional$dlog_persons - regional$country_dlog_persons,
      regional$dlog_hours - regional$country_dlog_hours,
      method = "spearman"
    ),
    median_measurement_gap_logpp = 100 * median(regional$wedge),
    eu_measurement_gap_logpp = 100 * (
      eu_reference$dlog_persons - eu_reference$dlog_hours
    )
  )

  list(summary = summary, regional = regional)
}

resilience_audits <- list(
  acute = audit_resilience(2019L, 2020L, "acute", TRUE, config$random_seed),
  adaptation = audit_resilience(
    2020L,
    2021L,
    "adaptation",
    TRUE,
    config$random_seed + 10L
  ),
  pandemic_net = audit_resilience(
    2019L,
    2021L,
    "pandemic_net",
    TRUE,
    config$random_seed + 20L
  )
)

resilience_table <- rbindlist(lapply(resilience_audits, `[[`, "summary"))
resilience_table <- merge(
  resilience_table,
  periods[, .(period, period_label = label)],
  by = "period",
  all.x = TRUE
)

historical_resilience <- rbindlist(lapply(2001:2023, function(end_year) {
  audit_resilience(
    end_year - 1L,
    end_year,
    paste0(end_year - 1L, "-", end_year)
  )$summary
}))

historical_end_year_break_audit <- rbindlist(lapply(
  2001:2020,
  function(end_year) {
    end_year_break_geographies <- total_emp_panel[
      year == end_year &
        (
          grepl("b", tolower(hours_flag), fixed = TRUE) |
            grepl("b", tolower(persons_flag), fixed = TRUE)
        ),
      unique(geo)
    ]
    result <- audit_resilience(
      end_year - 1L,
      end_year,
      paste0(end_year - 1L, "-", end_year),
      analysis_panel = total_emp_panel[
        !geo %in% end_year_break_geographies
      ]
    )$summary
    result[, end_year_break_regions_excluded :=
      historical_resilience[end == end_year, regions] - regions]
    result
  }
))

balanced_historical_panel <- total_emp_panel[
  geo %in% balanced_historical_geographies
]

historical_resilience_balanced <- rbindlist(lapply(
  2001:2020,
  function(end_year) {
    audit_resilience(
      end_year - 1L,
      end_year,
      paste0(end_year - 1L, "-", end_year),
      analysis_panel = balanced_historical_panel
    )$summary
  }
))

leave_one_country_out <- rbindlist(lapply(
  unique(resilience_audits$acute$regional$country),
  function(excluded_country) {
    retained <- resilience_audits$acute$regional[
      country != excluded_country
    ]
    data.table(
      country = excluded_country,
      spearman_rho = cor(
        retained$dlog_persons,
        retained$dlog_hours,
        method = "spearman"
      )
    )
  }
))

summarise_rank_restriction <- function(dt, label) {
  data.table(
    restriction = label,
    regions = nrow(dt),
    spearman_rho = cor(dt$dlog_persons, dt$dlog_hours, method = "spearman"),
    broad_category_disagreement_pct = 100 * mean(
      rank_category(dt$dlog_persons) != rank_category(dt$dlog_hours)
    )
  )
}

multi_region_countries <- resilience_audits$acute$regional[
  , .N,
  by = country
][N > 1L, country]

robustness_items <- list(
  summarise_rank_restriction(
    resilience_audits$acute$regional,
    "Main 2019-2021 event universe"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[
      geo %in% legacy_geographies
    ],
    "Former common 2008-2009/2019-2023 sample"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[
      country %in% multi_region_countries
    ],
    "Countries with at least two NUTS-2 regions"
  ),
  summarise_rank_restriction(
    audit_resilience(
      2019L,
      2020L,
      "acute_balanced",
      analysis_panel = balanced_historical_panel
    )$regional,
    "Strictly balanced 2000-2021 panel"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[
      !geo %in% config$capital_regions
    ],
    "Excluding capital regions"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[
      !geo %in% config$outermost_regions
    ],
    "Excluding outermost regions"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[
      !geo %in% c(config$capital_regions, config$outermost_regions)
    ],
    "Excluding capital and outermost regions"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[country != "HR"],
    "Excluding Croatia's regional 2020 break"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[
      !geo %in% config$nuts2024_territorial_change_regions
    ],
    "Excluding nine NUTS-2024-affected regions"
  ),
  summarise_rank_restriction(
    resilience_audits$acute$regional[
      !geo %in% config$nuts2024_territorial_change_regions & country != "HR"
    ],
    "Excluding NUTS-2024-affected regions and Croatia's break"
  )
)

event_flags <- unique(total_emp_panel[
  year %in% c(2019L, 2020L),
  .(geo, year, hours_flag, persons_flag)
])
event_flags[, `:=`(
  hours_flagged = nzchar(hours_flag),
  persons_flagged = nzchar(persons_flag),
  hours_break = grepl("b", tolower(hours_flag), fixed = TRUE),
  persons_break = grepl("b", tolower(persons_flag), fixed = TRUE),
  hours_provisional = grepl("p", tolower(hours_flag), fixed = TRUE),
  persons_provisional = grepl("p", tolower(persons_flag), fixed = TRUE)
)]
break_geographies <- event_flags[
  hours_break | persons_break,
  unique(geo)
]
provisional_geographies <- event_flags[
  hours_provisional | persons_provisional,
  unique(geo)
]
any_flag_geographies <- event_flags[
  hours_flagged | persons_flagged,
  unique(geo)
]
robustness_items <- append(
  robustness_items,
  list(
    summarise_rank_restriction(
      resilience_audits$acute$regional[!geo %in% break_geographies],
      "Conservative endpoint b-flag exclusion (Poland and Croatia)"
    ),
    summarise_rank_restriction(
      resilience_audits$acute$regional[
        !geo %in% provisional_geographies
      ],
      "Excluding provisional Eurostat observations, 2019-2020"
    ),
    summarise_rank_restriction(
      resilience_audits$acute$regional[!geo %in% any_flag_geographies],
      "Excluding any Eurostat flag (conservative stress test), 2019-2020"
    )
  )
)

robustness_items <- c(robustness_items, list(
  rbindlist(lapply(c(10, 15), function(limit_logpp) {
    summarise_rank_restriction(
      resilience_audits$acute$regional[
        abs(100 * wedge) <= limit_logpp
      ],
      paste0("Absolute persons-hours gap <= ", limit_logpp, " log pp")
    )
  })),
  data.table(
    restriction = "Leave-one-country-out minimum",
    regions = NA_integer_,
    spearman_rho = min(leave_one_country_out$spearman_rho),
    broad_category_disagreement_pct = NA_real_
  ),
  data.table(
    restriction = "Leave-one-country-out maximum",
    regions = NA_integer_,
    spearman_rho = max(leave_one_country_out$spearman_rho),
    broad_category_disagreement_pct = NA_real_
  )
))

acute_robustness <- rbindlist(robustness_items, fill = TRUE)

regional_status_decomposition <- decompose_status_hours(
  make_status_levels(total_panel, 2019L, 2020L, c("geo", "country"))
)
country_status_decomposition <- decompose_status_hours(
  make_status_levels(
    aggregate_values(total_panel, "country"),
    2019L,
    2020L,
    "country"
  )
)
eu_status_panel <- aggregate_values(total_panel, character(0L))
eu_status_panel[, geography := "EU27"]
eu_status_decomposition <- decompose_status_hours(
  make_status_levels(
    eu_status_panel,
    2019L,
    2020L,
    "geography"
  )
)

status_decomposition_table <- rbindlist(list(
  eu_status_decomposition[, .(
    level = "EU aggregate",
    geography = "EU27",
    baseline_self_employment_pct = 100 * share_0,
    total_measurement_gap_logpp = 100 * total_wedge,
    composition_logpp = 100 * composition,
    self_employed_hours_logpp = 100 * self_employed_hours,
    employee_hours_logpp = 100 * employee_hours,
    self_employed_contribution_pct
  )],
  country_status_decomposition[, .(
    level = "Country aggregate",
    geography = country,
    baseline_self_employment_pct = 100 * share_0,
    total_measurement_gap_logpp = 100 * total_wedge,
    composition_logpp = 100 * composition,
    self_employed_hours_logpp = 100 * self_employed_hours,
    employee_hours_logpp = 100 * employee_hours,
    self_employed_contribution_pct
  )]
), use.names = TRUE)

main_dynamics <- rbindlist(lapply(names(period_data), function(period_name) {
  item <- period_data[[period_name]]

  total_summary <- summarise_paired(item$total_region)
  total_summary[, `:=`(
    period = period_name,
    analysis_level = "NUTS-2 total economy"
  )]

  sector_summary <- summarise_paired(
    sector_cells[period == period_name]
  )
  sector_summary[, `:=`(
    period = period_name,
    analysis_level = "NUTS-2 region-sector"
  )]

  eu_summary <- data.table(
    observations = 1L,
    countries = 27L,
    mean_D_logpp = 100 * item$eu_total$D,
    mean_se_logpp = NA_real_,
    median_D_logpp = 100 * item$eu_total$D,
    median_ci_low_logpp = NA_real_,
    median_ci_high_logpp = NA_real_,
    share_positive_pct = NA_real_,
    countries_positive = NA_integer_,
    period = period_name,
    analysis_level = "EU aggregate"
  )

  rbindlist(list(total_summary, sector_summary, eu_summary), fill = TRUE)
}))

main_dynamics <- merge(
  main_dynamics,
  periods[, .(period, period_label = label)],
  by = "period",
  all.x = TRUE
)

country_table <- rbindlist(lapply(names(period_data), function(period_name) {
  x <- copy(period_data[[period_name]]$country_total)
  x[, .(
    country,
    country_name = unname(country_labels[country]),
    period = period_name,
    D_logpp = 100 * D,
    employee_hours_pct = 100 * expm1(dlog_hours_SAL),
    self_employed_hours_pct = 100 * expm1(dlog_hours_SELF),
    employee_persons_pct = 100 * expm1(dlog_persons_SAL),
    self_employed_persons_pct = 100 * expm1(dlog_persons_SELF)
  )]
}))

sector_cell_check <- sector_cells[, .(
  region_sector_pairs_check = .N,
  cell_median_D_logpp_check = 100 * median(D),
  cell_share_positive_pct_check = 100 * mean(D > 0)
), by = .(period, nace_r2)]

sector_table <- merge(
  copy(sector_summary_reference),
  sector_cell_check,
  by = c("period", "nace_r2"),
  all.x = TRUE
)
if (
  any(sector_table$region_sector_pairs !=
    sector_table$region_sector_pairs_check) ||
    max(abs(
      sector_table$cell_median_D_logpp -
        sector_table$cell_median_D_logpp_check
    )) > 1e-10 ||
    max(abs(
      sector_table$cell_share_positive_pct -
        sector_table$cell_share_positive_pct_check
    )) > 1e-10
) {
  stop("The frozen sector cells do not reproduce the validated sector summary.")
}
sector_table[, c(
  "region_sector_pairs_check",
  "cell_median_D_logpp_check",
  "cell_share_positive_pct_check"
) := NULL]
setnames(
  sector_table,
  c(
    "country_median_stability_low_logpp",
    "country_median_stability_high_logpp"
  ),
  c("country_median_ci_low_logpp", "country_median_ci_high_logpp")
)

minimum_size_table <- copy(minimum_size_reference)

sample_table <- data.table(
  item = c(
    "Eurostat retrieval date",
    "Complete EU-27 NUTS-2 event regions",
    "EU countries",
    "Acute total-economy region pairs",
    "Acute region-sector pairs",
    "Maximum EMP = SAL + SELF relative error, persons (%)",
    "Maximum EMP = SAL + SELF relative error, hours (%)",
    "First analysis year",
    "Last primary event year"
  ),
  value = c(
    paste(hours_retrieval_date, persons_retrieval_date, sep = " / "),
    as.character(uniqueN(panel$geo)),
    as.character(uniqueN(panel$country)),
    as.character(nrow(period_data$acute$total_region)),
    as.character(nrow(sector_cells[period == "acute"])),
    as.character(
      100 * max(event_persons_identity_audit$relative_error, na.rm = TRUE)
    ),
    as.character(
      100 * max(event_hours_identity_audit$relative_error, na.rm = TRUE)
    ),
    as.character(min(config$years)),
    "2021"
  )
)

assert_close <- function(observed, expected, tolerance, label) {
  if (!is.finite(observed) || abs(observed - expected) > tolerance) {
    stop(
      label, " failed: observed=", format(observed, digits = 16L),
      ", expected=", format(expected, digits = 16L)
    )
  }
}

acute_acceptance <- resilience_table[period == "acute"]
if (nrow(acute_acceptance) != 1L || acute_acceptance$regions != 244L) {
  stop("Acute acceptance gate failed: expected one N=244 row.")
}
assert_close(
  acute_acceptance$spearman_rho,
  0.5437484769256822,
  1e-12,
  "Acute Spearman rho"
)
assert_close(
  acute_acceptance$kendall_tau,
  0.3889900829791540,
  1e-12,
  "Acute Kendall tau"
)
assert_close(
  acute_acceptance$broad_category_disagreement_pct,
  39.34426229508197,
  1e-10,
  "Acute broad-category disagreement"
)
assert_close(
  acute_acceptance$eu_classification_disagreement_pct,
  29.508196721311474,
  1e-10,
  "Acute EU-benchmark disagreement"
)
assert_close(
  acute_acceptance$eu_measurement_gap_logpp,
  5.698846230129284,
  1e-10,
  "Acute EU measurement gap"
)
if (
  acute_acceptance$median_rank_displacement != 33.5 ||
    acute_acceptance$p90_rank_displacement != 110 ||
    acute_acceptance$maximum_rank_displacement != 222
) {
  stop("Acute rank-displacement acceptance gate failed.")
}

assert_followup_row <- function(end_year, rho, broad_pct, median_rank) {
  row <- historical_resilience[end == end_year]
  label <- paste0(end_year - 1L, "-", end_year)
  if (nrow(row) != 1L || row$regions != 244L || row$countries != 27L) {
    stop("Later observed comparison acceptance gate failed for: ", label)
  }
  assert_close(row$spearman_rho, rho, 1e-12, paste(label, "rho"))
  assert_close(
    row$broad_category_disagreement_pct,
    broad_pct,
    1e-10,
    paste(label, "broad disagreement")
  )
  assert_close(
    row$median_rank_displacement,
    median_rank,
    1e-12,
    paste(label, "median rank displacement")
  )
  row
}

followup_acceptance_2021 <- assert_followup_row(
  2021L,
  0.6439843211115653,
  36.0655737704918,
  28
)
followup_acceptance_2022 <- assert_followup_row(
  2022L,
  0.7125906100144975,
  34.01639344262295,
  30
)
followup_acceptance_2023 <- assert_followup_row(
  2023L,
  0.7757817972748784,
  33.19672131147541,
  27
)

eu_decomposition_acceptance <- status_decomposition_table[
  level == "EU aggregate" & geography == "EU27"
]
if (nrow(eu_decomposition_acceptance) != 1L) {
  stop("EU status-decomposition acceptance gate failed.")
}
assert_close(
  eu_decomposition_acceptance$baseline_self_employment_pct,
  14.651727053997007,
  1e-10,
  "EU baseline self-employment share"
)
assert_close(
  eu_decomposition_acceptance$total_measurement_gap_logpp,
  5.698853544710314,
  1e-10,
  "EU Shapley total gap"
)
assert_close(
  eu_decomposition_acceptance$composition_logpp,
  -0.021322191036077243,
  1e-10,
  "EU Shapley composition component"
)
assert_close(
  eu_decomposition_acceptance$self_employed_hours_logpp,
  1.583433580648474,
  1e-10,
  "EU Shapley self-employed-hours component"
)
assert_close(
  eu_decomposition_acceptance$employee_hours_logpp,
  4.1367421550979175,
  1e-10,
  "EU Shapley employee-hours component"
)
assert_close(
  eu_decomposition_acceptance$self_employed_contribution_pct,
  27.785124994451206,
  1e-10,
  "EU self-employed contribution share"
)

assert_robustness_row <- function(label, regions, rho, broad_pct) {
  row <- acute_robustness[restriction == label]
  if (nrow(row) != 1L || row$regions != regions) {
    stop("Robustness acceptance gate failed for: ", label)
  }
  assert_close(row$spearman_rho, rho, 1e-12, paste(label, "rho"))
  assert_close(
    row$broad_category_disagreement_pct,
    broad_pct,
    1e-10,
    paste(label, "broad disagreement")
  )
}

assert_robustness_row(
  "Excluding Croatia's regional 2020 break",
  240L,
  0.5463705967117484,
  41.25
)
assert_robustness_row(
  "Excluding nine NUTS-2024-affected regions",
  235L,
  0.5288422240099123,
  40.42553191489361
)
assert_robustness_row(
  "Excluding NUTS-2024-affected regions and Croatia's break",
  231L,
  0.5309867793376040,
  41.55844155844156
)

historical_break_pre <- historical_end_year_break_audit[end < 2020L]
historical_break_acute <- historical_end_year_break_audit[end == 2020L]
if (nrow(historical_break_acute) != 1L || historical_break_acute$regions != 240L) {
  stop("End-year break-flag acceptance gate failed for 2019-2020.")
}
assert_close(
  min(historical_break_pre$spearman_rho),
  0.7370457108162026,
  1e-12,
  "Pre-pandemic end-year-break minimum rho"
)
assert_close(
  max(historical_break_pre$broad_category_disagreement_pct),
  30.603448275862068,
  1e-10,
  "Pre-pandemic end-year-break maximum broad disagreement"
)
assert_close(
  max(historical_break_pre$median_rank_displacement),
  26.5,
  1e-12,
  "Pre-pandemic end-year-break maximum median displacement"
)
assert_close(
  max(historical_break_pre$p90_rank_displacement),
  90.8,
  1e-10,
  "Pre-pandemic end-year-break maximum p90 displacement"
)
assert_close(
  historical_break_acute$spearman_rho,
  0.5463705967117484,
  1e-12,
  "Acute end-year-break rho"
)
assert_close(
  historical_break_acute$broad_category_disagreement_pct,
  41.25,
  1e-10,
  "Acute end-year-break broad disagreement"
)
assert_close(
  historical_break_acute$median_rank_displacement,
  34,
  1e-12,
  "Acute end-year-break median displacement"
)
assert_close(
  historical_break_acute$p90_rank_displacement,
  109.1,
  1e-10,
  "Acute end-year-break p90 displacement"
)

sample_definitions <- list(
  list(
    sample = "Complete 2019-2021 event universe",
    rationale = paste(
      "Primary event sample; requires only observations used by the event",
      "analysis"
    ),
    geographies = event_geographies
  ),
  list(
    sample = "Former common 2008-2009/2019-2023 sample",
    rationale = paste(
      "Legacy restriction; additionally requires GFC and post-2021",
      "observations"
    ),
    geographies = legacy_geographies
  ),
  list(
    sample = "Strictly balanced 2000-2021 panel",
    rationale = paste(
      "Historical stress test with identical regions in every adjacent-year",
      "comparison"
    ),
    geographies = balanced_historical_geographies
  )
)

sample_sensitivity <- rbindlist(lapply(sample_definitions, function(item) {
  rbindlist(lapply(seq_len(nrow(periods)), function(i) {
    result <- audit_resilience(
      periods$start[i],
      periods$end[i],
      periods$period[i],
      analysis_panel = total_emp_panel[geo %in% item$geographies]
    )$summary
    result[, .(
      sample = item$sample,
      rationale = item$rationale,
      period,
      start,
      end,
      regions,
      countries,
      spearman_rho,
      kendall_tau,
      eu_disagreement_pct = eu_classification_disagreement_pct,
      broad_disagreement_pct = broad_category_disagreement_pct,
      median_rank_displacement,
      p90_rank_displacement,
      median_measurement_gap_logpp,
      eu_measurement_gap_logpp
    )]
  }))
}))

resilience_output <- copy(resilience_table)
setnames(
  resilience_output,
  c(
    "spearman_ci_low", "spearman_ci_high",
    "eu_classification_ci_low_pct", "eu_classification_ci_high_pct"
  ),
  c(
    "spearman_stability_low", "spearman_stability_high",
    "eu_classification_stability_low_pct",
    "eu_classification_stability_high_pct"
  )
)
resilience_output[, countries := NULL]

historical_output <- copy(historical_resilience)
historical_balanced_output <- copy(historical_resilience_balanced)
historical_break_output <- copy(historical_end_year_break_audit)
history_rename_from <- c(
  "eu_classification_disagreement_pct",
  "country_classification_disagreement_pct",
  "broad_category_disagreement_pct"
)
history_rename_to <- c(
  "eu_disagreement_pct",
  "country_disagreement_pct",
  "broad_disagreement_pct"
)
setnames(historical_output, history_rename_from, history_rename_to)
setnames(historical_balanced_output, history_rename_from, history_rename_to)
setnames(historical_break_output, history_rename_from, history_rename_to)
history_columns <- c(
  "period", "start", "end", "regions", "spearman_rho", "kendall_tau",
  "eu_disagreement_pct", "country_disagreement_pct",
  "broad_disagreement_pct", "opposite_quartiles_pct",
  "median_rank_displacement", "p90_rank_displacement",
  "maximum_rank_displacement", "country_relative_spearman_rho",
  "median_measurement_gap_logpp", "eu_measurement_gap_logpp"
)
historical_output <- historical_output[, ..history_columns]
historical_balanced_output <- historical_balanced_output[, ..history_columns]
historical_break_output <- historical_break_output[, c(
  history_columns,
  "end_year_break_regions_excluded"
), with = FALSE]

main_dynamics_output <- copy(main_dynamics)
setnames(
  main_dynamics_output,
  c("median_ci_low_logpp", "median_ci_high_logpp"),
  c("median_stability_low_logpp", "median_stability_high_logpp")
)

sector_output <- copy(sector_table)
setnames(
  sector_output,
  c("country_median_ci_low_logpp", "country_median_ci_high_logpp"),
  c(
    "country_median_stability_low_logpp",
    "country_median_stability_high_logpp"
  )
)

write_csv <- function(x, filename) {
  fwrite(x, file.path(config$output_dir, filename), na = "")
}

write_csv(sample_table, "table_1_sample_244.csv")
write_csv(resilience_output, "table_2_resilience_agreement_244.csv")
write_csv(status_decomposition_table, "table_3_status_decomposition_244.csv")
write_csv(country_table, "table_4a_country_status_anatomy_244.csv")
write_csv(sector_output, "table_4b_sector_status_anatomy_244.csv")
write_csv(main_dynamics_output, "table_4c_status_contrasts_244.csv")
write_csv(historical_output, "table_A1_historical_resilience_audit_244.csv")
write_csv(
  historical_balanced_output,
  "table_A1b_balanced_historical_resilience_audit_244.csv"
)
write_csv(
  historical_break_output,
  "table_A1c_end_year_break_audit_244.csv"
)
write_csv(acute_robustness, "table_A2a_acute_robustness_244.csv")
write_csv(leave_one_country_out, "table_A2b_leave_one_country_out_244.csv")
write_csv(minimum_size_table, "table_A3_minimum_cell_size_244.csv")
write_csv(sample_sensitivity, "table_A4_sample_definition_244.csv")

acute_plot_data <- copy(resilience_audits$acute$regional)
acute_plot_data[, `:=`(
  sample = "acute",
  period = "acute",
  persons_change_pct = 100 * expm1(dlog_persons),
  hours_change_pct = 100 * expm1(dlog_hours),
  broad_category_disagreement_label = fifelse(
    broad_category_disagreement,
    "Different category",
    "Same category"
  )
)]
setnames(
  acute_plot_data,
  c(
    "eu_classification_disagreement",
    "country_classification_disagreement",
    "broad_category_disagreement"
  ),
  c("eu_disagreement", "country_disagreement", "broad_disagreement")
)
figure_1_columns <- c(
  "sample", "period", "geo", "country",
  "persons_2019", "persons_2020", "hours_2019", "hours_2020",
  "dlog_persons", "dlog_hours", "wedge",
  "country_dlog_persons", "country_dlog_hours",
  "rank_persons", "rank_hours", "rank_displacement",
  "category_persons", "category_hours",
  "eu_class_persons", "eu_class_hours",
  "country_class_persons", "country_class_hours",
  "eu_disagreement", "country_disagreement", "broad_disagreement",
  "opposite_quartiles", "persons_change_pct", "hours_change_pct",
  "broad_category_disagreement_label"
)
write_csv(
  acute_plot_data[, ..figure_1_columns],
  "figure_1_persons_hours_data_244.csv"
)

figure_1_labels <- acute_plot_data[
  opposite_quartiles == TRUE
][order(-rank_displacement)]
figure_1_labels <- head(figure_1_labels, 4L)

figure_1 <- ggplot(
  acute_plot_data,
  aes(
    x = persons_change_pct,
    y = hours_change_pct,
    colour = broad_category_disagreement_label
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "grey55",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_point(alpha = 0.78, size = 1.8) +
  geom_text(
    data = figure_1_labels,
    aes(label = geo),
    colour = "grey20",
    size = 2.4,
    nudge_y = 0.7,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c(
    "Different category" = "#B22222",
    "Same category" = "#4C78A8"
  )) +
  labs(
    x = "Change in employed persons, 2019-2020 (%)",
    y = "Change in hours worked, 2019-2020 (%)",
    colour = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    config$output_dir,
    "figure_1_persons_versus_hours.pdf"
  ),
  plot = figure_1,
  width = 6.8,
  height = 5.2,
  units = "in",
  bg = "white"
)

historical_plot_data <- rbindlist(list(
  historical_resilience[, .(
    end,
    statistic = "A. Rank agreement (Spearman rho)",
    value = spearman_rho
  )],
  historical_resilience[, .(
    end,
    statistic = "B. Broad-category disagreement (%)",
    value = broad_category_disagreement_pct
  )]
))
write_csv(historical_plot_data, "figure_2_historical_data_244.csv")

historical_plot_data[, comparison_phase := fcase(
  end < 2020L, "Pre-2020 adjacent-year comparisons",
  end == 2020L, "Acute shock: 2019-2020",
  default = "Later observed comparisons"
)]
if (nrow(historical_plot_data) != 46L) {
  stop("Figure 2 must contain two statistics for 23 adjacent-year comparisons.")
}
historical_plot_pre <- historical_plot_data[end <= 2020L]
historical_plot_followup <- historical_plot_data[end >= 2020L]

figure_2 <- ggplot(
  historical_plot_data,
  aes(x = end, y = value)
) +
  geom_vline(
    xintercept = 2020,
    colour = "#B22222",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_line(
    data = historical_plot_pre,
    colour = "#1F4E79",
    linewidth = 0.75
  ) +
  geom_line(
    data = historical_plot_followup,
    colour = "#6B7280",
    linetype = "dashed",
    linewidth = 0.75
  ) +
  geom_point(
    aes(colour = comparison_phase),
    size = 1.8
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Pre-2020 adjacent-year comparisons" = "#1F4E79",
      "Acute shock: 2019-2020" = "#B22222",
      "Later observed comparisons" = "#6B7280"
    )
  ) +
  scale_x_continuous(breaks = c(seq(2002, 2018, by = 2), 2020, 2023)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  facet_wrap(~statistic, ncol = 1, scales = "free_y") +
  labs(
    x = "End year of adjacent-year comparison",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(hjust = 0),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    config$output_dir,
    "figure_2_historical_measure_dependence.pdf"
  ),
  plot = figure_2,
  width = 7.0,
  height = 5.8,
  units = "in",
  bg = "white"
)

sector_plot_data <- sector_table[
  period %in% c("acute", "pandemic_net"),
  .(
    nace_r2,
    sector_name,
    period,
    country_median_D_logpp,
    country_median_ci_low_logpp,
    country_median_ci_high_logpp
  )
]
write_csv(
  sector_output[period %in% c("acute", "pandemic_net")],
  "figure_3_sector_data_244.csv"
)
sector_plot_data[, period := factor(
  period,
  levels = c("acute", "pandemic_net"),
  labels = c("2019-2020", "2019-2021")
)]
sector_order <- sector_plot_data[
  period == "2019-2020",
  nace_r2[order(country_median_D_logpp)]
]
sector_plot_data[, sector_name := factor(
  sector_name,
  levels = unname(sector_labels[sector_order])
)]

figure_3 <- ggplot(
  sector_plot_data,
  aes(
    x = country_median_D_logpp,
    y = sector_name,
    shape = period,
    colour = period
  )
) +
  geom_vline(xintercept = 0, colour = "grey65", linewidth = 0.4) +
  geom_errorbarh(
    aes(
      xmin = country_median_ci_low_logpp,
      xmax = country_median_ci_high_logpp
    ),
    height = 0,
    linewidth = 0.45,
    position = position_dodge(width = 0.35)
  ) +
  geom_point(size = 2.4, position = position_dodge(width = 0.35)) +
  scale_colour_manual(values = c("#B22222", "#1F4E79")) +
  scale_shape_manual(values = c(16, 17)) +
  labs(
    x = "Median country-sector gap (log percentage points)",
    y = NULL,
    colour = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    config$output_dir,
    "figure_3_sector_wedges.pdf"
  ),
  plot = figure_3,
  width = 7.2,
  height = 4.8,
  units = "in",
  bg = "white"
)

validation_report <- data.table(
  gate = c(
    "Frozen input hashes",
    "Event regions",
    "Event countries",
    "Legacy regions",
    "Balanced historical regions",
    "Balanced historical countries",
    "Sector cells: acute",
    "Sector cells: adaptation",
    "Sector cells: pandemic net",
    "Event identity maximum: persons (%)",
    "Event identity maximum: hours (%)",
    "Acute Spearman rho",
    "Acute Kendall tau",
    "Acute broad-category disagreement (%)",
    "Acute EU-benchmark disagreement (%)",
    "Acute median rank displacement",
    "Acute p90 rank displacement",
    "Acute maximum rank displacement",
    "EU measurement gap (log pp)",
    "EU Shapley total gap (log pp)",
    "Follow-up 2020-2021 Spearman rho",
    "Follow-up 2020-2021 broad disagreement (%)",
    "Follow-up 2021-2022 Spearman rho",
    "Follow-up 2021-2022 broad disagreement (%)",
    "Follow-up 2022-2023 Spearman rho",
    "Follow-up 2022-2023 broad disagreement (%)",
    "Figure 2 plotted rows",
    "Figure 3 plotted rows"
  ),
  status = "PASS",
  observed = c(
    "5/5",
    as.character(length(event_geographies)),
    as.character(uniqueN(total_all[geo %in% event_geographies, country])),
    as.character(length(legacy_geographies)),
    as.character(length(balanced_historical_geographies)),
    as.character(uniqueN(
      total_all[geo %in% balanced_historical_geographies, country]
    )),
    as.character(sector_cells[period == "acute", .N]),
    as.character(sector_cells[period == "adaptation", .N]),
    as.character(sector_cells[period == "pandemic_net", .N]),
    as.character(100 * max(
      event_persons_identity_audit$relative_error,
      na.rm = TRUE
    )),
    as.character(100 * max(
      event_hours_identity_audit$relative_error,
      na.rm = TRUE
    )),
    as.character(acute_acceptance$spearman_rho),
    as.character(acute_acceptance$kendall_tau),
    as.character(acute_acceptance$broad_category_disagreement_pct),
    as.character(acute_acceptance$eu_classification_disagreement_pct),
    as.character(acute_acceptance$median_rank_displacement),
    as.character(acute_acceptance$p90_rank_displacement),
    as.character(acute_acceptance$maximum_rank_displacement),
    as.character(acute_acceptance$eu_measurement_gap_logpp),
    as.character(eu_decomposition_acceptance$total_measurement_gap_logpp),
    as.character(followup_acceptance_2021$spearman_rho),
    as.character(followup_acceptance_2021$broad_category_disagreement_pct),
    as.character(followup_acceptance_2022$spearman_rho),
    as.character(followup_acceptance_2022$broad_category_disagreement_pct),
    as.character(followup_acceptance_2023$spearman_rho),
    as.character(followup_acceptance_2023$broad_category_disagreement_pct),
    as.character(nrow(historical_plot_data)),
    as.character(nrow(sector_plot_data))
  ),
  expected = c(
    "5/5", "244", "27", "243", "232", "26",
    "2071", "2070", "2070",
    "0.09990009990009778", "0.03169572107765452",
    "0.5437484769256822", "0.3889900829791540",
    "39.34426229508197", "29.508196721311474",
    "33.5", "110", "222", "5.698846230129284",
    "5.698853544710314",
    "0.6439843211115653", "36.0655737704918",
    "0.7125906100144975", "34.01639344262295",
    "0.7757817972748784", "33.19672131147541",
    "46", "20"
  )
)
write_csv(validation_report, "validation_report_244.csv")

metadata <- data.table(
  field = c(
    "release_status",
    "input_release_status",
    "run_started_utc",
    "replication_script_sha256",
    "hours_dataset",
    "persons_dataset",
    "retrieval_date",
    "hours_source_updated",
    "persons_source_updated",
    "hours_json_md5",
    "persons_json_md5",
    "hours_total_json_md5",
    "persons_total_json_md5",
    "hours_total_json_sha256",
    "persons_total_json_sha256",
    "total_panel_sha256",
    "sector_panel_sha256",
    "sector_cells_sha256",
    "sector_summary_reference_sha256",
    "minimum_size_reference_sha256",
    "sector_analysis_provenance",
    "nuts2024_territorial_change_regions",
    "event_regions",
    "countries",
    "first_year",
    "last_year",
    "event_reference",
    "bootstrap_repetitions",
    "random_seed",
    "eurostat_flag_audit",
    "extra_regio_excluded",
    "balanced_historical_regions",
    "historical_comparison_region_missing",
    "historical_missing_reason"
  ),
  value = c(
    "replication_candidate_self_checks_passed",
    manifest_value("release_status"),
    run_started_utc,
    replication_script_sha256,
    config$hours_dataset,
    config$persons_dataset,
    manifest_value("retrieval_date"),
    manifest_value("hours_source_updated"),
    manifest_value("persons_source_updated"),
    manifest_value("hours_json_md5"),
    manifest_value("persons_json_md5"),
    manifest_value("hours_total_json_md5"),
    manifest_value("persons_total_json_md5"),
    manifest_value("hours_total_json_sha256"),
    manifest_value("persons_total_json_sha256"),
    manifest_value("total_panel_sha256"),
    manifest_value("sector_panel_sha256"),
    manifest_value("sector_cells_sha256"),
    manifest_value("sector_summary_reference_sha256"),
    manifest_value("minimum_size_reference_sha256"),
    manifest_value("sector_analysis_provenance"),
    paste(config$nuts2024_territorial_change_regions, collapse = ";"),
    as.character(length(event_geographies)),
    as.character(uniqueN(total_all[geo %in% event_geographies, country])),
    as.character(min(config$years)),
    as.character(max(config$years)),
    as.character(config$event_reference),
    as.character(config$bootstrap_repetitions),
    as.character(config$random_seed),
    "TRUE",
    "TRUE",
    as.character(length(balanced_historical_geographies)),
    "FRY5",
    "Mayotte lacks the required 2008-2009 observations"
  )
)

write_csv(
  metadata,
  "data_vintage_metadata_244.csv"
)

capture.output(
  sessionInfo(),
  file = file.path(config$output_dir, "session_info_244.txt")
)

output_files <- list.files(
  config$output_dir,
  full.names = TRUE,
  recursive = FALSE
)
output_files <- output_files[
  basename(output_files) != "output_manifest_244.csv"
]
output_manifest <- data.table(
  file = basename(output_files),
  bytes = file.info(output_files)$size,
  sha256 = vapply(output_files, sha256_file, character(1L))
)
setorder(output_manifest, file)
write_csv(output_manifest, "output_manifest_244.csv")

message(
  "Replication completed with input status '",
  manifest_value("release_status"),
  "'. Paper tables and figures are in: ",
  config$output_dir
)
