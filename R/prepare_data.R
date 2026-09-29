# ==============================================================================
# Prepare dashboard-ready data for the CNMI Coverage Planning Dashboard
# ==============================================================================

required_packages <- c(
  "dplyr", "tidyr", "purrr", "stringr", "lubridate", "readxl",
  "readr", "janitor", "here", "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the following packages before preparing dashboard data:\n  ",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(readxl)
library(readr)
library(janitor)
library(here)
library(tibble)

source(here::here("R", "cnmi_functions.R"), local = TRUE)

# ==============================================================================
# Paths
# ==============================================================================

data_dir <- here::here("data")
output_dir <- here::here("data", "Analytic Code Output")

neighborhood_file <- here::here("data", "CNMI_Neighborhood_Coordinates.xlsx")
region_file <- here::here("data", "CNMI_County_Population_Area_GIS.xlsx")

patient_file <- newest_matching_file(
  output_dir,
  "^Dataset 2_CNMI_2-83 Mos_DeID_Full_Dataset_.*\\.xlsx$"
)

dashboard_files <- c(
  community = here::here("data", "community_summary.csv"),
  vaccine_needs = here::here("data", "community_vaccine_needs.csv"),
  product_needs = here::here("data", "community_product_needs.csv"),
  patient = here::here("data", "patient_dashboard.csv"),
  quality = here::here("data", "qa_cnmi_geography.csv")
)

required_files <- c(patient_file, neighborhood_file, region_file)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required file(s) not found:\n  - ",
      paste(missing_files, collapse = "\n  - ")
    ),
    call. = FALSE
  )
}

# ==============================================================================
# Helper functions
# ==============================================================================

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_quantile <- function(x, probability) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(quantile(x, probs = probability, na.rm = TRUE, names = FALSE))
}

percentile_100 <- function(x) {
  valid <- !is.na(x)
  out <- rep(NA_real_, length(x))

  if (sum(valid) == 1 || dplyr::n_distinct(x[valid]) == 1) {
    out[valid] <- 50
    return(out)
  }

  out[valid] <- 100 * (rank(x[valid], ties.method = "average") - 1) / (sum(valid) - 1)
  out
}

rescale_100 <- function(x) {
  valid <- !is.na(x)
  out <- rep(NA_real_, length(x))
  limits <- range(x[valid], na.rm = TRUE)

  if (!any(valid)) return(out)
  if (diff(limits) == 0) {
    out[valid] <- 50
  } else {
    out[valid] <- 100 * (x[valid] - limits[[1]]) / diff(limits)
  }

  out
}

# ==============================================================================
# Geography reference data
# ==============================================================================

neighborhood_lookup <- read_neighborhood_lookup(neighborhood_file)

region_lookup <- readxl::read_excel(
  region_file,
  sheet = "CNMI_County_Data"
) |>
  janitor::clean_names() |>
  mutate(
    region = standardize_cnmi_region(county_equivalent),
    population = as.numeric(total_population),
    area_km2 = as.numeric(land_area_sq_m) / 1e6,
    population_density_km2 = population / area_km2,
    region_latitude = as.numeric(latitude),
    region_longitude = as.numeric(longitude)
  ) |>
  filter(region %in% c("Saipan", "Rota", "Tinian")) |>
  select(
    region,
    population_year,
    population,
    area_km2,
    population_density_km2,
    region_latitude,
    region_longitude,
    source_url
  ) |>
  distinct(region, .keep_all = TRUE)

# ==============================================================================
# Patient data
# ==============================================================================

patient_raw <- readxl::read_excel(patient_file) |>
  select(-matches("rsv|nirsevimab|clesrovimab|beyfortus|enflonsia", ignore.case = TRUE))

patient_raw <- add_missing_columns(
  patient_raw,
  c(
    "patient_id", "region", "state", "county", "age_months", "agegroup",
    "age_group_label", "utd", "utd_no_mmr", "mmr_utd",
    "individual_risk_score", "days_since_last_vax", "latitude", "longitude",
    "onreminder"
  ),
  NA
)

patient <- patient_raw |>
  mutate(
    patient_id = as.character(patient_id),
    region = coalesce(standardize_cnmi_region(region), standardize_cnmi_region(state), "Saipan"),
    county = clean_cnmi_label(county),
    county_key = normalize_cnmi_text(county),
    community_id = paste(normalize_cnmi_text(region), county_key, sep = "__"),
    age_months = as.numeric(age_months),
    agegroup = coalesce(as.integer(agegroup), cnmi_age_group(age_months)),
    age_group_label = coalesce(as.character(age_group_label), cnmi_age_label(age_months)),
    utd = as.integer(utd),
    utd_no_mmr = as.integer(utd_no_mmr),
    mmr_utd = as.integer(mmr_utd),
    individual_risk_score = as.numeric(individual_risk_score),
    days_since_last_vax = as.numeric(days_since_last_vax),
    months_since_last_vax = pmin(days_since_last_vax / 30.4375, 36),
    onreminder = coalesce(as.integer(onreminder), 0L),
    not_utd = case_when(
      is.na(utd) ~ NA_integer_,
      utd == 0 ~ 1L,
      TRUE ~ 0L
    ),
    not_utd_no_mmr = case_when(
      is.na(utd_no_mmr) ~ NA_integer_,
      utd_no_mmr == 0 ~ 1L,
      TRUE ~ 0L
    ),
    mmr_not_utd = case_when(
      is.na(mmr_utd) ~ NA_integer_,
      mmr_utd == 0 ~ 1L,
      TRUE ~ 0L
    ),
    high_individual_risk = as.integer(individual_risk_score >= 50)
  ) |>
  left_join(
    neighborhood_lookup |>
      select(
        county_key,
        coordinate_region = region_lookup,
        coordinate_latitude = latitude_lookup,
        coordinate_longitude = longitude_lookup
      ),
    by = "county_key"
  ) |>
  mutate(
    region = coalesce(region, coordinate_region),
    latitude = coalesce(as.numeric(latitude), coordinate_latitude),
    longitude = coalesce(as.numeric(longitude), coordinate_longitude),
    community_id = paste(normalize_cnmi_text(region), county_key, sep = "__")
  ) |>
  select(-coordinate_region, -coordinate_latitude, -coordinate_longitude) |>
  left_join(region_lookup, by = "region")

# ==============================================================================
# Community summary and priority score
# ==============================================================================

community_summary <- patient |>
  group_by(
    community_id,
    region,
    county,
    county_key,
    population_year,
    population,
    area_km2,
    population_density_km2,
    region_latitude,
    region_longitude
  ) |>
  summarise(
    child_population = n_distinct(patient_id),
    children_not_utd = sum(not_utd == 1, na.rm = TRUE),
    proportion_not_utd = 100 * safe_mean(not_utd),
    utd_coverage = 100 * safe_mean(utd),
    utd_no_mmr_coverage = 100 * safe_mean(utd_no_mmr),
    mmr_eligible_n = sum(!is.na(mmr_utd)),
    mmr_not_utd_n = sum(mmr_not_utd == 1, na.rm = TRUE),
    mmr_coverage = 100 * safe_mean(mmr_utd),
    var_coverage = 100 * safe_mean(var_utd),
    hepa_coverage = 100 * safe_mean(hepa_utd),
    median_months_since_vax = safe_median(
      if_else(not_utd == 1, months_since_last_vax, NA_real_)
    ),
    mean_individual_risk = safe_mean(individual_risk_score),
    median_individual_risk = safe_median(individual_risk_score),
    p75_individual_risk = safe_quantile(individual_risk_score, 0.75),
    high_risk_n = sum(high_individual_risk == 1, na.rm = TRUE),
    high_risk_percent = 100 * safe_mean(high_individual_risk),
    latitude = safe_mean(latitude),
    longitude = safe_mean(longitude),
    .groups = "drop"
  ) |>
  mutate(
    latitude = coalesce(latitude, if_else(county == "Unknown", region_latitude, NA_real_)),
    longitude = coalesce(longitude, if_else(county == "Unknown", region_longitude, NA_real_)),
    # Rota and Tinian require inter-island access from the main population
    # center. This transparent proxy preserves the original score structure.
    island_access_component = if_else(region %in% c("Rota", "Tinian"), 100, 0),
    median_risk_component = pmin(pmax(median_individual_risk, 0), 100),
    p75_risk_component = pmin(pmax(p75_individual_risk, 0), 100),
    high_risk_percent_component = pmin(pmax(high_risk_percent, 0), 100),
    community_patient_risk_component =
      0.60 * median_risk_component +
      0.20 * p75_risk_component +
      0.20 * high_risk_percent_component,
    log_population_density = log1p(population_density_km2),
    density_percentile_component = percentile_100(log_population_density),
    low_density_component_for_score = coalesce(100 - density_percentile_component, 50),
    high_density_component_for_score = coalesce(density_percentile_component, 50),
    transmission_component =
      high_density_component_for_score * community_patient_risk_component / 100,
    burden_component = rescale_100(log1p(high_risk_n)),
    priority_score = round(
      0.90 * community_patient_risk_component +
        0.01 * island_access_component +
        0.01 * low_density_component_for_score +
        0.04 * transmission_component +
        0.04 * burden_component,
      1
    ),
    priority_rank = min_rank(desc(priority_score))
  )

priority_cutpoints <- quantile(
  community_summary$priority_score,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE,
  names = FALSE
)

community_summary <- community_summary |>
  mutate(
    priority_group = case_when(
      is.na(priority_score) ~ NA_character_,
      utd_coverage < 30 | mmr_coverage < 50 ~ "High",
      priority_score > priority_cutpoints[[2]] ~ "High",
      island_access_component == 100 & (utd_coverage < 50 | mmr_coverage < 50) ~ "Moderate",
      priority_score > priority_cutpoints[[1]] ~ "Moderate",
      TRUE ~ "Low"
    ),
    priority_group = factor(priority_group, levels = c("Low", "Moderate", "High"), ordered = TRUE),
    data_quality_flag = case_when(
      county == "Unknown" ~ "Unknown village",
      is.na(latitude) | is.na(longitude) ~ "Missing coordinates",
      is.na(population_density_km2) ~ "Missing region population data",
      TRUE ~ "Ready"
    )
  ) |>
  arrange(priority_rank, region, county)

# ==============================================================================
# Vaccine and product needs, retaining age group for dashboard filtering
# ==============================================================================

need_columns <- intersect(unname(CNMI_VACCINE_NEED_MAP), names(patient))

community_vaccine_needs <- patient |>
  select(
    community_id,
    region,
    county,
    agegroup,
    age_group_label,
    all_of(need_columns)
  ) |>
  pivot_longer(
    all_of(need_columns),
    names_to = "need_variable",
    values_to = "needed"
  ) |>
  mutate(
    vaccine = names(CNMI_VACCINE_NEED_MAP)[
      match(need_variable, CNMI_VACCINE_NEED_MAP)
    ]
  ) |>
  group_by(
    community_id,
    region,
    county,
    agegroup,
    age_group_label,
    vaccine
  ) |>
  summarise(
    eligible_children = n(),
    children_due = sum(needed == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(region, county, agegroup, vaccine)

product_columns <- intersect(unname(CNMI_PRODUCT_MAP), names(patient))

community_product_needs <- patient |>
  select(
    community_id,
    region,
    county,
    agegroup,
    age_group_label,
    all_of(product_columns)
  ) |>
  pivot_longer(
    all_of(product_columns),
    names_to = "product_variable",
    values_to = "needed"
  ) |>
  mutate(
    product = names(CNMI_PRODUCT_MAP)[match(product_variable, CNMI_PRODUCT_MAP)]
  ) |>
  group_by(
    community_id,
    region,
    county,
    agegroup,
    age_group_label,
    product
  ) |>
  summarise(
    doses_needed = sum(needed == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(region, county, agegroup, product)

# ==============================================================================
# QA and exports
# ==============================================================================

qa_cnmi_geography <- patient |>
  group_by(region, county, county_key) |>
  summarise(
    children = n_distinct(patient_id),
    latitude = safe_mean(latitude),
    longitude = safe_mean(longitude),
    coordinate_status = case_when(
      county == "Unknown" ~ "Unknown village",
      is.na(latitude) | is.na(longitude) ~ "Missing coordinates",
      TRUE ~ "Matched"
    ),
    .groups = "drop"
  ) |>
  arrange(region, county)

patient_dashboard <- patient |>
  select(
    -any_of(c("source_url", "immunization_recommendations")),
    -matches("rsv|nirsevimab|clesrovimab|beyfortus|enflonsia", ignore.case = TRUE)
  ) |>
  arrange(region, county, age_months, patient_id)

write_csv(community_summary, dashboard_files[["community"]], na = "")
write_csv(community_vaccine_needs, dashboard_files[["vaccine_needs"]], na = "")
write_csv(community_product_needs, dashboard_files[["product_needs"]], na = "")
write_csv(patient_dashboard, dashboard_files[["patient"]], na = "")
write_csv(qa_cnmi_geography, dashboard_files[["quality"]], na = "")

message("CNMI dashboard data preparation complete.")
message("Patient dataset: ", normalizePath(patient_file, winslash = "/", mustWork = FALSE))
for (path in dashboard_files) {
  message("Created: ", normalizePath(path, winslash = "/", mustWork = FALSE))
}
