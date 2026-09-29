# ==============================================================================
# Shared functions for the CNMI childhood vaccination analysis and dashboard
# ==============================================================================

normalize_cnmi_text <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("&", " AND ") |>
    stringr::str_replace_all("[^A-Z0-9]+", " ") |>
    stringr::str_squish() |>
    dplyr::na_if("")
}

clean_cnmi_label <- function(x, unknown = "Unknown") {
  key <- normalize_cnmi_text(x)
  dplyr::case_when(
    is.na(key) ~ unknown,
    key %in% c("UNKNOWN", "UNK", "MISSING", "NA", "N A") ~ unknown,
    TRUE ~ stringr::str_to_title(key)
  )
}

coalesce_character_columns <- function(data, candidates) {
  out <- rep(NA_character_, nrow(data))

  for (candidate in candidates) {
    if (candidate %in% names(data)) {
      value <- data[[candidate]] |>
        as.character() |>
        stringr::str_squish() |>
        dplyr::na_if("")

      out <- dplyr::coalesce(out, value)
    }
  }

  out
}

add_missing_columns <- function(data, columns, value = NA) {
  for (column in setdiff(columns, names(data))) {
    data[[column]] <- value
  }
  data
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (is.character(x)) {
    x <- x[stringr::str_squish(x) != ""]
  }
  if (length(x) == 0) NA else x[[1]]
}

parse_cnmi_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  value <- stringr::str_squish(as.character(x))
  value[value == ""] <- NA_character_

  as.Date(
    suppressWarnings(
      lubridate::parse_date_time(
        value,
        orders = c(
          "ymd", "mdy", "dmy",
          "ymd HMS", "mdy HMS", "dmy HMS",
          "Ymd", "mdY", "dmY"
        ),
        quiet = TRUE
      )
    )
  )
}

newest_matching_file <- function(directory, pattern, required = TRUE) {
  files <- list.files(
    directory,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    if (!required) return(NA_character_)
    stop(
      paste0(
        "No file matching `", pattern, "` was found in:\n  ",
        normalizePath(directory, winslash = "/", mustWork = FALSE)
      ),
      call. = FALSE
    )
  }

  files[[which.max(file.info(files)$mtime)]]
}

standardize_cnmi_region <- function(x) {
  key <- normalize_cnmi_text(x)

  dplyr::case_when(
    stringr::str_detect(dplyr::coalesce(key, ""), "ROTA") ~ "Rota",
    stringr::str_detect(dplyr::coalesce(key, ""), "TINIAN") ~ "Tinian",
    stringr::str_detect(dplyr::coalesce(key, ""), "SAIPAN") ~ "Saipan",
    TRUE ~ NA_character_
  )
}

read_neighborhood_lookup <- function(path) {
  readxl::read_excel(path, sheet = "CNMI Coordinates") |>
    janitor::clean_names() |>
    dplyr::transmute(
      region_lookup = standardize_cnmi_region(island),
      county_key = normalize_cnmi_text(neighborhood),
      county_lookup = stringr::str_to_title(normalize_cnmi_text(neighborhood)),
      latitude_lookup = as.numeric(latitude),
      longitude_lookup = as.numeric(longitude),
      coordinate_basis,
      coordinate_source_url = source_url
    ) |>
    dplyr::filter(!is.na(county_key)) |>
    dplyr::distinct(county_key, .keep_all = TRUE)
}

assign_cnmi_geography <- function(data, neighborhood_lookup) {
  data <- add_missing_columns(data, c("county", "city"), NA_character_)
  data <- add_missing_columns(data, c("latitude", "longitude"), NA_real_)

  provider_text <- coalesce_character_columns(
    data,
    c(
      "provider", "provider_name", "default_provider",
      "patient_default_provider", "patient_default_clinic", "clinic"
    )
  )

  data |>
    dplyr::mutate(
      county_input = county,
      city_input = city,
      provider_for_geography = provider_text,
      county_key = normalize_cnmi_text(county_input),
      county_key = dplyr::if_else(
        is.na(county_key) |
          county_key %in% c("UNKNOWN", "UNK", "MISSING", "NA", "N A"),
        NA_character_,
        county_key
      ),
      region_from_city = standardize_cnmi_region(city_input),
      region_from_provider = standardize_cnmi_region(provider_for_geography)
    ) |>
    dplyr::left_join(
      neighborhood_lookup |>
        dplyr::select(
          county_key,
          county_lookup,
          region_lookup,
          latitude_lookup,
          longitude_lookup
        ),
      by = "county_key"
    ) |>
    dplyr::mutate(
      county = dplyr::coalesce(
        county_lookup,
        clean_cnmi_label(county_input, unknown = NA_character_),
        "Unknown"
      ),
      county = dplyr::if_else(is.na(county_key), "Unknown", county),
      region = dplyr::coalesce(
        region_from_city,
        region_lookup,
        region_from_provider,
        "Saipan"
      ),
      # These aliases retain compatibility with older FSM-derived code while
      # using the CNMI definitions: City = region and County = village.
      city = region,
      state = region,
      latitude = dplyr::coalesce(
        suppressWarnings(as.numeric(latitude)),
        latitude_lookup
      ),
      longitude = dplyr::coalesce(
        suppressWarnings(as.numeric(longitude)),
        longitude_lookup
      ),
      community_id = paste(normalize_cnmi_text(region), normalize_cnmi_text(county), sep = "__")
    ) |>
    dplyr::select(
      -region_from_city,
      -region_from_provider,
      -county_lookup,
      -region_lookup,
      -latitude_lookup,
      -longitude_lookup
    )
}

cnmi_age_group <- function(age_months) {
  dplyr::case_when(
    dplyr::between(age_months, 2, 3) ~ 1L,
    dplyr::between(age_months, 4, 5) ~ 2L,
    dplyr::between(age_months, 6, 11) ~ 3L,
    dplyr::between(age_months, 12, 18) ~ 4L,
    dplyr::between(age_months, 19, 35) ~ 5L,
    dplyr::between(age_months, 36, 47) ~ 6L,
    dplyr::between(age_months, 48, 59) ~ 7L,
    dplyr::between(age_months, 60, 71) ~ 8L,
    dplyr::between(age_months, 72, 83) ~ 9L,
    TRUE ~ NA_integer_
  )
}

cnmi_age_label <- function(age_months) {
  dplyr::case_when(
    dplyr::between(age_months, 2, 3) ~ "2-3 months",
    dplyr::between(age_months, 4, 5) ~ "4-5 months",
    dplyr::between(age_months, 6, 11) ~ "6-11 months",
    dplyr::between(age_months, 12, 18) ~ "12-18 months",
    dplyr::between(age_months, 19, 35) ~ "19-35 months",
    dplyr::between(age_months, 36, 47) ~ "3 years",
    dplyr::between(age_months, 48, 59) ~ "4 years",
    dplyr::between(age_months, 60, 71) ~ "5 years",
    dplyr::between(age_months, 72, 83) ~ "6 years",
    TRUE ~ NA_character_
  )
}

CNMI_CVX_CODES <- list(
  bcg = c("19"),
  dtap = c("1", "20", "22", "28", "50", "102", "106", "107", "110", "120", "130", "132", "146", "170", "198"),
  ipv = c("2", "10", "89", "110", "120", "130", "132", "146", "170", "178", "179", "182", "195"),
  mmr = c("3", "94"),
  hepb = c("8", "42", "43", "44", "45", "51", "102", "104", "110", "132", "146", "189", "193", "198", "220"),
  pcv = c("33", "100", "109", "133", "152", "177", "215", "216"),
  hib = c("17", "22", "46", "47", "48", "49", "50", "51", "102", "120", "132", "146", "148", "170", "198"),
  rota = c("74", "116", "119", "122"),
  var = c("21", "94"),
  hepa = c("31", "52", "83", "84", "85", "104", "193"),
  rsv_mab = c("306", "307", "332")
)

classify_cnmi_antigens <- function(cvx, vaccine_name = NA_character_) {
  cvx <- stringr::str_remove(as.character(cvx), "\\.0$")
  name <- normalize_cnmi_text(vaccine_name)
  name <- dplyr::coalesce(name, "")

  antigens <- names(CNMI_CVX_CODES)[
    vapply(CNMI_CVX_CODES, function(codes) cvx %in% codes, logical(1))
  ]

  name_rules <- c(
    bcg = "(^| )BCG($| )|CALMETTE",
    dtap = "DTAP|DIPHTHERIA.*PERTUSSIS",
    ipv = "(^| )IPV($| )|POLIO",
    mmr = "(^| )MMR($| )|MMRV|MEASLES.*MUMPS.*RUBELLA",
    hepb = "HEP B|HEPATITIS B|TWINRIX",
    pcv = "PCV|PNEUMOCOCCAL|PREVNAR",
    hib = "(^| )HIB($| )|HAEMOPHILUS",
    rota = "ROTA",
    var = "VARICELLA|MMRV|VARIVAX|PROQUAD",
    hepa = "HEP A|HEPATITIS A|HAVRIX|VAQTA|TWINRIX",
    rsv_mab = "NIRSEVIMAB|CLESROVIMAB|BEYFORTUS|ENFLONSIA|RSV MAB|RSV MONOCLONAL"
  )

  for (antigen in names(name_rules)) {
    if (stringr::str_detect(name, name_rules[[antigen]])) {
      antigens <- union(antigens, antigen)
    }
  }

  antigens
}

extract_patient_id <- function(data) {
  existing <- coalesce_character_columns(
    data,
    c("patient_id", "patient_identifier", "webiz_id", "record_id")
  )

  patient_name <- coalesce_character_columns(
    data,
    c("patient_name", "patient", "name")
  )

  extracted <- stringr::str_match(patient_name, "\\(([^()]*)\\)")[, 2]
  extracted <- stringr::str_extract(extracted, "[A-Za-z0-9-]+")

  dplyr::coalesce(existing, extracted)
}

build_cnmi_vaccine_history <- function(detail) {
  detail <- add_missing_columns(
    detail,
    c("vaccination_code_id", "vaccination_date"),
    NA
  )

  vaccine_name <- coalesce_character_columns(
    detail,
    c(
      "vaccine_name", "vaccination_name", "immunization_name",
      "vaccination_description", "vaccine", "product_name"
    )
  )

  history_long <- detail |>
    dplyr::mutate(
      patient_id = extract_patient_id(detail),
      cvx = stringr::str_remove(as.character(vaccination_code_id), "\\.0$"),
      vaccine_name_for_matching = vaccine_name,
      vaccination_date = parse_cnmi_date(vaccination_date),
      antigen = purrr::map2(cvx, vaccine_name_for_matching, classify_cnmi_antigens)
    ) |>
    dplyr::filter(!is.na(patient_id), lengths(antigen) > 0) |>
    tidyr::unnest_longer(antigen) |>
    dplyr::filter(!is.na(vaccination_date))

  if (nrow(history_long) == 0) {
    return(tibble::tibble(patient_id = character()))
  }

  per_antigen <- history_long |>
    dplyr::arrange(patient_id, antigen, vaccination_date) |>
    dplyr::group_by(patient_id, antigen) |>
    dplyr::summarise(
      count = dplyr::n_distinct(vaccination_date),
      dose1_date = dplyr::nth(vaccination_date, 1, default = as.Date(NA)),
      dose2_date = dplyr::nth(vaccination_date, 2, default = as.Date(NA)),
      dose3_date = dplyr::nth(vaccination_date, 3, default = as.Date(NA)),
      dose4_date = dplyr::nth(vaccination_date, 4, default = as.Date(NA)),
      dose5_date = dplyr::nth(vaccination_date, 5, default = as.Date(NA)),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = antigen,
      values_from = c(count, dose1_date, dose2_date, dose3_date, dose4_date, dose5_date),
      names_glue = "{antigen}_{.value}"
    )

  latest <- history_long |>
    dplyr::group_by(patient_id) |>
    dplyr::summarise(
      last_vaccination_date = max(vaccination_date, na.rm = TRUE),
      .groups = "drop"
    )

  dplyr::left_join(per_antigen, latest, by = "patient_id")
}

add_cnmi_utd_flags <- function(data, age_as_of_date) {
  antigens <- names(CNMI_CVX_CODES)
  count_columns <- paste0(antigens, "_count")
  date_columns <- as.vector(outer(
    antigens,
    paste0("dose", 1:5, "_date"),
    paste,
    sep = "_"
  ))

  data <- add_missing_columns(data, count_columns, 0L)
  data <- add_missing_columns(data, date_columns, as.Date(NA))
  data <- add_missing_columns(data, c("dob", "last_vaccination_date"), as.Date(NA))

  data |>
    dplyr::mutate(
      dob = parse_cnmi_date(dob),
      last_vaccination_date = parse_cnmi_date(last_vaccination_date),
      dplyr::across(dplyr::all_of(count_columns), ~dplyr::coalesce(as.integer(.x), 0L)),
      dplyr::across(dplyr::all_of(date_columns), parse_cnmi_date),
      age_months = floor(lubridate::time_length(lubridate::interval(dob, age_as_of_date), "months")),
      age_years = floor(lubridate::time_length(lubridate::interval(dob, age_as_of_date), "years")),
      agegroup = cnmi_age_group(age_months),
      age_group_label = cnmi_age_label(age_months),
      dtap4_age_months = floor(lubridate::time_length(lubridate::interval(dob, dtap_dose4_date), "months")),
      ipv3_age_months = floor(lubridate::time_length(lubridate::interval(dob, ipv_dose3_date), "months")),

      dtap1utd = as.integer(dtap_count >= 1),
      dtap2utd = as.integer(dtap_count >= 2),
      dtap3utd = as.integer(dtap_count >= 3),
      dtap4utd = as.integer(dtap_count >= 4),
      dtap5utd = as.integer(dtap_count >= 5),
      dtap54utd = as.integer(dtap_count >= 5 | (dtap_count >= 4 & dtap4_age_months >= 48)),

      ipv1utd = as.integer(ipv_count >= 1),
      ipv2utd = as.integer(ipv_count >= 2),
      ipv3utd = as.integer(ipv_count >= 3),
      ipv4utd = as.integer(ipv_count >= 4),
      ipv43utd = as.integer(ipv_count >= 4 | (ipv_count >= 3 & ipv3_age_months >= 48)),

      mmr1utd = as.integer(mmr_count >= 1),
      mmr2utd = as.integer(mmr_count >= 2),
      mmr_utd = dplyr::case_when(
        age_months < 12 ~ NA_integer_,
        dplyr::between(age_months, 12, 47) ~ mmr1utd,
        dplyr::between(age_months, 48, 83) ~ mmr2utd,
        TRUE ~ NA_integer_
      ),

      hib1utd = as.integer(hib_count >= 1),
      hib2utd = as.integer(hib_count >= 2),
      hib3utd = as.integer(hib_count >= 3),
      hib4utd = as.integer(hib_count >= 4),
      hib_utd = dplyr::case_when(
        dplyr::between(age_months, 2, 3) ~ hib1utd,
        dplyr::between(age_months, 4, 11) ~ hib2utd,
        dplyr::between(age_months, 12, 83) ~ hib3utd,
        TRUE ~ NA_integer_
      ),

      hepb1utd = as.integer(hepb_count >= 1),
      hepb2utd = as.integer(hepb_count >= 2),
      hepb3utd = as.integer(hepb_count >= 3),
      hepb_utd = dplyr::case_when(
        dplyr::between(age_months, 2, 5) ~ hepb2utd,
        dplyr::between(age_months, 6, 83) ~ hepb3utd,
        TRUE ~ NA_integer_
      ),

      pcv1utd = as.integer(pcv_count >= 1),
      pcv2utd = as.integer(pcv_count >= 2),
      pcv3utd = as.integer(pcv_count >= 3),
      pcv4utd = as.integer(pcv_count >= 4),
      pcv_required_doses = dplyr::case_when(
        dplyr::between(age_months, 2, 3) ~ 1L,
        dplyr::between(age_months, 4, 5) ~ 2L,
        dplyr::between(age_months, 6, 11) ~ 3L,
        dplyr::between(age_months, 12, 83) ~ 4L,
        TRUE ~ NA_integer_
      ),
      pcv_utd = dplyr::case_when(
        !is.na(pcv_required_doses) ~ as.integer(pcv_count >= pcv_required_doses),
        TRUE ~ NA_integer_
      ),

      rota1utd = as.integer(rota_count >= 1),
      rota2utd = as.integer(rota_count >= 2),
      rota3utd = as.integer(rota_count >= 3),
      rota_utd = dplyr::case_when(
        dplyr::between(age_months, 2, 3) ~ rota1utd,
        dplyr::between(age_months, 4, 5) ~ rota2utd,
        dplyr::between(age_months, 6, 83) ~ rota3utd,
        TRUE ~ NA_integer_
      ),

      var1utd = as.integer(var_count >= 1),
      var2utd = as.integer(var_count >= 2),
      var_utd = dplyr::case_when(
        age_months < 12 ~ NA_integer_,
        dplyr::between(age_months, 12, 47) ~ var1utd,
        dplyr::between(age_months, 48, 83) ~ var2utd,
        TRUE ~ NA_integer_
      ),

      hepa1utd = as.integer(hepa_count >= 1),
      hepa2utd = as.integer(hepa_count >= 2),
      hepa_second_dose_due = dplyr::case_when(
        hepa_count != 1 | is.na(hepa_dose1_date) ~ FALSE,
        TRUE ~ age_as_of_date >= lubridate::add_with_rollback(
          hepa_dose1_date,
          lubridate::period(months = 6)
        )
      ),
      hepa_utd = dplyr::case_when(
        age_months < 12 ~ NA_integer_,
        hepa_count >= 2 ~ 1L,
        hepa_count == 1 & !hepa_second_dose_due ~ 1L,
        dplyr::between(age_months, 12, 83) ~ 0L,
        TRUE ~ NA_integer_
      ),

      dtap_utd = dplyr::case_when(
        dplyr::between(age_months, 2, 3) ~ dtap1utd,
        dplyr::between(age_months, 4, 5) ~ dtap2utd,
        dplyr::between(age_months, 6, 11) ~ dtap3utd,
        dplyr::between(age_months, 12, 47) ~ dtap4utd,
        dplyr::between(age_months, 48, 83) ~ dtap54utd,
        TRUE ~ NA_integer_
      ),
      ipv_utd = dplyr::case_when(
        dplyr::between(age_months, 2, 3) ~ ipv1utd,
        dplyr::between(age_months, 4, 5) ~ ipv2utd,
        dplyr::between(age_months, 6, 47) ~ ipv3utd,
        dplyr::between(age_months, 48, 83) ~ ipv43utd,
        TRUE ~ NA_integer_
      ),
      days_since_last_vax = as.numeric(age_as_of_date - last_vaccination_date),
      months_since_last_vax = pmin(days_since_last_vax / 30.4375, 36)
    ) |>
    dplyr::filter(!is.na(agegroup))
}

recommendation_flag <- function(text, pattern) {
  stringr::str_detect(
    dplyr::coalesce(as.character(text), ""),
    stringr::regex(pattern, ignore_case = TRUE)
  )
}

add_cnmi_need_and_product_flags <- function(data, age_as_of_date) {
  data <- add_missing_columns(
    data,
    c("immunization_recommendations", "onreminder"),
    NA
  )

  data |>
    dplyr::mutate(
      onreminder = dplyr::case_when(
        !is.na(onreminder) ~ as.integer(onreminder),
        stringr::str_squish(dplyr::coalesce(as.character(immunization_recommendations), "")) != "" ~ 1L,
        TRUE ~ 0L
      ),
      need_dtap = recommendation_flag(immunization_recommendations, "DTAP\\s*\\("),
      need_dtap123 = recommendation_flag(immunization_recommendations, "DTAP\\s*\\((1|2|3)\\)"),
      need_ipv = recommendation_flag(immunization_recommendations, "(POLIO|IPV)\\s*\\("),
      need_ipv123 = recommendation_flag(immunization_recommendations, "(POLIO|IPV)\\s*\\((1|2|3)\\)"),
      need_hib = recommendation_flag(immunization_recommendations, "HIB\\s*\\("),
      need_hib4 = recommendation_flag(immunization_recommendations, "HIB\\s*\\(4\\)"),
      need_hib123 = recommendation_flag(immunization_recommendations, "HIB\\s*\\((1|2|3)\\)"),
      need_hepb = recommendation_flag(immunization_recommendations, "HEP ?B\\s*\\("),
      # WebIZ labels are not consistent across PCV products and reports. Keep
      # the IIS recommendation flag, but determine PCV need independently from
      # the number of doses required for the child's age. This also guarantees
      # that every child with an incomplete PCV series produces one planned
      # Prevnar dose for the current planning round.
      pcv_recommended_by_iis = recommendation_flag(
        immunization_recommendations,
        "PCV|PNEUMO|PREVNAR|VAXNEUVANCE|CAPVAXIVE"
      ),
      pcv_age_series_incomplete = !is.na(pcv_required_doses) &
        pcv_count < pcv_required_doses,
      need_pcv = pcv_recommended_by_iis | pcv_age_series_incomplete,
      need_rota = recommendation_flag(immunization_recommendations, "ROTA(VIRUS)?\\s*\\("),

      # CNMI follows the routine 2-dose MMR schedule at 12-15 months and
      # 4-6 years. The second dose is never generated before 48 months.
      need_mmr_dose1 = age_months >= 12 & mmr_count < 1,
      need_mmr_dose2 = age_months >= 48 & mmr_count < 2,
      need_mmr = recommendation_flag(immunization_recommendations, "(MMR|MEASLES)\\s*\\(") |
        need_mmr_dose1 | need_mmr_dose2,

      need_var = recommendation_flag(immunization_recommendations, "(VARICELLA|VAR|MMRV)\\s*\\(") |
        (!is.na(var_utd) & var_utd == 0),
      need_hepa = recommendation_flag(immunization_recommendations, "(HEP ?A|HEPATITIS A)\\s*\\(") |
        (!is.na(hepa_utd) & hepa_utd == 0),

      # RSV-mAb eligibility depends on season, maternal RSV vaccination, and
      # risk status. In the absence of those fields, a child is counted as due
      # only when the IIS reminder/recall explicitly recommends RSV-mAb.
      need_rsv_mab = recommendation_flag(
        immunization_recommendations,
        "RSV|NIRSEVIMAB|CLESROVIMAB|BEYFORTUS|ENFLONSIA"
      ),
      rsv_mab_utd = dplyr::case_when(
        rsv_mab_count >= 1 ~ 1L,
        need_rsv_mab ~ 0L,
        TRUE ~ NA_integer_
      ),

      utd_no_mmr = as.integer(
        dplyr::coalesce(dtap_utd, 1L) == 1 &
          dplyr::coalesce(ipv_utd, 1L) == 1 &
          dplyr::coalesce(hib_utd, 1L) == 1 &
          dplyr::coalesce(hepb_utd, 1L) == 1 &
          dplyr::coalesce(pcv_utd, 1L) == 1 &
          dplyr::coalesce(var_utd, 1L) == 1 &
          dplyr::coalesce(hepa_utd, 1L) == 1
      ),
      utd = as.integer(
        utd_no_mmr == 1 & dplyr::coalesce(mmr_utd, 1L) == 1
      ),

      vaxelis_antigen_count = as.integer(need_dtap123) +
        as.integer(need_ipv123) + as.integer(need_hib123) + as.integer(need_hepb),
      pediarix_antigen_count = as.integer(need_dtap123) +
        as.integer(need_ipv123) + as.integer(need_hepb),
      Vaxelis = as.integer(
        need_hib123 & vaxelis_antigen_count >= 2 &
          !(need_hib123 & !need_dtap & !need_ipv & !need_hepb)
      ),
      Pediarix = as.integer(
        (need_hib4 | !need_hib) & pediarix_antigen_count >= 2 & Vaxelis == 0
      ),
      PedVaxHib = as.integer(
        need_hib4 | (need_hib123 & Vaxelis == 0 & Pediarix == 0)
      ),
      DTaP_Single = as.integer(need_dtap & Vaxelis == 0 & Pediarix == 0),
      IPV_Single = as.integer(need_ipv & Vaxelis == 0 & Pediarix == 0),
      HepB_Single = as.integer(need_hepb & Vaxelis == 0 & Pediarix == 0),
      MMR_Single = as.integer(need_mmr),
      Prevnar = as.integer(need_pcv),
      RotaTeq = as.integer(need_rota),
      Varivax = as.integer(need_var),
      Havrix = as.integer(need_hepa),
      RSV_mAb_Product = as.integer(need_rsv_mab),
      t_i_tilde = dplyr::coalesce(pmin(months_since_last_vax, 36) / 36, 0),
      individual_risk_score = 100 * (
        0.20 * as.integer(utd_no_mmr == 0) +
          0.55 * as.integer(dplyr::coalesce(mmr_utd, 1L) == 0) +
          0.25 * t_i_tilde
      )
    ) |>
    dplyr::select(-vaxelis_antigen_count, -pediarix_antigen_count)
}

CNMI_VACCINE_NEED_MAP <- c(
  DTaP = "need_dtap",
  IPV = "need_ipv",
  MMR = "need_mmr",
  Hib = "need_hib",
  HepB = "need_hepb",
  PCV = "need_pcv",
  Rotavirus = "need_rota",
  Varicella = "need_var",
  HepA = "need_hepa"
)

CNMI_COVERAGE_MAP <- c(
  DTaP = "dtap_utd",
  IPV = "ipv_utd",
  MMR = "mmr_utd",
  Hib = "hib_utd",
  HepB = "hepb_utd",
  PCV = "pcv_utd",
  Rotavirus = "rota_utd",
  Varicella = "var_utd",
  HepA = "hepa_utd"
)

CNMI_PRODUCT_MAP <- c(
  PedVaxHib = "PedVaxHib",
  Vaxelis = "Vaxelis",
  Pediarix = "Pediarix",
  `DTaP single-antigen` = "DTaP_Single",
  `IPV single-antigen` = "IPV_Single",
  `HepB single-antigen` = "HepB_Single",
  `MMR single-antigen` = "MMR_Single",
  Prevnar = "Prevnar",
  RotaTeq = "RotaTeq",
  Varivax = "Varivax",
  Havrix = "Havrix"
)
