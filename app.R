library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(DT)
library(leaflet)
library(plotly)
library(here)

# ==============================================================================
# Project files and dashboard data
# ==============================================================================

project_dir <- here::here()
functions_file <- here::here("R", "cnmi_functions.R")
prepare_data_file <- here::here("R", "prepare_data.R")
styles_file <- here::here("www", "styles.css")

required_project_files <- c(functions_file, prepare_data_file)
missing_project_files <- required_project_files[!file.exists(required_project_files)]

if (length(missing_project_files) > 0) {
  stop(
    paste0(
      "Required project file(s) could not be found:\n  - ",
      paste(missing_project_files, collapse = "\n  - "),
      "\n\nOpen CNMI_Coverage_Planning_Dashboard.Rproj and rerun the app."
    ),
    call. = FALSE
  )
}

source(functions_file, local = FALSE)

# Keep RSV-mAb analytic variables available in the source functions while
# excluding them defensively from every dashboard mapping and loaded data file.
rsv_display_pattern <- regex(
  "RSV|NIRSEVIMAB|CLESROVIMAB|BEYFORTUS|ENFLONSIA",
  ignore_case = TRUE
)

without_rsv_map <- function(x) {
  x[!str_detect(names(x), rsv_display_pattern)]
}

CNMI_VACCINE_NEED_MAP <- without_rsv_map(CNMI_VACCINE_NEED_MAP)
CNMI_COVERAGE_MAP <- without_rsv_map(CNMI_COVERAGE_MAP)
CNMI_PRODUCT_MAP <- without_rsv_map(CNMI_PRODUCT_MAP)

drop_rsv_columns <- function(data) {
  data |>
    select(-matches(
      "rsv|nirsevimab|clesrovimab|beyfortus|enflonsia",
      ignore.case = TRUE
    ))
}

apply_dashboard_pcv_fix <- function(data) {
  if (!all(c("age_months", "pcv_count") %in% names(data))) return(data)

  recommendations <- if ("immunization_recommendations" %in% names(data)) {
    coalesce(as.character(data$immunization_recommendations), "")
  } else {
    rep("", nrow(data))
  }

  data |>
    mutate(
      age_months = as.numeric(age_months),
      pcv_count = coalesce(as.integer(pcv_count), 0L),
      pcv_required_doses = case_when(
        between(age_months, 2, 3) ~ 1L,
        between(age_months, 4, 5) ~ 2L,
        between(age_months, 6, 11) ~ 3L,
        between(age_months, 12, 83) ~ 4L,
        TRUE ~ NA_integer_
      ),
      pcv_recommended_by_iis = str_detect(
        recommendations,
        regex("PCV|PNEUMO|PREVNAR|VAXNEUVANCE|CAPVAXIVE", ignore_case = TRUE)
      ),
      pcv_age_series_incomplete = !is.na(pcv_required_doses) &
        pcv_count < pcv_required_doses,
      need_pcv = pcv_recommended_by_iis | pcv_age_series_incomplete,
      Prevnar = as.integer(need_pcv)
    )
}

dashboard_files <- c(
  community = here::here("data", "community_summary.csv"),
  vaccine_needs = here::here("data", "community_vaccine_needs.csv"),
  product_needs = here::here("data", "community_product_needs.csv"),
  patient = here::here("data", "patient_dashboard.csv"),
  quality = here::here("data", "qa_cnmi_geography.csv")
)

if (any(!file.exists(dashboard_files))) {
  message("Dashboard files are missing. Running R/prepare_data.R.")
  source(prepare_data_file, local = FALSE)
}

missing_dashboard_files <- dashboard_files[!file.exists(dashboard_files)]
if (length(missing_dashboard_files) > 0) {
  stop(
    paste0(
      "Dashboard preparation did not create:\n  - ",
      paste(missing_dashboard_files, collapse = "\n  - "),
      "\n\nRender the Quarto analysis first, then rerun the app."
    ),
    call. = FALSE
  )
}

community <- read_csv(dashboard_files[["community"]], show_col_types = FALSE) |>
  drop_rsv_columns()
vax_needs <- read_csv(dashboard_files[["vaccine_needs"]], show_col_types = FALSE) |>
  drop_rsv_columns() |>
  filter(!str_detect(coalesce(vaccine, ""), rsv_display_pattern))
product_needs <- read_csv(dashboard_files[["product_needs"]], show_col_types = FALSE) |>
  drop_rsv_columns() |>
  filter(!str_detect(coalesce(product, ""), rsv_display_pattern))
patient <- read_csv(dashboard_files[["patient"]], show_col_types = FALSE) |>
  apply_dashboard_pcv_fix() |>
  drop_rsv_columns()
quality <- read_csv(dashboard_files[["quality"]], show_col_types = FALSE) |>
  drop_rsv_columns()

# ==============================================================================
# Display settings and helpers
# ==============================================================================

region_choices <- c("CNMI", "Saipan", "Rota", "Tinian")

age_choices <- c(
  "2-3 months" = "2_3",
  "4-5 months" = "4_5",
  "6-11 months" = "6_11",
  "12-18 months" = "12_18",
  "19-35 months" = "19_35",
  "3 years" = "3_years",
  "4 years" = "4_years",
  "5 years" = "5_years",
  "6 years" = "6_years",
  "2-59 months" = "2_59",
  "2-83 months" = "2_83",
  "4-6 years" = "4_6_years"
)

age_ranges <- list(
  "2_3" = c(2, 3),
  "4_5" = c(4, 5),
  "6_11" = c(6, 11),
  "12_18" = c(12, 18),
  "19_35" = c(19, 35),
  "3_years" = c(36, 47),
  "4_years" = c(48, 59),
  "5_years" = c(60, 71),
  "6_years" = c(72, 83),
  "2_59" = c(2, 59),
  "2_83" = c(2, 83),
  "4_6_years" = c(48, 83)
)

age_labels <- setNames(names(age_choices), unname(age_choices))

coverage_indicator_choices <- c(
  "DTaP UTD" = "dtap_utd",
  "IPV UTD" = "ipv_utd",
  "MMR UTD" = "mmr_utd",
  "Hib UTD" = "hib_utd",
  "HepB UTD" = "hepb_utd",
  "PCV UTD" = "pcv_utd",
  "Rotavirus UTD" = "rota_utd",
  "Varicella UTD" = "var_utd",
  "HepA UTD" = "hepa_utd"
)

coverage_definition_notes <- c(
  "2_3" = "Age-specific UTD includes at least 1 DTaP, 1 IPV, 1 Hib, 2 HepB, and 1 PCV.",
  "4_5" = "Age-specific UTD includes at least 2 DTaP, 2 IPV, 2 Hib, 2 HepB, and 2 PCV.",
  "6_11" = "Age-specific UTD includes at least 3 DTaP, 3 IPV, 2 Hib, 3 HepB, and 3 PCV.",
  "12_18" = paste0(
    "Age-specific UTD includes at least 4 DTaP, 3 IPV, 1 MMR, 3 Hib, 3 HepB, ",
    "4 PCV, 1 varicella, and 1 HepA, with HepA dose 2 required ",
    "once at least 6 months have elapsed after dose 1."
  ),
  "19_35" = paste0(
    "Age-specific UTD includes at least 4 DTaP, 3 IPV, 1 MMR, 3 Hib, 3 HepB, ",
    "4 PCV, 1 varicella, and 1 HepA, with HepA dose 2 required ",
    "once at least 6 months have elapsed after dose 1."
  ),
  "3_years" = paste0(
    "Age-specific UTD includes at least 4 DTaP, 3 IPV, 1 MMR, 3 Hib, 3 HepB, ",
    "4 PCV, 1 varicella, and 1 HepA. MMR and varicella dose 2 ",
    "are not required before 48 months; HepA dose 2 is required once at least ",
    "6 months have elapsed after dose 1."
  ),
  "4_years" = paste0(
    "Age-specific UTD includes at least 5 DTaP (or 4 when dose 4 was given at ",
    "age 48 months or older), 4 IPV (or 3 when dose 3 was given at age 48 months ",
    "or older), 2 MMR, 3 Hib, 3 HepB, 4 PCV, 2 varicella, and 1 HepA, with ",
    "HepA dose 2 required once at least 6 months have elapsed after dose 1."
  ),
  "5_years" = paste0(
    "Age-specific UTD includes at least 5 DTaP (or 4 when dose 4 was given at ",
    "age 48 months or older), 4 IPV (or 3 when dose 3 was given at age 48 months ",
    "or older), 2 MMR, 3 Hib, 3 HepB, 4 PCV, 2 varicella, and 1 HepA, with ",
    "HepA dose 2 required once at least 6 months have elapsed after dose 1."
  ),
  "6_years" = paste0(
    "Age-specific UTD includes at least 5 DTaP (or 4 when dose 4 was given at ",
    "age 48 months or older), 4 IPV (or 3 when dose 3 was given at age 48 months ",
    "or older), 2 MMR, 3 Hib, 3 HepB, 4 PCV, 2 varicella, and 1 HepA, with ",
    "HepA dose 2 required once at least 6 months have elapsed after dose 1."
  ),
  "2_59" = "Each child is evaluated using the exact requirements for the child's age. MMR and varicella dose 2 begin at 48 months.",
  "2_83" = "Each child is evaluated using the exact requirements for the child's age. MMR and varicella dose 2 begin at 48 months.",
  "4_6_years" = paste0(
    "Each child is evaluated using the exact requirements for the child's age. ",
    "The DTaP and IPV school-entry dose rules apply, and 2 MMR and 2 varicella ",
    "doses are required."
  )
)

filter_age <- function(data, age_group) {
  range <- age_ranges[[age_group]]
  data |>
    filter(age_months >= range[[1]], age_months <= range[[2]])
}

filter_region <- function(data, region) {
  if (region == "CNMI") data else filter(data, .data$region == .env$region)
}

priority_levels <- c("High", "Moderate", "Low")
priority_colors <- c("#E03F4F", "#F8C463", "#81912F")
pal_priority <- colorFactor(
  palette = priority_colors,
  levels = priority_levels,
  ordered = TRUE,
  na.color = "#BDBDBD"
)

coverage_colors <- c("#FF0D0D", "#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C")
coverage_legend_labels <- c("<30%", "30-<50%", "50-<80%", "80-<90%", "90-<95%", ">=95%")

coverage_color <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(x) ~ "#BDBDBD",
    x < 30 ~ coverage_colors[[1]],
    x < 50 ~ coverage_colors[[2]],
    x < 80 ~ coverage_colors[[3]],
    x < 90 ~ coverage_colors[[4]],
    x < 95 ~ coverage_colors[[5]],
    TRUE ~ coverage_colors[[6]]
  )
}

region_colors <- c(Saipan = "#7A5195", Rota = "#0056A6", Tinian = "#EF8354")

region_color <- function(x) {
  color <- unname(region_colors[as.character(x)])
  color[is.na(color)] <- "#6C757D"
  color
}

# Use an explicit, no-key basemap so maps render consistently after publishing.
# Keeping the tile URL here also avoids provider-registry changes between local
# and hosted versions of leaflet/leaflet.providers.
dashboard_tile_url <- "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
dashboard_tile_attribution <- paste0(
  "&copy; <a href='https://www.openstreetmap.org/copyright' target='_blank'>",
  "OpenStreetMap</a> contributors"
)

dashboard_leaflet <- function(data) {
  leaflet(
    data,
    options = leafletOptions(
      preferCanvas = TRUE,
      zoomControl = TRUE
    )
  ) |>
    addTiles(
      urlTemplate = dashboard_tile_url,
      attribution = dashboard_tile_attribution,
      options = tileOptions(minZoom = 2, maxZoom = 19, noWrap = TRUE)
    )
}

fit_map_to_data <- function(map, data, default_zoom = 11) {
  distinct_locations <- data |>
    distinct(longitude, latitude)

  if (nrow(distinct_locations) == 1) {
    return(setView(map, data$longitude[[1]], data$latitude[[1]], zoom = default_zoom))
  }

  fitBounds(
    map,
    lng1 = min(data$longitude, na.rm = TRUE),
    lat1 = min(data$latitude, na.rm = TRUE),
    lng2 = max(data$longitude, na.rm = TRUE),
    lat2 = max(data$latitude, na.rm = TRUE)
  )
}

planning_vaccine_order <- names(CNMI_VACCINE_NEED_MAP)
planning_product_order <- names(CNMI_PRODUCT_MAP)

# ==============================================================================
# User interface
# ==============================================================================

ui <- page_navbar(
  title = "CNMI Coverage Planning",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#0056A6", navbar_bg = "#003E73"),
  header = tags$head(
    if (file.exists(styles_file)) tags$link(rel = "stylesheet", href = "styles.css")
  ),

  nav_panel(
    "Situation overview",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectInput(
          "overview_region",
          "Region",
          choices = region_choices,
          selected = "CNMI"
        ),
        sliderInput("min_priority", "Minimum priority score", 0, 100, 0),
        checkboxInput("ready_only", "Only villages ready for mapping", FALSE),
        p(
          class = "small-note",
          paste0(
            "Priority score: 90% community patient risk, 1% inter-island access, ",
            "1% low-density access difficulty, 4% density-adjusted transmission ",
            "potential, and 4% high-risk child burden."
          )
        )
      ),
      layout_columns(
        value_box(
          title = "Children 2 - 83 months",
          value = textOutput("n_children"),
          showcase = bs_icon("people"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Not UTD",
          value = textOutput("n_not_utd"),
          showcase = bs_icon("exclamation-triangle"),
          class = "overview-value-box"
        ),
        value_box(
          title = "High-priority villages",
          value = textOutput("n_high"),
          showcase = bs_icon("geo-alt"),
          class = "overview-value-box"
        ),
        value_box(
          title = "MMR doses potentially needed",
          value = textOutput("n_mmr"),
          showcase = bs_icon("shield-plus"),
          class = "overview-value-box"
        ),
        col_widths = c(3, 3, 3, 3),
        class = "overview-metrics"
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Village priority map"),
          leafletOutput("overview_map", height = "430px")
        ),
        card(
          full_screen = TRUE,
          card_header("Vaccination coverage: up to date for age"),
          leafletOutput("overview_coverage_map", height = "430px")
        ),
        col_widths = c(6, 6)
      ),
      card(card_header("Ranked village priorities"), DTOutput("priority_table"))
    )
  ),

  nav_panel(
    "Vaccination coverage",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectInput("coverage_region", "Region", choices = region_choices, selected = "CNMI"),
        selectInput("coverage_age_group", "Age group", choices = age_choices, selected = "2_59"),
        selectInput(
          "coverage_map_indicator",
          "Coverage map indicator",
          choices = coverage_indicator_choices,
          selected = "dtap_utd"
        )
      ),
      layout_columns(
        value_box(
          title = "Children in selected age group",
          value = textOutput("coverage_children"),
          showcase = bs_icon("people"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Percent UTD for age",
          value = textOutput("coverage_utd_percent"),
          showcase = bs_icon("shield-check"),
          class = "overview-value-box"
        ),
        col_widths = c(6, 6)
      ),
      card(
        card_header("Vaccination coverage by indicator"),
        plotlyOutput("coverage_bar", height = "360px"),
        uiOutput("coverage_definition_note")
      ),
      card(
        full_screen = TRUE,
        card_header("Village vaccination coverage map"),
        p(class = "small-note map-note", "Marker size represents the eligible denominator for the selected vaccine."),
        leafletOutput("vaccination_coverage_map", height = "460px")
      )
    )
  ),

  nav_panel(
    "Vaccination planning",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectInput("planning_region", "Region", choices = region_choices, selected = "CNMI"),
        selectInput("planning_age_group", "Age group", choices = age_choices, selected = "2_59"),
        p(
          class = "small-note",
          paste0(
            "The map and both tables use the selected region and age group. ",
            "MMR dose 2 is not counted as due before 48 months."
          )
        )
      ),
      layout_columns(
        value_box(
          title = "Children selected",
          value = textOutput("planning_children"),
          showcase = bs_icon("people"),
          class = "overview-value-box"
        ),
        value_box(
          title = "On reminder/recall",
          value = textOutput("planning_reminder"),
          showcase = bs_icon("bell"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Villages with reminder/recall",
          value = textOutput("planning_villages"),
          showcase = bs_icon("pin-map"),
          class = "overview-value-box"
        ),
        col_widths = c(4, 4, 4)
      ),
      card(
        full_screen = TRUE,
        card_header("Reminder/recall by village"),
        p(class = "small-note map-note", "Marker size increases with the number of children on reminder/recall."),
        leafletOutput("planning_reminder_map", height = "460px")
      ),
      layout_columns(
        card(
          card_header("Children due by vaccine type"),
          DTOutput("planning_vaccine_table")
        ),
        card(
          card_header("Estimated vaccine products needed"),
          DTOutput("planning_product_table")
        ),
        col_widths = c(6, 6)
      )
    )
  ),

  nav_panel(
    "Community risk & access",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectizeInput(
          "community_id",
          "Village",
          choices = setNames(
            community$community_id,
            paste(community$region, community$county, sep = " - ")
          ),
          selected = community$community_id[[1]]
        )
      ),
      layout_columns(
        card(card_header("Village profile"), uiOutput("community_profile")),
        card(card_header("Priority-score components"), plotlyOutput("components_plot", height = "340px")),
        col_widths = c(5, 7)
      ),
      layout_columns(
        card(card_header("Children due by vaccine type"), DTOutput("community_vaccine_table")),
        card(card_header("Estimated vaccine products needed"), DTOutput("community_product_table")),
        col_widths = c(6, 6)
      )
    )
  ),

  nav_panel(
    "Data quality & methods",
    layout_columns(
      card(
        card_header("Geography QA"),
        DTOutput("quality_table")
      ),
      card(
        card_header("CNMI analytic rules"),
        tags$ul(
          tags$li("County is treated as village; blank county is labeled Unknown."),
          tags$li("City is treated as region: Saipan, Rota, or Tinian."),
          tags$li("For an unknown village, provider name assigns Rota or Tinian when present; otherwise the record is assigned to Saipan."),
          tags$li("MMR and varicella dose 2 are not required before 48 months."),
          tags$li("HepA dose 2 is due at least 6 months after dose 1.")
        ),
        tags$p(
          tags$a(
            href = "https://downloads.aap.org/AAP/PDF/AAP-Immunization-Schedule.pdf",
            target = "_blank",
            "AAP 2026 Child and Adolescent Immunization Schedule"
          )
        )
      ),
      col_widths = c(7, 5)
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {
  overview_data <- reactive({
    x <- community |>
      filter(priority_score >= input$min_priority)

    if (input$overview_region != "CNMI") {
      x <- x |> filter(region == input$overview_region)
    }
    if (isTRUE(input$ready_only)) {
      x <- x |> filter(data_quality_flag == "Ready")
    }
    x
  })

  output$n_children <- renderText(format(sum(overview_data()$child_population, na.rm = TRUE), big.mark = ","))
  output$n_not_utd <- renderText(format(sum(overview_data()$children_not_utd, na.rm = TRUE), big.mark = ","))
  output$n_high <- renderText(sum(overview_data()$priority_group == "High", na.rm = TRUE))
  output$n_mmr <- renderText(format(sum(overview_data()$mmr_not_utd_n, na.rm = TRUE), big.mark = ","))

  output$overview_map <- renderLeaflet({
    x <- overview_data() |> filter(!is.na(latitude), !is.na(longitude))
    validate(need(nrow(x) > 0, "No mapped villages are available for this selection."))

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmin(22, pmax(5, sqrt(child_population))),
        color = ~pal_priority(priority_group),
        fillColor = ~pal_priority(priority_group),
        fillOpacity = 0.82,
        opacity = 1,
        weight = 1,
        label = ~paste0(county, ": ", priority_group, " priority (", priority_score, ")"),
        popup = ~paste0(
          "<b>", county, "</b><br>",
          "Region: ", region, "<br>",
          "Children: ", child_population, "<br>",
          "Not UTD: ", children_not_utd, "<br>",
          "Priority score: ", priority_score, "<br>",
          "Mapping status: ", data_quality_flag
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = priority_colors,
        labels = priority_levels,
        opacity = 0.9,
        title = "Village priority"
      )

    fit_map_to_data(map, x)
  })

  output$overview_coverage_map <- renderLeaflet({
    x <- overview_data() |> filter(!is.na(latitude), !is.na(longitude))
    validate(need(nrow(x) > 0, "No mapped villages are available for this selection."))

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmin(22, pmax(5, sqrt(child_population))),
        color = ~coverage_color(utd_coverage),
        fillColor = ~coverage_color(utd_coverage),
        fillOpacity = 0.85,
        opacity = 1,
        weight = 1,
        label = ~paste0(county, ": ", round(utd_coverage, 1), "% UTD"),
        popup = ~paste0(
          "<b>", county, "</b><br>",
          "Region: ", region, "<br>",
          "UTD for age: ", round(utd_coverage, 1), "%<br>",
          "Children not UTD: ", children_not_utd
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = coverage_colors,
        labels = coverage_legend_labels,
        opacity = 0.9,
        title = "UTD for age"
      )

    fit_map_to_data(map, x)
  })

  output$priority_table <- renderDT({
    datatable(
      overview_data() |>
        transmute(
          `Priority rank` = priority_rank,
          Region = region,
          Village = county,
          Children = child_population,
          `Children not UTD` = children_not_utd,
          `UTD coverage (%)` = round(utd_coverage, 1),
          `MMR coverage (%)` = round(mmr_coverage, 1),
          `Varicella coverage (%)` = round(var_coverage, 1),
          `HepA coverage (%)` = round(hepa_coverage, 1),
          `Priority score` = priority_score,
          `Priority group` = priority_group,
          `Data quality` = data_quality_flag
        ),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
    )
  })

  coverage_scope <- reactive({
    patient |>
      filter_region(input$coverage_region) |>
      filter_age(input$coverage_age_group)
  })

  output$coverage_children <- renderText(
    format(n_distinct(coverage_scope()$patient_id), big.mark = ",")
  )

  output$coverage_utd_percent <- renderText({
    x <- coverage_scope()
    if (nrow(x) == 0) return("-")
    paste0(round(100 * mean(x$utd == 1, na.rm = TRUE), 1), "%")
  })

  output$coverage_definition_note <- renderUI({
    tags$p(class = "small-note", coverage_definition_notes[[input$coverage_age_group]])
  })

  output$coverage_bar <- renderPlotly({
    x <- coverage_scope()

    coverage_data <- lapply(names(coverage_indicator_choices), function(label) {
      variable <- coverage_indicator_choices[[label]]
      values <- x[[variable]]
      denominator <- sum(!is.na(values))

      tibble(
        indicator = label,
        denominator = denominator,
        coverage = if (denominator > 0) 100 * sum(values == 1, na.rm = TRUE) / denominator else NA_real_
      )
    }) |>
      bind_rows() |>
      filter(denominator > 0) |>
      mutate(indicator = factor(indicator, levels = rev(indicator)))

    validate(need(nrow(coverage_data) > 0, "No eligible coverage indicators are available."))

    plot_ly(
      coverage_data,
      x = ~coverage,
      y = ~indicator,
      type = "bar",
      orientation = "h",
      marker = list(color = "#0056A6"),
      text = ~paste0(round(coverage, 1), "% (n=", denominator, ")"),
      textposition = "auto",
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "Coverage (%)", range = c(0, 100)),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 135, r = 20, t = 10, b = 50),
        showlegend = FALSE
      )
  })

  coverage_map_data <- reactive({
    selected_indicator <- input$coverage_map_indicator

    coverage_scope() |>
      mutate(selected_indicator = as.numeric(.data[[selected_indicator]])) |>
      group_by(community_id, region, county) |>
      summarise(
        eligible_children = sum(!is.na(selected_indicator)),
        children_utd = sum(selected_indicator == 1, na.rm = TRUE),
        coverage = if_else(
          eligible_children > 0,
          100 * children_utd / eligible_children,
          NA_real_
        ),
        latitude = median(latitude, na.rm = TRUE),
        longitude = median(longitude, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        latitude = if_else(is.nan(latitude), NA_real_, latitude),
        longitude = if_else(is.nan(longitude), NA_real_, longitude)
      )
  })

  output$vaccination_coverage_map <- renderLeaflet({
    x <- coverage_map_data() |>
      filter(eligible_children > 0, !is.na(latitude), !is.na(longitude))

    validate(need(nrow(x) > 0, "No geocoded coverage data are available for this selection."))

    indicator_label <- names(coverage_indicator_choices)[
      match(input$coverage_map_indicator, coverage_indicator_choices)
    ]

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmin(22, pmax(5, sqrt(eligible_children))),
        color = ~coverage_color(coverage),
        fillColor = ~coverage_color(coverage),
        fillOpacity = 0.85,
        opacity = 1,
        weight = 1,
        label = ~paste0(county, ": ", round(coverage, 1), "%"),
        popup = ~paste0(
          "<b>", county, "</b><br>",
          "Region: ", region, "<br>",
          indicator_label, ": ", round(coverage, 1), "%<br>",
          "Children UTD: ", children_utd, " of ", eligible_children
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = coverage_colors,
        labels = coverage_legend_labels,
        opacity = 0.9,
        title = indicator_label
      )

    fit_map_to_data(map, x)
  })

  planning_scope <- reactive({
    patient |>
      filter_region(input$planning_region) |>
      filter_age(input$planning_age_group)
  })

  planning_reminder_data <- reactive({
    planning_scope() |>
      filter(onreminder == 1, !is.na(latitude), !is.na(longitude), county != "Unknown") |>
      group_by(region, county) |>
      summarise(
        children = n_distinct(patient_id),
        latitude = median(latitude, na.rm = TRUE),
        longitude = median(longitude, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(marker_radius = pmin(24, pmax(5, 3 + 2.2 * sqrt(children))))
  })

  output$planning_children <- renderText(
    format(n_distinct(planning_scope()$patient_id), big.mark = ",")
  )
  output$planning_reminder <- renderText(
    format(n_distinct(planning_scope()$patient_id[planning_scope()$onreminder == 1]), big.mark = ",")
  )
  output$planning_villages <- renderText(n_distinct(planning_reminder_data()$county))

  output$planning_reminder_map <- renderLeaflet({
    x <- planning_reminder_data()
    validate(need(nrow(x) > 0, "No geocoded reminder/recall records are available for this selection."))

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~marker_radius,
        color = ~region_color(region),
        fillColor = ~region_color(region),
        fillOpacity = 0.75,
        opacity = 1,
        weight = 1.3,
        label = ~paste0(county, ": ", children, " children"),
        popup = ~paste0(
          "<b>", county, "</b><br>",
          "Region: ", region, "<br>",
          "Children on reminder/recall: ", children
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = unname(region_colors),
        labels = names(region_colors),
        opacity = 0.9,
        title = "Region"
      )

    fit_map_to_data(map, x)
  })

  output$planning_vaccine_table <- renderDT({
    x <- planning_scope() |>
      select(all_of(unname(CNMI_VACCINE_NEED_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Vaccine = names(CNMI_VACCINE_NEED_MAP)[match(variable, CNMI_VACCINE_NEED_MAP)]) |>
      group_by(Vaccine) |>
      summarise(`Children due` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Vaccine = planning_vaccine_order, fill = list(`Children due` = 0)) |>
      mutate(Vaccine = factor(Vaccine, levels = planning_vaccine_order)) |>
      arrange(Vaccine) |>
      mutate(Vaccine = as.character(Vaccine))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      class = "compact stripe",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, columnDefs = list(list(className = "dt-center", targets = 1)))
    )
  })

  output$planning_product_table <- renderDT({
    x <- planning_scope() |>
      select(all_of(unname(CNMI_PRODUCT_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Product = names(CNMI_PRODUCT_MAP)[match(variable, CNMI_PRODUCT_MAP)]) |>
      group_by(Product) |>
      summarise(`Doses needed` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Product = planning_product_order, fill = list(`Doses needed` = 0)) |>
      mutate(Product = factor(Product, levels = planning_product_order)) |>
      arrange(Product) |>
      mutate(Product = as.character(Product))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      class = "compact stripe",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, columnDefs = list(list(className = "dt-center", targets = 1)))
    )
  })

  selected_community <- reactive({
    community |> filter(community_id == input$community_id) |> slice(1)
  })

  output$community_profile <- renderUI({
    x <- selected_community()
    req(nrow(x) == 1)

    tagList(
      h3(x$county),
      h5(x$region),
      tags$hr(),
      tags$p(strong("Priority: "), x$priority_score, " (", x$priority_group, ")"),
      tags$p(strong("Children: "), x$child_population),
      tags$p(strong("Not UTD: "), x$children_not_utd, " (", round(x$proportion_not_utd, 1), "%)"),
      tags$p(strong("MMR coverage: "), round(x$mmr_coverage, 1), "%"),
      tags$p(strong("Varicella coverage: "), round(x$var_coverage, 1), "%"),
      tags$p(strong("HepA coverage: "), round(x$hepa_coverage, 1), "%"),
      tags$p(strong("Median months since vaccination among not UTD: "), round(x$median_months_since_vax, 1)),
      tags$p(strong("Mapping status: "), x$data_quality_flag)
    )
  })

  output$components_plot <- renderPlotly({
    x <- selected_community()
    req(nrow(x) == 1)

    data <- tibble(
      component = c(
        "Community patient risk",
        "Inter-island access",
        "Low-density access difficulty",
        "Density-adjusted transmission",
        "High-risk child burden"
      ),
      score = c(
        x$community_patient_risk_component,
        x$island_access_component,
        x$low_density_component_for_score,
        x$transmission_component,
        x$burden_component
      ),
      weight = c(0.90, 0.01, 0.01, 0.04, 0.04)
    ) |>
      mutate(component = factor(component, levels = rev(component)))

    plot_ly(
      data,
      x = ~score,
      y = ~component,
      type = "bar",
      orientation = "h",
      marker = list(color = "#0056A6"),
      text = ~paste0(
        round(score, 1), " / 100; weight ", round(weight * 100), "%"
      ),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "Component score (0-100)", range = c(0, 100)),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 225, r = 20, t = 10, b = 55),
        showlegend = FALSE
      )
  })

  output$community_vaccine_table <- renderDT({
    x <- patient |>
      filter(community_id == input$community_id) |>
      select(all_of(unname(CNMI_VACCINE_NEED_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Vaccine = names(CNMI_VACCINE_NEED_MAP)[
        match(variable, CNMI_VACCINE_NEED_MAP)
      ]) |>
      group_by(Vaccine) |>
      summarise(`Children due` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Vaccine = planning_vaccine_order, fill = list(`Children due` = 0)) |>
      mutate(Vaccine = factor(Vaccine, levels = planning_vaccine_order)) |>
      arrange(Vaccine) |>
      mutate(Vaccine = as.character(Vaccine))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  })

  output$community_product_table <- renderDT({
    x <- patient |>
      filter(community_id == input$community_id) |>
      select(all_of(unname(CNMI_PRODUCT_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Product = names(CNMI_PRODUCT_MAP)[match(variable, CNMI_PRODUCT_MAP)]) |>
      group_by(Product) |>
      summarise(`Doses needed` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Product = planning_product_order, fill = list(`Doses needed` = 0)) |>
      mutate(Product = factor(Product, levels = planning_product_order)) |>
      arrange(Product) |>
      mutate(Product = as.character(Product))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  })

  output$quality_table <- renderDT({
    datatable(
      quality |>
        transmute(
          Region = region,
          Village = county,
          Children = children,
          Latitude = round(latitude, 6),
          Longitude = round(longitude, 6),
          Status = coordinate_status
      ),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
