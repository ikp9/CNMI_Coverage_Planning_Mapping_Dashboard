# CNMI_Coverage_Planning_Mapping_Dashboard
CNMI vaccination coverage, outreach planning, and mapping dashboard

# CNMI Coverage Planning Dashboard

This project adapts the FSM childhood vaccination analysis and dashboard for
the Commonwealth of the Northern Mariana Islands (CNMI). The analysis is a
Quarto document, and the dashboard focuses on coverage, community risk, and
vaccination planning. The FSM Outreach Planner and Scenario Comparison modules
have been removed.

## Project structure

```text
CNMI_Coverage_Planning_Dashboard/
├── analysis/
│   └── CNMI_Child_Coverage_Planning_Analysis.qmd
├── data/
│   ├── Raw Patient Data/
│   ├── Analytic Code Output/
│   ├── CNMI_County_Population_Area_GIS.xlsx
│   └── CNMI_Neighborhood_Coordinates.xlsx
├── R/
│   ├── cnmi_functions.R
│   └── prepare_data.R
├── www/
│   └── styles.css
├── app.R
└── CNMI_Coverage_Planning_Dashboard.Rproj
```

## First-time setup

1. Open `CNMI_Coverage_Planning_Dashboard.Rproj` in RStudio.
2. Install the required packages:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "dplyr", "tidyr", "purrr",
  "stringr", "lubridate", "readxl", "readr", "janitor",
  "openxlsx", "here", "tibble", "DT", "leaflet", "plotly",
  "knitr", "quarto"
))
```

3. Place the three WebIZ workbooks in `data/Raw Patient Data`:

   - a file beginning with `Roster`
   - a file beginning with `Detail` or `Details`
   - a file beginning with `RR` or containing `Reminder`

The newest matching file is used when several files are present.

## Run the analysis

Open `analysis/CNMI_Child_Coverage_Planning_Analysis.qmd`. Update the
`age_as_of_date` parameter at the top so it matches the WebIZ report date, then
render the document.

The analysis creates:

- a full patient-level analytic workbook;
- a deidentified workbook with a run-specific surrogate patient ID;
- a Patients Needing Vaccination workbook;
- a formatted planning-tables workbook;
- missing-coordinate QA; and
- the five CSV files used by the dashboard.

All patient-level workbooks are written to `data/Analytic Code Output`.

## Run the dashboard

After rendering the analysis, run:

```r
shiny::runApp()
```

If dashboard CSV files are missing but a deidentified analytic workbook exists,
`app.R` automatically runs `R/prepare_data.R`.

## Geography rules

- CNMI overall, Saipan, Rota, and Tinian are the dashboard choices.
- `County` is treated as village.
- `City` is treated as region.
- A blank or unknown County becomes `Unknown`.
- When village is unknown, a provider name containing `Rota` or `Tinian`
  assigns that region; otherwise the record is assigned to Saipan.
- A known village can also be assigned to its region using the attached
  neighborhood coordinate crosswalk.

The Northern Islands Municipality appears in the Census reference workbook but
is not used as a dashboard region, consistent with the requested three-region
structure.

## Vaccine rules added for CNMI

- MMR dose 2 is required beginning at 48 months, not before age 4 years.
- Varicella dose 1 begins at 12 months and dose 2 at 48 months.
- HepA begins at 12 months; dose 2 is due at least 6 months after dose 1.
- RSV monoclonal antibody includes nirsevimab (Beyfortus) and clesrovimab
  (Enflonsia). Because eligibility depends on maternal RSV vaccination,
  seasonality, and high-risk status, a child is counted as due only when the IIS
  reminder/recall explicitly recommends RSV-mAb.

Schedule source:
https://downloads.aap.org/AAP/PDF/AAP-Immunization-Schedule.pdf

CVX source:
https://www2.cdc.gov/vaccines/iis/iisstandards/vaccines.asp?rpt=cvx

## Privacy and GitHub

The `.gitignore` excludes raw patient data, patient-level analytic outputs, and
generated dashboard CSVs. Do not override these exclusions for files containing
PII. The geography workbooks and source code are safe to share through the
repository.

#Updates Sept. 3, 2026-AT
1.	Removed all instances of RSV-mAb from tables and the dashboard, including any footnotes. 
2.	Fixed PCV recommendations
3.	Added number of doses relevant for each age group in the vaccine label, added rota 3 for all older age groups. 
4.	Add filters to all the tables
5.	On the dashboard vaccination coverage page, updated footnotes 
6.	On the vaccination planning page, fixed colors on the map not matching the legend

#Updates Sept. 29, 2026 - AT
Cloned into new folder because of .git issues with old project
