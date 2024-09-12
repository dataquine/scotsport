# File: 01-download-clean-population-data.R
# Author: Lesley Duff
# Date created: 2024-08-29
# Description:
#    Download a population data spreadsheet from the National Records of
# Scotland

# Data Source
# Population Estimates Time Series Data
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/population-estimates-time-series-data
#
# National Records of Scotland
#
# This time series section provides access to the latest time series data, taking
# into account any revisions or corrections over the years.
#
# All content is available under the [Open Government Licence v3.0]
# (https://www.nationalarchives.gov.uk/doc/open-government-licence/
#     "Link to Open Government Licence v3.0") except where otherwise stated.

# Libraries --------------------------------------------------------------------
library(dplyr) # A Grammar of Data Manipulation
library(glue) # Interpreted String Literals
library(here) # A Simpler Way to Find Your Files
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(readr) # Read Rectangular Text Data
library(readxl) # For processing Excel spreadsheets
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(tidyr) # Tidy Messy Data

# Constants --------------------------------------------------------------------
filename_population_scotland_csv <- "scotland-population.csv"
filename_population_scotland_raw <- "mid-year-pop-est-time-series.xlsx"
url_population_data_source <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-year-pop-est-time-series.xlsx"
sheet_population_data <- "Table 3"
sheet_population_skip <- 5

# For switching off during dev
DO_DOWNLOAD <- TRUE

# download-population-data -----------------------------------------------------
path_population_spreadsheet_raw <- here::here(
  "data-raw",
  filename_population_scotland_raw
)

message(glue::glue(
  "Downloading population data from {url_population_data_source}"
))

if (DO_DOWNLOAD) {
  # This is provided as an Excel spreadsheet, download the binary
  download_result <- download.file(url_population_data_source,
    path_population_spreadsheet_raw,
    quiet = FALSE,
    mode = "wb"
  )
  #  0 for success and non-zero for failure.
  download_ok <- download_result == 0
  stopifnot(download_ok)
}

# clean-population-data --------------------------------------------------------

# Read the spreadsheet file
population_data_spreadsheet_raw <- readxl::read_xlsx(
  path_population_spreadsheet_raw,
  sheet = sheet_population_data,
  skip = sheet_population_skip
) |>
  janitor::clean_names() |>
  # Get the sheet out of the spreadsheet
  # Mid-year population estimates by administrative area and sex, 1981-2022 [note 1] [note 2]
  filter(
    sex == "Persons",
    area_type %in% c("Country", "Council area")
  ) |>
  # Keep the last column as the 'latest' year of population data available
  select(area_type, area_name = area_name_note_3, last_col())

population_data_spreadsheet <- population_data_spreadsheet_raw |>
  # Split the year out of a column name like x2022
  tidyr::pivot_longer(
    cols = last_col(),
    names_to = "year",
    values_to = "population",
    names_prefix = "x"
  ) |>
  mutate(
    year = as.integer(year),
    across(starts_with("area"), as.factor)
  )

#glimpse(population_data_spreadsheet)
#View(population_data_spreadsheet)

# write-population-data --------------------------------------------------------
# Write CSV for human readable version
write_csv(
  population_data_spreadsheet,
  here::here("data", filename_population_scotland_csv)
)

# Write RDS for Shiny app
filename_population_scotland_rds <- stringr::str_replace(
  filename_population_scotland_csv, ".csv", ".rds")
write_rds(
  population_data_spreadsheet,
  here::here("data", filename_population_scotland_rds)
)

rm(population_data_spreadsheet)
rm(population_data_spreadsheet_raw)

