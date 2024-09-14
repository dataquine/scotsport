# File: global.R
# Author: Lesley Duff
# Date created: 2024-08-31
# Description:
#  Read in clean data from Sport Scotland and National Records of Scotland

# Libraries --------------------------------------------------------------------

library(bslib) # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'
library(bsicons) # Easily Work with 'Bootstrap' Icons
library(dplyr) # A Grammar of Data Manipulation
library(forcats) # Tools for Working with Categorical Variables (Factors)
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggtext) # Improved Text Rendering Support for 'ggplot2'
library(glue) # Interpreted String Literals
library(gt) # Easily Create Presentation-Ready Display Tables
library(here) # A Simpler Way to Find Your Files
library(htmltools) # Tools for HTML
library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet' Library
library(mapview) # Interactive Viewing of Spatial Data in R
library(readr) # Read Rectangular Text Data
library(markdown) # Render Markdown with 'commonmark'
library(scales) # Scale Functions for Visualization
library(sf) # Simple Features for R
library(shiny) # Web Application Framework for R
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(tidyr) # Tidy Messy Data
library(viridis) # Colorblind-Friendly Color Maps for R


# Constants --------------------------------------------------------------------

filename_scotland_sports_facilities_csv <- "scotland-sports-facility.csv"
filename_population_rds <- "scotland-population.rds"
filename_council_bounary_rds <- "scotland-council-boundary.rds"

caption_source_sport_scotland <- "Sport Scotland"

# Read data --------------------------------------------------------------------

df_sports_facilities_scotland <- readr::read_csv(
  here::here("data", filename_scotland_sports_facilities_csv)
)

df_population_scotland <- readr::read_rds(
  here::here("data", filename_population_rds)
)

# Geometry of council areas and hectares
df_council_boundaries <- readr::read_rds(here::here(
  "data",
  filename_council_bounary_rds
)) |>
  select(local_authority, hectares, geometry)


# Calculated values ------------------------------------------------------------

# How many facilities in the whole of Scotland?
total_facilities_scotland <- nrow(df_sports_facilities_scotland)

# What date is Sport Scotland's most recently uploaded data?
latest_update <- max(df_sports_facilities_scotland$date_updated)

# Date to show to user
#date_updated <- get_date(latest_update)
#format(latest_update, format = "%d %B %Y")

# Unique council area names ----------------------------------------------------
council_areas <- df_sports_facilities_scotland |>
  distinct(la_name) |>
  arrange(la_name) |>
  pull()

# Unique town names ----------------------------------------------------
town_names <- df_sports_facilities_scotland |>
  count(town) |>
  drop_na() |> # Need more data cleaning to identify missing towns
  arrange(town)

  list_town_names <-  town_names$town
  names(list_town_names) = paste0(town_names$town, " (", town_names$n,")")

# Population -------------------------------------------------------------------
population_country <- df_population_scotland |>
  filter(area_type == "Country")

# Get population of each council area
population_council <- df_population_scotland |>
  filter(area_type == "Council area") |>
  select(-area_type, -year)

get_date <- function(str_date) {
  format(str_date, format = "%d %B %Y")
}

# Date that faciities dataset was last updated
date_updated <- get_date(latest_update)
