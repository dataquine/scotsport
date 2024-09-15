# File: global.R
# Author: Lesley Duff
# Date created: 2024-08-31
# Description:
#  Read in clean data from Sport Scotland, Spatial Hub Scotland and National
# Records of Scotland

# Libraries --------------------------------------------------------------------

suppressPackageStartupMessages({
  library(bslib) # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'
  library(bsicons) # Easily Work with 'Bootstrap' Icons
  library(dplyr) # A Grammar of Data Manipulation
  library(forcats) # Tools for Working with Categorical Variables (Factors)
  library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of
  # Graphics
  library(ggtext) # Improved Text Rendering Support for 'ggplot2'
  library(glue) # Interpreted String Literals
  library(gt) # Easily Create Presentation-Ready Display Tables
  library(here) # A Simpler Way to Find Your Files
  library(htmltools) # Tools for HTML
  library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet'
  #Library
  library(leaflet.extras) # Extra Functionality for 'leaflet' Package
  library(readr) # Read Rectangular Text Data
  library(markdown) # Render Markdown with 'commonmark'
  library(scales) # Scale Functions for Visualization
  library(sf) # Simple Features for R
  library(shiny) # Web Application Framework for R
  library(showtext) # Using Fonts More Easily in R Graphs
  library(sysfonts) # Loading Fonts into R
  library(stringr) # Simple, Consistent Wrappers for Common String Operations
  library(tidyr) # Tidy Messy Data
  library(viridis) # Colorblind-Friendly Color Maps for R
})

# Constants --------------------------------------------------------------------

filename_sports_facilities_rds <- "scotland-sports-facility.rds"
filename_population_rds <- "scotland-population.rds"
filename_council_bounary_rds <- "scotland-council-boundary.rds"

caption_source_sport_scotland <- "Sport Scotland"

# Read data --------------------------------------------------------------------

# Sportscotland facilities data
df_sports_facilities_scotland <- readr::read_rds(
  here::here("data", filename_sports_facilities_rds)
)

# National Records of Scotland population data
df_population_scotland <- readr::read_rds(
  here::here("data", filename_population_rds)
)

# Spatial Hub Scotland geometry of council areas and hectares
df_council_boundaries <- readr::read_rds(here::here(
  "data",
  filename_council_bounary_rds
)) |>
  dplyr::select(local_authority, hectares, geometry)

# variables and helper functions -----------------------------------------------

# What date is Sport Scotland's most recently uploaded data?
latest_update <- max(df_sports_facilities_scotland$date_updated)

# Unique council area names ----------------------------------------------------
council_areas <- df_sports_facilities_scotland |>
  dplyr::distinct(la_name) |>
  arrange(la_name) |>
  dplyr::mutate(la_name <- as.character(la_name)) |>
  dplyr::pull()

# Population -------------------------------------------------------------------
population_country <- df_population_scotland |>
  dplyr::filter(area_type == "Country")

# Get population of each council area
population_council <- df_population_scotland |>
  dplyr::filter(area_type == "Council area") |>
  dplyr::select(-area_type, -year)

# Helper functions -------------------------------------------------------------
# for consistent date formatting

get_date <- function(str_date) {
  format(str_date, format = "%d %B %Y")
}

# Date that facilities dataset was last updated
date_updated <- get_date(latest_update)

# Font -------------------------------------------------------------------------

scotsport_default_font_family <- "lato"
scotsport_default_font_size <- 14

# Loading Google fonts (https://fonts.google.com/)
sysfonts::font_add_google("Lato", scotsport_default_font_family)

scotsport_table_font_family <- "Lato"

# Automatically use showtext to render text
showtext::showtext_auto()

# Colours ----------------------------------------------------------------------

# create a custom theme for bslib valueboxes
scotsport_text_colour <- "#000099"
scotsport_vbbackground_colour <- "#e6f2fd"
scotsport_value_box_theme <- bslib::value_box_theme(
  fg = scotsport_text_colour,
  bg = scotsport_vbbackground_colour
)
