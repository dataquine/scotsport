# File: 03-download-clean-council-boundary-data.R
# Author: Lesley Duff
# Date created: 2024-09-08
# Description:
#    Download council boundary data from Spatial Hub Scotland

# Data Source

# Spatial Hub Scotland: Local Authority Boundaries - Scotland
# https://data.spatialhub.scot/dataset/local_authority_boundaries-is

# Data Provided by
#   Improvement Service

# Open Government Licence (OGL)
# https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/


# Libraries --------------------------------------------------------------------
library(dplyr)
library(glue)
library(here)
library(sf)
library(readr)

# Constants --------------------------------------------------------------------
# URL on the Spatial Hub
url_council_boundary_data_source <- "https://geo.spatialhub.scot/geoserver/sh_las/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=sh_las:pub_las&format_options=filename:Local_Authority_Boundaries_-_Scotland&outputFormat=application/json"

filename_council_bounary_json <- "scotland-council-boundary.json"
filename_council_bounary_csv <- "scotland-council-boundary.csv"
filename_council_bounary_rds <- "scotland-council-boundary.rds"

# Downloading ------------------------------------------------------------------

# For switching off during dev
DO_DOWNLOAD <- TRUE

#' Download the JSON file containing council boundary info
#' includes size in hectares
#'
#' @param url URL the council boundary data is being downloaded from
#' @param path_destfile Path to where the JSON file being stored
#'
#' @return download_code integer
download_council_boundary_data <- function(url, path_destfile) {
  print(glue::glue("Downloading council boundary data to ({path_destfile})"))

  download_code <- 0
  tryCatch(
    {
      # Download the facilities data CSV file
      download_code <- download.file(url, path_destfile)
    },
    error = function(e) {
      print(glue::glue("Problem downloading URL: {url}: {e}"))
    }
  )
  return(download_code)
}

path_council_boundary_destfile <- here::here(
  "data-raw",
  filename_council_bounary_json
)

if (DO_DOWNLOAD) {
  # For every row in dataframe, download the facilities data CSV file
  download_council_boundary_code <-
    download_council_boundary_data(url_council_boundary_data_source,
                                   path_council_boundary_destfile)
} else {
  download_council_boundary_code = 0
}

# On successful download
if (download_council_boundary_code == 0) {
  # read-council-boundary-data -------------------------------------------------
  df_scotland_council_boundary_clean <-
    sf::st_read(path_council_boundary_destfile) |>

    # Reduce the size of the boundary file tolerance = 1000 metres
    sf::st_simplify(dTolerance = 1000)

  # Make the Western Isles consistent with the facility data version
  df_scotland_council_boundary_clean$local_authority <-
    dplyr::case_match(df_scotland_council_boundary_clean$local_authority,
             "Na h-Eileanan an Iar" ~ "Na h-Eileanan Siar",
             .default = df_scotland_council_boundary_clean$local_authority)

#  View(df_scotland_council_boundary_clean)

  # write-council-boundary-data ------------------------------------------------
  # Write human readable CSV
  readr::write_csv(
    df_scotland_council_boundary_clean,
    here::here("data", filename_council_bounary_csv)
  )

  # Write RDS for Shiny app
  readr::write_rds(
    df_scotland_council_boundary_clean,
    here::here("data", filename_council_bounary_rds)
  )
}
