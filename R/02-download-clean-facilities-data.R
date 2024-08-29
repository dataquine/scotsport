# File: 02-download-clean-facilities-data.R
# Author: Lesley Duff
# Date created: 2024-08-29
# Description:
#    Download sports facilities data from Spatial Hub Scotland

# Data Source

# Spatial Hub Scotland: Sports Facilities - Scotland
# https://data.spatialhub.scot/dataset/sports_facilities-unknown

# The dataset is provided under Open Government Licence (OGL)
# https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
# for download and use. You are free to copy, publish, distribute and transmit
# the information as long as you acknowledge the source as coming from
# Improvement Service under OGL.

# Please note: while the Improvement Service shall make reasonable efforts
# ("Best Endeavours") to collect and provide accurate and up-to-date data, the
# Improvement Service does not guarantee the accuracy, completeness, or
# reliability of the Data provided. By accessing the data you acknowledge that
# the data may contain errors, omissions, or inconsistencies, and use the data
# at your own risk.

# Open Government Licence (OGL)
# https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#

# This dataset contains multiple themed layers of different types of sporting
# facilities across Scotland. The facilities are represented as point locations,
# which have been captured against Google Maps (along with attributes and
# details about them). Where there are multiple sports facilities at the same
# location there will be multiple points, meaning that calculations of total
# numbers of facilities can be calculated easily.

# Where there were missing coordinates for some facilities, manual lookups of
# appropriate coordinates have been made. Also, a judgement has been made to
# delete some provided facilities where they appear to no longer exist (for
# whatever reason).

# Provided layers are:
#  Athletics Tracks (incl velodromes, training areas, indoor and outdoor)
# Bowling Greens (incl croquet, petanque and cricket squares)
#  Fitness Suites
#  Golf Courses
# Ice Rinks (incl curling rinks)
#  Pitches (incl size, sport and type)
#  Sports Halls (incl gyms and other types)
#  Squash Court
#  Swimming Pools (incl diving and other types)
#  Indoor Tennis Courts
#  Outdoor Tennis Courts
#  Please note that the information provided is provided by third parties and
# therefore we cannot guarantee its accuracy, but it is the most up to date
# information we hold.
# As part of our work to keep this data up to date, we would kindly request
# that if you identify any issues, you share this with sportscotland at
# facilities\@sportscotland.org.uk

# Libraries --------------------------------------------------------------------
library(dplyr)
library(glue)
library(here)
library(purrr)
library(readr)
library(stringr)
library(tibble)

# Constants --------------------------------------------------------------------
url_facilities_data_source <- "https://data.spatialhub.scot/dataset/sports_facilities-unknown"

# Helper table with URLs for downloading
df_facility_type_info <- tribble(
  ~id, ~slug, ~facility_type, ~facility_type_desciption, ~url,
  "pub_spfat", "athletics-tracks", "Athletics Tracks", "includes velodromes, training areas, indoor and outdoor", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfat&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid",
  "pub_spfbg", "bowling-greens", "Bowling Greens", "includes croquet, petanque and cricket squares", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfbg&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid",
  "pub_spffs", "fitness-suites", "Fitness Suites", "", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spffs&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid",
  "pub_spfgc", "golf-courses", "Golf Courses", "", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfgc&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid",
  "pub_spfir", "ice-rinks", "Ice Rinks", "includes curling rinks", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfir&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid",
  "pub_spfp", "pitches", "Pitches", "includes size, sport and type", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfp&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid,surface",
  "pub_spfsh", "sports-halls", "Sports Halls", "includes gyms and other types", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfsh&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,fid,x,y,fac_sub_type",
  "pub_spfsc", "squash-courts", "Squash Courts", "", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfsc&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid",
  "pub_spfsp", "swimming-pools", "Swimming Pools", "includes diving and other types", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfsp&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,la_name,town,eastings_x,northing_y,fid,fac_sub_type",
  "pub_spfitc", "tennis-courts-indoor", "Tennis Courts (Indoor)", "", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfitc&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid,site_id,facid,management_type,ownership_category,facility_status,date_opened,flood_lights,surface",
  "pub_spfotc", "tennis-courts-outdoor", "Tennis Courts (Outdoor)", "", "https://geo.spatialhub.scot/geoserver/ext_spf/wfs?service=WFS&authkey=964e5ffb-92a9-47da-9dd0-c634eb1c7364&request=GetFeature&typeName=ext_spf:pub_spfotc&format_options=filename:Sports_Facilities_-_Scotland&outputFormat=csv&propertyName=sh_date_uploaded,site_name,address,postcode,la_name,town,eastings_x,northing_y,facility_sub_type,fid"
)

filename_scotland_sports_facilities_csv <- "scotland-sports-facilities.csv"

# Downloading ------------------------------------------------------------------
DO_DOWNLOAD <- TRUE



#' download_facility_data
#'
#' @param slug Shortened version of type
#' @param url URL the CSV data is being downloaded from
#' @param path_destfile Path to the CSV file being stored
#'
#' @return download_code integer
download_facility_data <- function(slug, url, path_destfile) {
  bs <- basename(path_destfile)
  print(glue::glue("Downloading [{slug}] url to ({bs})"))

  # Delay in seconds after download
  delay <- 1
  download_code <- NULL
  tryCatch(
    {
      # Download the facilities data CSV file
      download_code <- 1 # download.file()
    },
    error = function(e) {
      print(glue::glue("Problem downloading URL: {url}: {e}"))
    },
    # Be nice to servers and space out requests by a few seconds
    finally = Sys.sleep(delay)
  )
  return(download_code)
}

# Create list of urls with the CSV files to be downloaded
df_download_files <- df_facility_type_info |>
  select(slug, url) |>

  # Create the full path to the CSV in the raw directory
  mutate(path_destfile = here::here("data-raw", paste0(slug, ".csv")))

# test with one row
# df_download_files <- df_download_files[1,]

if (DO_DOWNLOAD) {
  # For every row in dataframe, download the facilities data CSV file
  purrr::pwalk(df_download_files, download_facility_data)
}

rm(df_download_files)

# build-downloaded-file-list ---------------------------------------------------

# Build the list of successfully downloaded facility CSV files
csv_downloaded_files <- list.files(here::here("data-raw", "csv"),
                                   pattern = "*.csv",
                                   full.names = TRUE
)

# read-facilties-data ----------------------------------------------------------

# Build list of dataframes from reading in facility CSV data
# N.B these files have many columns in common but some have
# extra information or suspected mistakes in naming which we will clean manually

build_slug_from_filename <- function(path_file) {
  stringr::str_remove(basename(path_file), "\\.csv$")
}


# Read the batch of CSV files into one dataframe
df_scotland_sports_facilities_raw <- purrr::map(csv_downloaded_files,
                                         ~ readr::read_csv(.x))

# Label each list with the slug
names(df_scotland_sports_facilities_raw) <-
  build_slug_from_filename(csv_downloaded_files)

# Fix Sports Hall entries
# map(df_scotland_sports_facilities_raw, names)

# combine list elements into a data frame by row
df_scotland_sports_facilities_combined <-
  purrr::list_rbind(df_scotland_sports_facilities_raw, names_to = "slug")
rm(df_scotland_sports_facilities_raw)
