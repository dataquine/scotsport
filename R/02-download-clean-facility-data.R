# File: 02-download-clean-facility-data.R
# Author: Lesley Duff
# Date created: 2024-08-29
# Description:
#    Download sports facilities data from Spatial Hub Scotland

# Data Source

# Spatial Hub Scotland: Sports Facilities - Scotland
# https://data.spatialhub.scot/dataset/sports_facilities-unknown

# N.B. The following comment text is taken from the above web page for
# reference purposes only during development

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
# facilities@sportscotland.org.uk

# Libraries --------------------------------------------------------------------
library(dplyr) # A Grammar of Data Manipulation
library(glue) # Interpreted String Literals
library(here) # A Simpler Way to Find Your Files
library(purrr) # Functional Programming Tools
library(readr) # Read Rectangular Text Data
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(tibble) # Simple Data Frames

# Constants --------------------------------------------------------------------
url_facility_data_source <- "https://data.spatialhub.scot/dataset/sports_facilities-unknown"

# Helper table with URLs for downloading the CSV versions directly
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

filename_scotland_sports_facility_csv <- "scotland-sports-facility.csv"

# Downloading ------------------------------------------------------------------

# For switching off during dev
DO_DOWNLOAD <- TRUE

#' download_facility_data
#'
#' @param slug Shortened version of type
#' @param url URL the CSV data is being downloaded from
#' @param path_destfile Path to the CSV file being stored
#'
#' @return download_code integer
download_facility_data <- function(slug, url, path_destfile) {
  bs <- here::here("data-raw", basename(path_destfile))
  print(glue::glue("Downloading [{slug}] url to ({bs})"))

  # Delay in seconds after download
  delay <- 15
  download_code <- NULL
  tryCatch(
    {
      # Download the facilities data CSV file
      download_code <- download.file(url, path_destfile)
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
df_download_facility_files <- df_facility_type_info |>
  select(slug, url) |>
  # Create the full path to the CSV in the raw directory
  mutate(path_destfile = here::here("data-raw", "facility",
    paste0(slug, ".csv")
  ))

# test with one row
# df_download_facility_files <- df_download_facility_files[1,]

if (DO_DOWNLOAD) {
  # For every row in dataframe, download the facilities data CSV file
  purrr::pwalk(df_download_facility_files, download_facility_data)
}

rm(df_download_facility_files)

# build-downloaded-file-list ---------------------------------------------------

# Build the list of successfully downloaded facility CSV files
csv_downloaded_files <- list.files(here::here("data-raw", "facility"),
  pattern = "*.csv",
  full.names = TRUE
)

# read-facilities-data ---------------------------------------------------------

# Build list of dataframes from reading in facility CSV data
# N.B these files have many columns in common but some have
# extra information or suspected mistakes in naming which we will clean manually

# Read the batch of CSV files into one dataframe
df_scotland_sports_facility_raw <- purrr::map(
  csv_downloaded_files,
  ~ readr::read_csv(.x)
)

build_slug_from_filename <- function(path_file) {
  stringr::str_remove(basename(path_file), "\\.csv$")
}

# Label each list with the slug
names(df_scotland_sports_facility_raw) <-
  build_slug_from_filename(csv_downloaded_files)

# combine list elements into a data frame by row
df_scotland_sports_facility_combined <-
  purrr::list_rbind(df_scotland_sports_facility_raw, names_to = "slug")

rm(df_scotland_sports_facility_raw)

# clean-facilities-data --------------------------------------------------------

# Fix inconsistencies in column
df_scotland_sports_facility_clean <- df_scotland_sports_facility_combined |>
  # Fix Sports Hall entries - some inconsistent column names
  mutate(
    facility_sub_type =
      if_else(slug == "sports-halls", fac_sub_type, facility_sub_type),
    eastings_x = if_else(slug == "sports-halls", x, eastings_x),
    northing_y = if_else(slug == "sports-halls", y, northing_y)
  ) |>
  # Fix Swimming Pools entries - an inconsistent column name
  mutate(
    facility_sub_type =
      if_else(slug == "swimming-pools", fac_sub_type, facility_sub_type)
  ) |>
  # Title Case (will handle exceptions later)
  mutate(town_clean = stringr::str_to_title(town), .after = town) |>
  # Remove misnamed columns
  select(-fac_sub_type, -x, -y) |>
  # Rename the unique id column FID to id as there may be confusions between
  # FID = globally unique among all facility types
  # fid = unique number within one facility type
  rename(id = FID, date_updated = sh_date_uploaded, name = site_name) |>
  relocate(slug, .after = id)

# Standardise Towns
df_scotland_sports_facility_clean <- df_scotland_sports_facility_clean |>
  mutate(
    town_clean = stringr::str_replace(town_clean, " Of ", " of "),
    town_clean = stringr::str_replace(town_clean, "(^By )(.*)", "\\2 (Near)"),
    town_clean = stringr::str_replace(town_clean, "(^Nr )(.*)", "\\2 (Near)"),

    # Individual exceptions
    town_clean = stringr::str_replace(town_clean, "Berwick-Upon-Tweed",
                                      "Berwick-upon-Tweed"),
    town_clean = stringr::str_replace(town_clean, "Grantown On Spey",
                                      "Grantown-on-Spey"),
    town_clean = stringr::str_replace(town_clean, "Grantown-On-Spey",
                                      "Grantown-on-Spey"),
    town_clean = stringr::str_replace(town_clean, "Newport-On-Tay",
                                      "Newport-on-Tay"),
    town_clean = stringr::str_replace(town_clean, "Star, Markinch",
                                      "Star of Markinch"),
    town_clean = stringr::str_replace(town_clean, "Cambeltown \\(Near\\)",
                                      "Campbeltown (Near)"),

    # N.B. The following town clean are quick hacks according to initial manual
    # inspection. Later build more robustness isolating by id

    # These are local authority names rather than towns
    # tread carefully as could be duplicates
    # Dumfries And Galloway, think these are both for Bannatynes - Dumfries
    # pub_spffs.155 and pub_spfsp.174
    town_clean = stringr::str_replace(town_clean, "Dumfries And Galloway",
                                      "Campbeltown (Near)"),

    # Dunbartonshire - these seem like for Clydebank High School, 2 Janetta Street, G81 3EJ
    town_clean = stringr::str_replace(town_clean, "Dunbartonshire",
                                      "Clydebank"),

    # East Lothian - Humbie Village Hall, Kippithill, EH36 5PJ
    town_clean = stringr::str_replace(town_clean, "East Lothian", "Humbie"),

    # Trailing comma
    # pub_spfp.625, Whitehills Primary School Fyfe Street DD8 3EQ, pitches
    town_clean = stringr::str_replace(town_clean, "Forfar,", "Forfar"),

    # pub_spfotc.1349 Stow Primary School, tennis-courts-outdoor, Station Yard,TD1 2SQ
    town_clean = stringr::str_replace(town_clean, "Galasheils", "Galashiels"),

    # pitches, Over Stenton Playing Field, Viewfield, KY6 2NG
    town_clean = stringr::str_replace(town_clean, "Glenothes", "Glenrothes"),

    # pub_spfp.1219 pitches,Stornoway Primary School, HS1 2LF
    town_clean = stringr::str_replace(town_clean, "Jamieson Drive",
                                      "Stornoway"),

    # Side effect of Title case
    # pub_spfsh.2612 sports-halls, Kincardine O'Neil School, North Deeside Road, AB34 5AB
    town_clean = stringr::str_replace(town_clean, "Kincardine O'neil",
                                      "Kincardine O'Neil"),

    # pub_spfp.2195, pitches, Balmullo Primary School, Hayston Park, KY16 0DH
    town_clean = stringr::str_replace(town_clean, "Leuchars St Andrews",
                                      "St Andrews"),

    # pub_spfp.584, pitches, Monikie Primary School, DD5 3QN
    town_clean = stringr::str_replace(town_clean, "Monikie,", "Monikie"),

    # pub_spfsh.402, sports-halls, Kirklandneuk Primary School, Ard Road, PA4 9DA
    town_clean = stringr::str_replace(town_clean, "Renfrew\\?", "Renfrew"),

    # pub_spfgc.956, golf-courses, Asta Golf Club, ZE1 0UQ
    town_clean = stringr::str_replace(town_clean, "Shetland, Scotland",
                                      "Shetland"),

    # pub_spfp.3098, pitches,Smithton Primary School, 59 Smithton Park, IV2 7PD
    town_clean = stringr::str_replace(town_clean, "Smithton, Inverness",
                                      "Smithton"),

    # pub_spfp.2186, pitches,Arncroach Football Pitch,ARNCROACH,KY10 2RW
    town_clean = stringr::str_replace(town_clean, "St Monance \\(Near\\)",
                                      "Arncroach"),

    # pub_spfp.606, pitches, Strathmartine Primary School, DD3 0PH
    town_clean = stringr::str_replace(town_clean, "Strathmartine,",
                                      "Bridgefoot"),

    # pub_spfitc.54, Morton of Pitmilly Resort, Morton of Pitmilly Countryside Resort, Kingsbarns, KY16 8QF
    management_type = ifelse(management_type == "Unknown", NA, management_type)
  ) |>
  rename(town_orig = town, town = town_clean)


# Fintry Sports Club (Strathendrick RFC)
# Local authority Glasgow City Should be Stirling?
df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Fintry Sports Club (Strathendrick RFC)",]$la_name <- "Stirling"


#   relocate(facility_type, .after = id) |>

df_facility_types <- df_facility_type_info |>
  select(slug, facility_type)

# Join facility types to CSV data dataframe
df_scotland_sports_facility_clean <- df_scotland_sports_facility_clean |>
  left_join(df_facility_types, by = join_by(slug)) |>
  relocate(facility_type, .after = id) |>
  relocate(facility_sub_type, .after = facility_type) |>
  mutate(
    # change & to and
    la_name = stringr::str_replace(la_name, "&", "and"),

    # To save screen estate we'll label columns with Council so don't need it
    # repeated

    # Remove the Gaelic version of council from the name
    la_name = stringr::str_replace(
      la_name, "Comhairle nan Eilean Siar",
      "Na h-Eileanan Siar"
    ),
    la_name = stringr::str_remove(la_name, " Council"),
    facility_sub_type = ifelse(is.na(facility_sub_type), "Unspecified",
      facility_sub_type
    )
  )

# Spatial information ----------------------------------------------------------
df_long_lat_facility <- df_scotland_sports_facility_clean |>
  # lng = eastings_x, lat = northing_y
  sf::st_as_sf(
    coords = c("eastings_x", "northing_y"),
    # coordinate reference system code for eastings/northings
    crs = 27700
  ) |>
  # coord ref system code for latitude longitude
  sf::st_transform(crs = 4326) |>
  sf::st_coordinates() |>
  as_tibble() |>
  rename(lng = X, lat = Y)

# Join the converted easting/northing data as long/lat columns
df_scotland_sports_facility_clean <-
  bind_cols(df_scotland_sports_facility_clean, df_long_lat_facility)

# For convenience move the calculated long lat columns to be next to
# the National Grid column
df_scotland_sports_facility_clean <- df_scotland_sports_facility_clean |>
  relocate(lng, .after = northing_y) |>
  relocate(lat, .after = lng) |>
  # Just make it easier if eyeballing the rows
  relocate(slug, .after = id) |>

  # Change categories to factors
  mutate_at(c("slug", "facility_type", "facility_sub_type", "la_name",
              "management_type", "ownership_category", "facility_status",
              "flood_lights"),
            as.factor)

# Do extra research on entries missing towns
# Xcite Bathgate, town = Bathgate, some have missing postcode too
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$name == "Xcite Bathgate"] <- "EH48 4LA"
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Xcite Bathgate"] <- "Bathgate"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Xcite Bathgate",]

# Archerfield Links, no town
# https://archerfieldgolfclub.com/
# https://en.wikipedia.org/wiki/Archerfield_Estate_and_Links
# Archerfield, Golf Green - Dirleton,East Lothian, EH39 5HU
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Archerfield Links"] <- "North Berwick (Near)"
df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Archerfield Links",]

# Stoneyburn Bowling Club
# https://www.stoneyburnbowlingclub.co.uk/, Main Street, Stoneyburn, EH47 8AU
# Bowling green,  Stoneyburn, Bathgate EH47 8BU
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Stoneyburn Bowling Club"] <- "Bathgate"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Stoneyburn Bowling Club",]

# North West Community Campus Primary, town = Dumfries
# https://www.dumgal.gov.uk/northwestcommunitycampus
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "North West Community Campus Primary"] <- "Dumfries"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "North West Community Campus Primary",]

# Cowan Park
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Cowan Park"] <- "Glasgow"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Cowan Park",]

# Bannatyne Health Club Inverness, town=Inverness, missing postcode in one
# https://www.bannatyne.co.uk/health-club/inverness, The Inshes Retail & Leisure Park, Inverness, IV2 3TW
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Bannatyne Health Club Inverness"] <- "Inverness"
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$name == "Bannatyne Health Club Inverness"] <- "IV2 3TW"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Bannatyne Health Club Inverness",]

# Football Ground - Balblair, town = near Dingwall
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Football Ground - Balblair"] <- "Dingwall (Near)"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Football Ground - Balblair",]

# St Timothys RC Primary School (there are two so using id)
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$id == "pub_spfp.3785"] <- "Coatbridge"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$id == "pub_spfp.3785",]

# Bishopton Primary School
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Bishopton Primary School"] <- "Bishopton"
df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Bishopton Primary School",]

# Aviemore Activity Centre it's part of a hotel, Sorry but our pool complex is not open to non-guests.
# https://www.macdonaldhotels.co.uk/aviemore/see-and-do/activities/swimming, Highland Resort, Aviemore PH22 1PN
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$name == "Aviemore Activity Centre"] <- "Aviemore"
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$name == "Aviemore Activity Centre"] <- "PH22 1PN"
df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Aviemore Activity Centre",]

# Abbeyhill Primary School, pub_spfsp.135, missing postcode - same as pub_spfsh.2074
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$id == "pub_spfsp.135"] <- "EH7 5SJ"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Abbeyhill Primary School",]

# Abercorn School (ASN),  missing postcode pub_spfsp.309 -same as pub_spfsh.661
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$id == "pub_spfsp.309"] <- "G4 9QH"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Abercorn School (ASN)",]

# Aberdeen Altens Hotel, pub_spfsp.17 missing postcode  -same as pub_spffs.1
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$id == "pub_spfsp.17"] <- "AB12 3LF"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Aberdeen Altens Hotel",]

# Aberdeen Aquatics Centre, postcode missing on two, Linksfield Road AB24 5RU
# https://www.aberdeensportsvillage.com/aquatics
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$name == "Aberdeen Aquatics Centre"] <- "AB24 5RU"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Aberdeen Aquatics Centre",]

# Aberdeen Grammar School,pub_spfsp.10  missing postcode  -same as pub_spfsh.2948
df_scotland_sports_facility_clean$postcode[df_scotland_sports_facility_clean$id == "pub_spfsp.10"] <- "AB10 1HT"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$name == "Aberdeen Grammar School",]

# Fix Town ---------------------------------------------------------------------

# Arisaig Playing Field,pub_spfp.2909, town name ARISAIGArisaig = Arisaig
#https://www.arisaigcommunitytrust.org.uk/projects/playing-fields/
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$id == "pub_spfp.2909"] <- "Arisaig"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$id == "pub_spfp.2909",]$town

# Auchterhouse Primary School, pub_spfp.513, stray comma after name in town
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$id == "pub_spfp.513"] <- "Auchterhouse"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$id == "pub_spfp.513",]$town

# Birkhill,, stray comma after name in town
df_scotland_sports_facility_clean$town[df_scotland_sports_facility_clean$id == "pub_spfp.514"] <- "Birkhill"
#df_scotland_sports_facility_clean[df_scotland_sports_facility_clean$id == "pub_spfp.514",]$town

#glimpse(df_scotland_sports_facility_clean)
#View(df_scotland_sports_facility_clean)

# write-clean-facility-data --------------------------------------------------
# Write out cleaned dataset CSV for human readability
write_csv(
  df_scotland_sports_facility_clean,
  here::here("data", filename_scotland_sports_facility_csv)
)

# Write out cleaned dataset RDS for performance in Shiny app - app should use
# this version
filename_scotland_sports_facility_rds <- stringr::str_replace(
  filename_scotland_sports_facility_csv, ".csv", ".rds")

write_rds(
  df_scotland_sports_facility_clean,
  here::here("data", filename_scotland_sports_facility_rds)
)

rm(df_scotland_sports_facility_combined)
rm(df_scotland_sports_facility_clean)
rm(df_facility_types)
rm(df_long_lat_facility)
rm(df_facility_type_info)

