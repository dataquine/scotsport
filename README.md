# ScotSport - sports facilities in Scotland

## About app

[Sport Scotland](https://sportscotland.org.uk/) - the national agency for sport
in Scotland has recorded the locations of sports venues and made an open data 
dataset available. This app uses the data to work out how many, where and what 
types are available. This data has been processed and combined with other 
geographical and population information for visualisation in this app. Some 
minor changes to names have been made for presentation purposes and consistency.
[1]

This includes:

- Athletics Tracks
  - including velodromes, training areas, indoor and outdoor

- Bowling Greens
  - including croquet, petanque and cricket squares

- Fitness Suites

- Golf Courses

- Ice Rinks
  - including curling rinks

- Pitches
  - including size, sport and type

- Sports Halls
  - including gyms and other types

- Squash Court

- Swimming Pools
  - including diving and other types

- Indoor Tennis Courts

- Outdoor Tennis Courts

**Important**

Please note that some of these facilities may be located on school/hotel grounds
or are run by private clubs. Not every facility listed will be easily accessible
to the general public. Many will require payment or membership fees to use. 
Accuracy of facilities information provided cannot be guaranteed so please do 
further research before planning a visit.

## Data sources

-   **Sports facilities:** [Sports Facilities - Scotland](https://data.spatialhub.scot/dataset/sports_facilities-unknown) 
([UK Open Government Licence (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/))

-   **Council areas geospatial:** [Local Authority Boundaries - Scotland](https://data.spatialhub.scot/dataset/local_authority_boundaries-is), 
Improvement Service, ([UK Open Government Licence (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/))

-   **Scottish population:** [Population Estimates Time Series Data](https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/population-estimates-time-series-data) (National Records of Scotland, Crown copyright, [UK Open Government Licence (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/))

## About author

**Author:** Lesley Duff AKA Data Quine. Scottish data analyst and developer. At 
time of initial publication September 2024 I am seeking data-related employment 
in Glasgow or fully remote.

**Contact:** [LinkedIn](https://www.linkedin.com/in/lesleyduff/) \| Mastodon: [datasci.social/\@scottish](https://datasci.social/@scottish) \| GitHub: [dataquine](https://github.com/dataquine)

## Background

This started as a personal learning project for building my data 
cleaning/wrangling skills. Having previously worked in education I thought 
details of venues would be helpful for e.g. anyone working with young people or 
community groups looking for venues for different sports or health workers 
'social prescribing' activity to others for health reasons helping folk find 
places nearby. A public-facing website would be a convenient way of accessing 
this data so that's why I chose [Quarto](https://quarto.org/) to publish a 
website version. There are several thousand facilties records, for ease 
of use these can be browsed by geographical council areas/towns or accessed via 
searchable tables. This app uses [R Shiny](https://shiny.posit.co/) to provide 
interactivity.

## Technical

To run this application locally you will require R and Quarto

### Installation

Clone the repository and

```         
quarto render
```

This should open a web page of the dashboard application.

### Source data

If you wish to download the original source data used by this project you can 
run the original cleaning scripts by running the following Quarto profile. This 
will try and download the source data to the `data-raw` folder.

```         
quarto render --profile download
```

[1]: The author of this app has no connection with the Sport Scotland 
organisation.
