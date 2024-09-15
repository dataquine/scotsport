# ScotSport - sports facilities in Scotland

## About app

[Sportscotland](https://sportscotland.org.uk/) - the national agency for sport
in Scotland has recorded the locations of sports facilities in Scotland and made
that data available as an 
[open data dataset](https://data.spatialhub.scot/dataset/sports_facilities-unknown). 
This app uses the data to show how many, where and what types of facility are 
available. This data has been processed and combined with other geographical and
population information. Users can see locations on a map or search a table of 
addresses. Some minor changes have been made to facilities information for 
presentation purposes and consistency. [1]

### Important

Please note that some of these facilities may be located on school/hotel grounds
or are run by private clubs. Not every facility listed will be easily 
accessible to the general public. Many will require payment or membership fees
to use. 

Accuracy of facilities information provided cannot be guaranteed so 
please do further research before planning a visit.

### Facility types

| Type                  | Includes                                                  |
|-------------------|-----------------------------------------------------|
| Athletics Tracks      | velodromes, training areas, indoor and outdoor |
| Bowling Greens        | croquet, petanque and cricket squares          |
| Fitness Suites        |                                                          |
| Golf Courses          |                                                          |
| Ice Rinks             | curling rinks                                  |
| Pitches               | size, sport and type                           |
| Sports Halls          | gyms and other types                           |
| Squash Court          |                                                          |
| Swimming Pools        | diving and other types                         |
| Indoor Tennis Courts  |                                                          |
| Outdoor Tennis Courts |                                                          |

### Data sources:

-   **Sports facilities:** [Sports Facilities - Scotland](https://data.spatialhub.scot/dataset/sports_facilities-unknown), Sportscotland, Improvement Service,    
[UK Open Government Licence (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

-   **Council areas geospatial:** [Local Authority Boundaries - Scotland](https://data.spatialhub.scot/dataset/local_authority_boundaries-is), Improvement Service,  
[UK Open Government Licence (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

-   **Scottish population:** [Population Estimates Time Series Data](https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/population-estimates-time-series-data), National Records of Scotland,  
Crown copyright, [UK Open Government Licence (OGL)](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

## About author

**Author:** Lesley Duff AKA Data Quine. Scottish data analyst and developer. At
time of initial publication, September 2024, I am seeking data-related 
employment in Glasgow or fully remote UK.

**Contact:** 
[LinkedIn](https://www.linkedin.com/in/lesleyduff/) | 
**Mastodon:** [datasci.social/\@scottish](https://datasci.social/@scottish) | 
**GitHub:** [dataquine](https://github.com/dataquine)

## Background

This started as a personal learning project for building my data 
cleaning/wrangling skills. Having previously worked in education I thought 
details of venues would be helpful for e.g. anyone working with young people or 
community groups looking for venues for different sports or health workers 
'social prescribing' activity to others for health reasons helping folk find 
places nearby. A public-facing website would be a convenient way of accessing 
this data so that's why I chose [Quarto](https://quarto.org/) to publish a 
website version. There are several thousand facilties records. For ease of use 
these can be browsed by geographical council areas/towns or accessed via 
searchable tables. This app uses [R Shiny](https://shiny.posit.co/) to provide 
interactivity.

### Factoid

If you are Scottish and your first thought on seeing the word *'Scotsport'* 
was to think of a man in chequered sports jacket...then you are older than you 
look 😊

[Scotsport](https://en.wikipedia.org/wiki/Scotsport) also used to be the name 
of a much-loved sports show on STV in Scotland mostly covering football. For 
many of its earlier years hosted by the late, great 
[Arthur Montford](https://en.wikipedia.org/wiki/Arthur_Montford). 

His jacket was **legendary**. 

Hear from [Arthur himself and see the faimed jaiket in black and white on YouTube](https://youtu.be/z3vt89S-ogs?si=p8rDxXLqeaVj8L3j&t=1175)

![Logo of this site is the Houndstooth pattern in Arthur's honour](images/favicon-32x32.png "Houndstooth") Logo of this site is the Houndstooth pattern in Arthur's honour.

Houndstooth By <a href="//commons.wikimedia.org/wiki/User:Kotivalo" title="User:Kotivalo">Kotivalo</a> - <span class="int-own-work" lang="en">Own work</span>, <a href="https://creativecommons.org/licenses/by-sa/3.0" title="Creative Commons Attribution-Share Alike 3.0">CC BY-SA 3.0</a>, <a href="https://commons.wikimedia.org/w/index.php?curid=33604186">Link</a>

## Technical

To run this application locally you will require R and Quarto

### Installation

Clone the repository and

```         
quarto render
```

This should open a web page of the dashboard application.

### Source data

If you wish to download the original source data used by this project you can run the original cleaning scripts by running the following Quarto profile. This will try and download the source data to the `data-raw` folder.

```         
quarto render --profile download
```
---

[1] Please note the author of this app has no connection with the Sportscotland 
organisation.
