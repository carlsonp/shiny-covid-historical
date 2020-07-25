# shiny-covid-historical

[![Build Status](https://travis-ci.com/carlsonp/shiny-covid-historical.svg?branch=master)](https://travis-ci.com/carlsonp/shiny-covid-historical)

Very simple [Shiny](https://shiny.rstudio.com/) app for showing historical COVID-19 data in the US.

[Deployed on ShinyApps.io](https://carlsonp.shinyapps.io/shiny-covid-historical/)

## Development

* Install packages with `install.packages()`
* Creates a cache file of data in the `cache` folder

## CI/CD

* [Automated deployment and data refreshing daily via Travis-CI cron job](https://travis-ci.com/github/carlsonp/shiny-covid-historical)
* `shinyapps_name`, `shinyapps_secret`, and `shinyapps_token` are environment variables in travis-ci

## Data

* [COVID-19 data from the Covid Tracking Project](https://covidtracking.com)
* [US State Populations](https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population)
* [NYTimes US Covid Data by County](https://github.com/nytimes/covid-19-data)
* [US Census County Population Estimates](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html)
* [US Census County Maps](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html)
