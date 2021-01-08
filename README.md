# shiny-covid-historical

[![Actions Status](https://github.com/carlsonp/shiny-covid-historical/workflows/github-actions/badge.svg)](https://github.com/carlsonp/shiny-covid-historical/actions)

Very simple [Shiny](https://shiny.rstudio.com/) app for showing historical COVID-19 data in the US.

[Deployed on ShinyApps.io](https://carlsonp.shinyapps.io/shiny-covid-historical/)

## Development

* Install packages with `install.packages()`
* Creates cache files of data in the `cache` folder

## CI/CD

* Automated deployment via Github Actions, scheduled with data updates daily
* `SHINYAPPS_NAME`, `SHINYAPPS_SECRET`, and `SHINYAPPS_TOKEN` are repository secrets in Github where the
values include single quotes around them.  The name is the Github account username.

## Data

* [COVID-19 data from the Covid Tracking Project](https://covidtracking.com)
* [US State Populations](https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population)
* [NYTimes US Covid Data by County](https://github.com/nytimes/covid-19-data)
* [US Census County Population Estimates](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html)
* [US Census State and County Maps](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html)
* [US CDC Covid Vaccinations by State](https://covid.cdc.gov/covid-data-tracker/#vaccinations)
