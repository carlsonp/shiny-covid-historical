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
