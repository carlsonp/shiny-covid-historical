library(simpleCache) # https://cran.r-project.org/web/packages/simpleCache/vignettes/simpleCacheIntroduction.html
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(readr)

loadVaccinations <- function() {
  # https://covid.cdc.gov/covid-data-tracker/#vaccinations
  request <- httr::GET(url = "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")
  if (request$status_code == "200") {
    response <- content(request, as="text", encoding="UTF-8")
    df <- jsonlite::fromJSON(response, flatten=TRUE) %>%
      data.frame()
    
    df$vaccination_data.Date <- lubridate::ymd(df$vaccination_data.Date)

    # https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population
    # Census population estimate July 1, 2019
    state_pop_df <- read.csv("state-populations-wikipedia.csv")
    state_pop_df$state <- as.character(state_pop_df$state)
    
    df <- dplyr::left_join(df, state_pop_df, by=c("vaccination_data.Location" = "state")) %>%
      dplyr::rename(abb=vaccination_data.Location) %>%
      dplyr::rename_all(
        funs(
          stringr::str_to_lower(.) %>%
          stringr::str_replace_all(., 'vaccination_data.', '')
        )
      )
    
    # https://stackoverflow.com/questions/5411979/state-name-to-abbreviation/5412122#5412122
    st_crosswalk <- tibble(state = state.name) %>%
      bind_cols(tibble(abb = state.abb)) %>%
      bind_rows(tibble(state = "District of Columbia", abb = "DC")) %>%
      bind_rows(tibble(state = "Puerto Rico", abb = "PR"))
    
    df <- dplyr::left_join(df, st_crosswalk, by=c("abb")) %>%
      dplyr::mutate(ratio_admin_dist = doses_administered / doses_distributed) %>%
      data.frame()
    
    return(df)
  }
}

loadDataREST <- function() {
  # https://covidtracking.com/api
  request <- httr::GET(url = "https://covidtracking.com/api/v1/states/daily.json")
  if (request$status_code == "200") {
    response <- content(request, as="text", encoding="UTF-8")
    df <- jsonlite::fromJSON(response, flatten=TRUE) %>% 
      data.frame()
    
    df$date <- lubridate::ymd(df$date)
    df$lastUpdateEt <- lubridate::mdy_hm(df$lastUpdateEt)
    
    # https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population
    # Census population estimate July 1, 2019
    state_pop_df <- read.csv("state-populations-wikipedia.csv")
    state_pop_df$state <- as.character(state_pop_df$state)
    
    df <- dplyr::left_join(df, state_pop_df, by=c("state" = "state")) %>%
      dplyr::select(-hash, -posNeg, -total) %>%
      dplyr::rename(abb=state)
    
    # https://stackoverflow.com/questions/5411979/state-name-to-abbreviation/5412122#5412122
    st_crosswalk <- tibble(state = state.name) %>%
      bind_cols(tibble(abb = state.abb)) %>%
      bind_rows(tibble(state = "District of Columbia", abb = "DC")) %>%
      bind_rows(tibble(state = "Puerto Rico", abb = "PR"))
    
    df <- dplyr::left_join(df, st_crosswalk, by=c("abb")) %>%
      data.frame()
    
    return(df)
  }
}

loadNYTimesCountyData <- function() {
  # https://github.com/nytimes/covid-19-data
  df <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  df$date <- lubridate::ymd(df$date)

  # https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
  # data dictionary:
  # https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/co-est2019-alldata.pdf
  census <- readr::read_csv("co-est2019-alldata.csv") %>% data.frame() %>%
    select(STATE, COUNTY, CTYNAME, POPESTIMATE2019) %>%
    mutate(COUNTY = as.character(COUNTY)) %>%
    mutate(STATE = as.character(STATE)) %>%
    mutate(fips = paste0(STATE, COUNTY)) %>%
    select(fips, CTYNAME, POPESTIMATE2019)
  
  # https://stackoverflow.com/questions/5411979/state-name-to-abbreviation/5412122#5412122
  st_crosswalk <- tibble(state = state.name) %>%
    bind_cols(tibble(abb = state.abb))
  
  df <- dplyr::left_join(df, st_crosswalk, by="state") %>%
    dplyr::left_join(census, by=c("fips")) %>%
    data.frame()
  
  return(df)
}

setCacheDir("./cache")
# the shinyapps check is an environment variable to show if we're running on shinyapps.io
if ((!file.exists('./cache/covid.RData') || as.numeric(difftime(Sys.time(), file.info('./cache/covid.RData')$mtime, units='hours')) > 6) && Sys.getenv("R_CONFIG_ACTIVE") != "shinyapps") {
  simpleCache('covid', loadDataREST(), recreate=TRUE)
  print("Recreating cache")
} else {
  simpleCache('covid')
  print("Loading data from cache")
}

if ((!file.exists('./cache/nytimescovidcounties.RData') || as.numeric(difftime(Sys.time(), file.info('./cache/nytimescovidcounties.RData')$mtime, units='hours')) > 6) && Sys.getenv("R_CONFIG_ACTIVE") != "shinyapps") {
  simpleCache('nytimescovidcounties', loadNYTimesCountyData(), recreate=TRUE)
  print("Recreating NYTimes county data cache")
} else {
  simpleCache('nytimescovidcounties')
  print("Loading NYTimes county data from cache")
}

if ((!file.exists('./cache/cdccovidvaccinations.RData') || as.numeric(difftime(Sys.time(), file.info('./cache/cdccovidvaccinations.RData')$mtime, units='hours')) > 6) && Sys.getenv("R_CONFIG_ACTIVE") != "shinyapps") {
  simpleCache('cdccovidvaccinations', loadVaccinations(), recreate=TRUE)
  print("Recreating CDC Covid vaccinations data cache")
} else {
  simpleCache('cdccovidvaccinations')
  print("Loading CDC Covid vaccinations data from cache")
}