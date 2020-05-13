library(simpleCache) # https://cran.r-project.org/web/packages/simpleCache/vignettes/simpleCacheIntroduction.html
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

loadDataREST <- function() {
  # https://covidtracking.com/api
  request <- httr::GET(url = "https://covidtracking.com/api/v1/states/daily.json")
  if (request$status_code == "200") {
    response <- content(request, as="text", encoding="UTF-8")
    df <- jsonlite::fromJSON(response, flatten=TRUE) %>% 
      data.frame()
    
    df$date <- lubridate::ymd(df$date)
    df$lastUpdateEt <- lubridate::mdy_hm(df$lastUpdateEt)
    return(df)
  }
}

setCacheDir("./cache")
if ((!file.exists('./cache/covid.RData') || as.numeric(difftime(Sys.time(), file.info('./cache/covid.RData')$mtime, units='hours')) > 6) && Sys.getenv("R_CONFIG_ACTIVE") != "shinyapps") {
  simpleCache('covid', loadDataREST(), recreate=TRUE)
  print("Recreating cache")
} else {
  simpleCache('covid')
  print("Loading data from cache")
}