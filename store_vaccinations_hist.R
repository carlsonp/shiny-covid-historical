library(simpleCache) # https://cran.r-project.org/web/packages/simpleCache/vignettes/simpleCacheIntroduction.html
library(tidyverse)
library(lubridate)
library(readr)

source(file="dataprep.R")

# store a historical version of the CDC vaccination data in the Git repo
# leverages Github actions

read_csv2("cdc_vaccinations_hist.csv") %>%
  dplyr::bind_rows(cdccovidvaccinations) %>%
  dplyr::distinct() %>%
  write_csv2("cdc_vaccinations_hist.csv")