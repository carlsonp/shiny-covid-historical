library(shiny)
library(tidyverse)
library(simpleCache) # https://cran.r-project.org/web/packages/simpleCache/vignettes/simpleCacheIntroduction.html
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(DT)
library(visdat)

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
if (!file.exists('./cache/covid.RData') || as.numeric(difftime(Sys.time(), file.info('./cache/covid.RData')$mtime, units='hours')) > 6) {
  simpleCache('covid', loadDataREST(), recreate=TRUE)
  print("Recreating cache")
} else {
  simpleCache('covid')
  print("Loading data from cache")
}

# https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population
# Census population estimate July 1, 2019
state_pop_df <- read.csv("state-populations-wikipedia.csv")
state_pop_df$state <- as.character(state_pop_df$state)

covid <- dplyr::left_join(covid, state_pop_df, by=c("state" = "state")) %>%
  dplyr::select(-hash, -posNeg, -total)
rm(state_pop_df)

shinyServer(function(input, output, session) {
  
  filtered_df <- reactive({
    return(
      covid %>%
        dplyr::filter(state %in% input$stateFilter)
    )
  })
  
  output$state_filter <- renderUI({
    pickerInput("stateFilter",
      label = "US States:",
      choices = unique(covid$state),
      selected = unique(covid$state),
      options = list(
        `actions-box` = TRUE, 
        size = 10
      ), 
      multiple = TRUE
    )
  })
  
  output$data_as_of <- renderUI({
    tags$p(paste("Data as of:", max(filtered_df()$date)))
  })
   
  output$us_daily_deaths_graph <- renderPlotly({
    filtered_df() %>%
      group_by(date) %>%
      summarize(dailyDeaths = sum(deathIncrease, na.rm=T)) %>%
      dplyr::filter(!is.na(dailyDeaths)) %>%
    plot_ly(x = ~date, y = ~dailyDeaths, type = "scatter", mode = "lines") %>%
      layout(title = "Daily Deaths",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Death Count"))
  })
  
  output$us_daily_testing_graph <- renderPlotly({
    filtered_df() %>%
      group_by(date) %>%
      summarize(dailyTests = sum(totalTestResultsIncrease, na.rm=T)) %>%
      dplyr::filter(!is.na(dailyTests)) %>%
    plot_ly(x = ~date, y = ~dailyTests, type = "scatter", mode = "lines") %>%
      layout(title = "Daily Testing",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Test Count"))
  })
  
  output$us_daily_testing_neg_graph <- renderPlotly({
    filtered_df() %>%
      group_by(date) %>%
      summarize(dailyTests = sum(negativeIncrease, na.rm=T)) %>%
      dplyr::filter(!is.na(dailyTests)) %>%
    plot_ly(x = ~date, y = ~dailyTests, type = "scatter", mode = "lines") %>%
      layout(title = "Daily Testing Negative",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Negative Result Test Count"))
  })
  
  output$us_daily_testing_pos_graph <- renderPlotly({
    filtered_df() %>%
      group_by(date) %>%
      summarize(dailyTests = sum(positiveIncrease, na.rm=T)) %>%
      dplyr::filter(!is.na(dailyTests)) %>%
    plot_ly(x = ~date, y = ~dailyTests, type = "scatter", mode = "lines") %>%
      layout(title = "Daily Testing Positive",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Positive Result Test Count"))
  })
  
  output$us_daily_positive_percentage_testing_graph <- renderPlotly({
    filtered_df() %>%
      group_by(date) %>%
      summarize(sumNegativeIncrease = sum(negativeIncrease, na.rm=T), sumPositiveIncrease = sum(positiveIncrease, na.rm=T)) %>%
      mutate(positivePercentage = (sumPositiveIncrease / (sumPositiveIncrease + sumNegativeIncrease))*100) %>%
      dplyr::filter(!is.na(positivePercentage), positivePercentage < 75) %>% # filter out extremely high percentages at start of pandemic
    plot_ly(x = ~date, y = ~positivePercentage, type = "scatter", mode = "lines") %>%
      layout(title = "Daily Positive Percentage",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Positive Percentage"))
  })
  
  output$us_total_immunity_graph <- renderPlotly({
    filtered_df() %>%
      group_by(date) %>%
      summarize(sumPopulation = sum(population, na.rm=T), sumCases = sum(positive, na.rm=T) + sum(death, na.rm=T)) %>%
      mutate(immunityPercentage = (sumCases / sumPopulation)*100) %>%
    plot_ly(x = ~date, y = ~immunityPercentage, type = "scatter", mode = "lines") %>%
      layout(title = "Immunity Percentage",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Immunity Percentage"))
  })
  
  output$worst_states_table <- DT::renderDT({
    df <- filtered_df() %>%
      filter(date == max(filtered_df()$date)) %>%
      group_by(state) %>%
      summarize(deathsPerCapita = sum(death, na.rm=T) / sum(population, na.rm=T), population=population, death=death, positive=positive, negative=negative) %>%
      arrange(desc(deathsPerCapita)) %>%
      data.frame()
    DT::datatable(df)
  })
  
  output$worst_states_testing_table <- DT::renderDT({
    df <- filtered_df() %>%
      filter(date == max(filtered_df()$date)) %>%
      group_by(state) %>%
      summarize(testsPerCapita = (sum(positive, na.rm=T)+sum(negative, na.rm=T)) / sum(population, na.rm=T), population=population, positive=positive, negative=negative) %>%
      arrange(testsPerCapita) %>%
      data.frame()
    DT::datatable(df)
  })
  
  output$missing_data <- renderPlot({
    vis_miss(filtered_df(), cluster=TRUE)
  })
  
  output$us_data_quality <- renderPlotly({
    filtered_df() %>%
      filter(date == max(filtered_df()$date)) %>%
      group_by(dataQualityGrade) %>%
      summarize(n = n()) %>%
    plot_ly(x = ~dataQualityGrade, y = ~n, type = "bar") %>%
      layout(title = "Data Quality Grade",
             xaxis = list(title = "Data Quality Grade"),
             yaxis = list(title = "Count of States"))
  })
  
})
