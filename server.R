library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(DT)
library(visdat)
library(zoo)

source(file="dataprep.R")

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
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    tags$p(paste("Data as of:", max(filtered_df()$date), "(updated daily)"))
  })
   
  output$us_daily_deaths_graph <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      group_by(date) %>%
      summarize(dailyDeaths = sum(deathIncrease, na.rm=T)) %>%
      mutate(roll_mean = rollmeanr(dailyDeaths, k=7, fill=NA)) %>%
      dplyr::filter(!is.na(dailyDeaths))
    plot_ly(df, x = ~date, y = ~dailyDeaths, type = "scatter", mode = "lines", name="Deaths") %>%
      add_trace(df, x=~date, y=~roll_mean, name="7 day moving average") %>%
      layout(title = paste0("Daily Deaths (total: ", sum(df$dailyDeaths, na.rm=T), ")"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Death Count"))
  })
  
  output$us_daily_testing_graph <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      group_by(date) %>%
      summarize(dailyTests = sum(totalTestResultsIncrease, na.rm=T)) %>%
      mutate(roll_mean = rollmeanr(dailyTests, k=7, fill=NA)) %>%
      dplyr::filter(!is.na(dailyTests))
    plot_ly(df, x = ~date, y = ~dailyTests, type = "scatter", mode = "lines", name="Tests") %>%
      add_trace(df, x=~date, y=~roll_mean, name="7 day moving average") %>%
      layout(title = paste0("Daily Testing (total: ", sum(df$dailyTests, na.rm=T), ")"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Test Count"))
  })
  
  output$us_daily_testing_neg_graph <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      group_by(date) %>%
      summarize(dailyTests = sum(negativeIncrease, na.rm=T)) %>%
      mutate(roll_mean = rollmeanr(dailyTests, k=7, fill=NA)) %>%
      dplyr::filter(!is.na(dailyTests))
    plot_ly(df, x = ~date, y = ~dailyTests, type = "scatter", mode = "lines", name="Negative Tests") %>%
      add_trace(df, x=~date, y=~roll_mean, name="7 day moving average") %>%
      layout(title = paste0("Daily Testing Negative (total: ", sum(df$dailyTests, na.rm=T), ")"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Negative Result Test Count"))
  })
  
  output$us_daily_testing_pos_graph <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      group_by(date) %>%
      summarize(dailyTests = sum(positiveIncrease, na.rm=T)) %>%
      mutate(roll_mean = rollmeanr(dailyTests, k=7, fill=NA)) %>%
      dplyr::filter(!is.na(dailyTests))
    plot_ly(df, x = ~date, y = ~dailyTests, type = "scatter", mode = "lines", name="Positive Tests") %>%
      add_trace(df, x=~date, y=~roll_mean, name="7 day moving average") %>%
      layout(title = paste0("Daily Testing Positive (total: ", sum(df$dailyTests, na.rm=T), ")"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Positive Result Test Count"))
  })
  
  output$us_daily_positive_percentage_testing_graph <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    filtered_df() %>%
      group_by(date) %>%
      summarize(sumNegativeIncrease = sum(negativeIncrease, na.rm=T), sumPositiveIncrease = sum(positiveIncrease, na.rm=T)) %>%
      mutate(positivePercentage = (sumPositiveIncrease / (sumPositiveIncrease + sumNegativeIncrease))*100) %>%
      mutate(roll_mean = rollmeanr(positivePercentage, k=7, fill=NA)) %>%
      dplyr::filter(!is.na(positivePercentage), positivePercentage < 75) %>% # filter out extremely high percentages at start of pandemic
    plot_ly(x = ~date, y = ~positivePercentage, type = "scatter", mode = "lines", name="Positive Percentage") %>%
      add_trace(df, x=~date, y=~roll_mean, name="7 day moving average") %>%
      layout(title = "Daily Positive Percentage",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Positive Percentage"))
  })
  
  output$us_daily_hospitalized_graph <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      group_by(date) %>%
      summarize(dailyHospitalized = sum(hospitalizedIncrease, na.rm=T)) %>%
      mutate(roll_mean = rollmeanr(dailyHospitalized, k=7, fill=NA)) %>%
      dplyr::filter(!is.na(dailyHospitalized))
    plot_ly(df, x = ~date, y = ~dailyHospitalized, type = "scatter", mode = "lines", name="Hospitalized") %>%
      add_trace(df, x=~date, y=~roll_mean, name="7 day moving average") %>%
      layout(title = paste0("Daily Hospitalized (total: ", sum(df$dailyHospitalized, na.rm=T), ")"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Hospitalized Count"))
  })
  
  output$us_total_immunity_graph <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
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
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      filter(date == max(filtered_df()$date)) %>%
      group_by(state) %>%
      summarize(deathsPerCapita = sum(death, na.rm=T) / sum(population, na.rm=T), population=population, death=death, positive=positive, negative=negative) %>%
      arrange(desc(deathsPerCapita)) %>%
      data.frame()
    DT::datatable(df)
  })
  
  output$worst_states_testing_table <- DT::renderDT({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      filter(date == max(filtered_df()$date)) %>%
      group_by(state) %>%
      summarize(testsPerCapita = (sum(positive, na.rm=T)+sum(negative, na.rm=T)) / sum(population, na.rm=T), population=population, positive=positive, negative=negative) %>%
      arrange(testsPerCapita) %>%
      data.frame()
    DT::datatable(df)
  })
  
  output$missing_data <- renderPlot({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    vis_miss(filtered_df(), cluster=TRUE)
  })
  
  output$us_data_quality <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    filtered_df() %>%
      filter(date == max(filtered_df()$date)) %>%
      group_by(dataQualityGrade) %>%
      summarize(n = n()) %>%
    plot_ly(x = ~dataQualityGrade, y = ~n, type = "bar") %>%
      layout(title = "Data Quality Grade",
             xaxis = list(title = "Data Quality Grade"),
             yaxis = list(title = "Count of States"))
  })
  
  output$data_quality_table <- DT::renderDT({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    df <- filtered_df() %>%
      filter(date == max(filtered_df()$date)) %>%
      select(state, dataQualityGrade) %>%
      arrange(state) %>%
      data.frame()
    DT::datatable(df)
  })
  
})
