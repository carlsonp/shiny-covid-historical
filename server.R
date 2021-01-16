library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(DT)
library(visdat)
library(zoo)
# sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
# sudo apt-get update
# sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev 
library(sf)
library(leaflet)
library(shinydashboard)

source(file="dataprep.R")

cdcvaccinationshist = read_csv2("cdc_vaccinations_hist.csv")

cdccovidvaccinations = cdccovidvaccinations %>%
  dplyr::mutate(ratio_admin_dist = doses_administered / doses_distributed)

shinyServer(function(input, output, session) {
  
  filtered_df <- reactive({
    return(
      covid %>%
        dplyr::filter(state %in% input$stateFilter)
    )
  })
  
  filtered_nytimes <- reactive({
    df <- nytimescovidcounties %>%
      dplyr::filter(state %in% input$mapStateFilter) %>%
      dplyr::filter(date == max(date))
    
    return(df)
  })
  
  filtered_vaccinations <- reactive({
    return(
      cdccovidvaccinations %>%
        dplyr::filter(state %in% input$vaccinationStateFilter)
    )
  })
  
  filtered_vaccinations_hist <- reactive({
    return(
      cdcvaccinationshist %>%
        dplyr::filter(state %in% input$vaccinationStateFilter)
    )
  })
  
  infectscale <- reactive({
    return((1-(input$infectionscaught/100)) / (input$infectionscaught/100))
  })
  deathscale <- reactive({
    return((1-(input$deathscaught/100)) / (input$deathscaught/100))
  })

  state_shape <- reactive({
    # extremely high resolution, large shape files, over 100 MB
    # https://www.census.gov/cgi-bin/geo/shapefiles/index.php
    
    # a more simplified version, better for Github
    # https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
    usgeo <- sf::st_read("./shapefiles/cb_2019_us_county_5m.shp") %>%
      mutate(fips = as.character(paste0(STATEFP, COUNTYFP)))
    
    df <- filtered_nytimes() %>%
      dplyr::group_by(fips) %>%
      summarise(percent_positive_cases = round((sum(cases) / unique(POPESTIMATE2019)) * 100, digits=2),
                positive_cases = sum(cases),
                population = unique(POPESTIMATE2019),
                percent_deaths = round((sum(deaths) / unique(POPESTIMATE2019)) * 100, digits=2),
                deaths = sum(deaths))
    
    # https://stackoverflow.com/questions/58152812/how-do-i-map-county-level-data-as-a-heatmap-using-fips-codes-interactively-in
    # join data with shapefiles
    df <- sf::st_as_sf(df %>% left_join(usgeo, by="fips"))
    # change projection otherwise we get a warning
    df <- sf::st_transform(df, "+proj=longlat +datum=WGS84")
    
    return(df)
  })
  
  vaccinations_state_shape <- reactive({
    # extremely high resolution, large shape files, over 100 MB
    # https://www.census.gov/cgi-bin/geo/shapefiles/index.php
    
    # a more simplified version, better for Github
    # https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
    usgeo <- sf::st_read("./shapefiles/cb_2019_us_state_20m.shp")
    
    df <- filtered_vaccinations()
    
    # https://stackoverflow.com/questions/58152812/how-do-i-map-county-level-data-as-a-heatmap-using-fips-codes-interactively-in
    # join data with shapefiles
    df <- sf::st_as_sf(df %>% left_join(usgeo, by=c("state"="NAME")))
    # change projection otherwise we get a warning
    df <- sf::st_transform(df, "+proj=longlat +datum=WGS84")
    
    return(df)
  })
  
  output$map_state_filter <- renderUI({
    pickerInput("mapStateFilter",
                label = "US State:",
                choices = sort(unique(nytimescovidcounties$state)),
                selected = "New Mexico",
                options = list(
                  `actions-box` = TRUE, 
                  size = 10
                ), 
                multiple = FALSE
    )
  })
  
  output$vaccination_state_filter <- renderUI({
    pickerInput("vaccinationStateFilter",
                label = "US State(s):",
                choices = sort(unique(cdccovidvaccinations$state)),
                selected = sort(unique(cdccovidvaccinations$state)),
                options = list(
                  `actions-box` = TRUE, 
                  size = 10
                ), 
                multiple = TRUE
    )
  })
  
  output$state_filter <- renderUI({
    pickerInput("stateFilter",
      label = "US State(s):",
      choices = sort(unique(covid$state)),
      selected = sort(unique(covid$state)),
      options = list(
        `actions-box` = TRUE, 
        size = 10
      ), 
      multiple = TRUE
    )
  })
  
  output$immunity_calc <- renderUI({

    df <- filtered_df() %>%
      group_by(date) %>%
      summarize(sumPopulation = sum(population, na.rm=T), sumPositiveCases = sum(positive, na.rm=T), sumDeaths=sum(death, na.rm=T)) %>%
      dplyr::filter(date == max(date))

    estimated_imm = round(((df$sumPositiveCases+(df$sumPositiveCases*infectscale())+df$sumDeaths+(df$sumDeaths*deathscale())) / df$sumPopulation)*100, 2)

    tags$div(
      tags$p("((tested_positives + estimated_additional_positive + known_deaths + estimated_additional_deaths) / population)*100"),
      tags$p(paste0("((", df$sumPositiveCases, " + ", round(df$sumPositiveCases*infectscale()), " + ", df$sumDeaths, " + ", round(df$sumDeaths*deathscale()), ") / ", df$sumPopulation, ")*100 = ", estimated_imm, "%"))
    )
  })

  output$data_as_of <- renderUI({
    shiny::validate(
      need(!is.na(filtered_df()$date), 'Loading...')
    )
    
    tags$p(paste("Data as of:", max(filtered_df()$date), "(updated daily)"))
  })
  
  output$nytimes_data_as_of <- renderUI({
    shiny::validate(
      need(!is.na(nytimescovidcounties$date), 'Loading...')
    )
    
    tags$p(paste("Data as of:", max(nytimescovidcounties$date), "(updated daily)"))
  })
  
  output$vaccination_data_as_of <- renderUI({
    shiny::validate(
      need(!is.na(cdccovidvaccinations$date), 'Loading...')
    )
    
    tags$p(paste("Data as of:", max(cdccovidvaccinations$date), "(updated daily)"))
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
      need(!is.na(filtered_df()$date) && !is.na(infectscale()) && !is.na(deathscale()), 'Loading...')
    )
    
    filtered_df() %>%
      group_by(date) %>%
      summarize(sumPopulation = sum(population, na.rm=T), sumCases = (sum(positive, na.rm=T) + sum(positive, na.rm=T)*infectscale() + sum(death, na.rm=T) + sum(death, na.rm=T)*deathscale())) %>%
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
  
  output$county_cases_leaflet <- leaflet::renderLeaflet({
    shiny::validate(
      need(!is.na(state_shape()$NAME), 'Loading...')
    )
    
    # create popups
    percentcasespopup <- paste0("County: ", state_shape()$NAME, "<br>Percentage of cumulative positive cases relative to population: ", state_shape()$percent_positive_cases, "%")
    numbercasespopup <- paste0("County: ", state_shape()$NAME, "<br>Number of cumulative positive cases: ", state_shape()$positive_cases)
    percentdeathspopup <- paste0("County: ", state_shape()$NAME, "<br>Percentage of cumulative deaths relative to population: ", state_shape()$percent_deaths, "%")
    numberdeathspopup <- paste0("County: ", state_shape()$NAME, "<br>Number of cumulative deaths: ", state_shape()$deaths)
    populationpopup <- paste0("County: ", state_shape()$NAME, "<br>Population: ", state_shape()$population)
    
    # create color palettes
    percentCasesPalette <- colorNumeric(palette = "Reds", domain = state_shape()$percent_positive_cases)
    numberCasesPalette <- colorNumeric(palette = "Reds", domain = state_shape()$positive_cases)
    percentDeathsPalette <- colorNumeric(palette = "Reds", domain = state_shape()$percent_deaths)
    numberDeathsPalette <- colorNumeric(palette = "Reds", domain = state_shape()$deaths)
    populationPalette <- colorNumeric(palette = "Reds", domain = state_shape()$population)
    
    # create map
    leaflet(state_shape(), options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = percentcasespopup,
                  color = ~percentCasesPalette(state_shape()$percent_positive_cases),
                  group = "Percentage of cumulative positive cases relative to population"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = numbercasespopup,
                  color = ~numberCasesPalette(state_shape()$positive_cases),
                  group = "Number of cumulative positive cases"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = percentdeathspopup,
                  color = ~percentDeathsPalette(state_shape()$percent_deaths),
                  group = "Percentage of cumulative deaths relative to population"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = numberdeathspopup,
                  color = ~numberDeathsPalette(state_shape()$deaths),
                  group = "Number of cumulative deaths"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = populationpopup,
                  color = ~populationPalette(state_shape()$population),
                  group = "Population"
      ) %>%
      addLayersControl(
        baseGroups=c("Percentage of cumulative positive cases relative to population",
                     "Number of cumulative positive cases",
                     "Percentage of cumulative deaths relative to population",
                     "Number of cumulative deaths",
                     "Population"),
        position = "topright",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$worst_hit_counties_table <- DT::renderDT({
    shiny::validate(
      need(!is.na(filtered_nytimes()$date), 'Loading...')
    )
    
    df <- filtered_nytimes() %>%
      dplyr::group_by(state, county) %>%
      summarise(percent_positive_cases_relative_pop = round((sum(cases) / unique(POPESTIMATE2019)) * 100, digits=2)) %>%
      arrange(desc(percent_positive_cases_relative_pop)) %>%
      mutate(percent_positive_cases_relative_pop = paste0(as.character(percent_positive_cases_relative_pop), "%")) %>%
      data.frame()
    
    DT::datatable(df)
  })
  
  output$vaccinations_table <- DT::renderDT({
    shiny::validate(
      need(!is.na(filtered_vaccinations()$date), 'Loading...')
    )
    
    df <- filtered_vaccinations() %>%
      select(state, population, doses_distributed, doses_administered, dist_per_100k, admin_per_100k, ratio_admin_dist) %>%
      arrange(desc(admin_per_100k)) %>%
      data.frame()
    
    DT::datatable(df)
  })
  
  output$totalVaccinationCounts <- renderUI({
    shiny::validate(
      need(!is.na(filtered_vaccinations()$date), 'Loading...')
    )
    
    tags$div(
      tags$p(paste0("Total Vaccine Doses Distributed: ", prettyNum(sum(filtered_vaccinations()$doses_distributed), big.mark=",",scientific=FALSE))),
      tags$p(paste0("Total Vaccine Doses Administered: ", prettyNum(sum(filtered_vaccinations()$doses_administered), big.mark=",",scientific=FALSE))),
      tags$p(paste0("Average Vaccine Doses Distributed per 100k Population: ", round(mean(filtered_vaccinations()$dist_per_100k), 2))),
      tags$p(paste0("Average Vaccine Doses Administered per 100k Population: ", round(mean(filtered_vaccinations()$admin_per_100k), 2))),
      tags$p(paste0("Average Ratio of Vaccinations Administered Over Distributed: ", round(mean(filtered_vaccinations()$ratio_admin_dist), 2))),
      tags$p(paste0("Total Population: ", prettyNum(sum(filtered_vaccinations()$population), big.mark=",",scientific=FALSE))),
      tags$br()
    )
  })
  
  output$vaccinations_map <- leaflet::renderLeaflet({
    shiny::validate(
      need(!is.na(vaccinations_state_shape()$state), 'Loading...')
    )
    
    # create popups
    dosesdistributedpopup <- paste0("State: ", vaccinations_state_shape()$STUSPS, "<br>Vaccine doses distributed: ", vaccinations_state_shape()$doses_distributed)
    dosesadministeredpopup <- paste0("State: ", vaccinations_state_shape()$STUSPS, "<br>Vaccine doses administered: ", vaccinations_state_shape()$doses_administered)
    distper100kpopup <- paste0("State: ", vaccinations_state_shape()$STUSPS, "<br>Vaccine doses distributed per 100k population: ", vaccinations_state_shape()$dist_per_100k)
    adminper100kpopup <- paste0("State: ", vaccinations_state_shape()$STUSPS, "<br>Vaccine doses administered per 100k population: ", vaccinations_state_shape()$admin_per_100k)
    ratioadminperdistpopup <- paste0("State: ", vaccinations_state_shape()$STUSPS, "<br>Ratio of vaccinations administered over distributed: ", vaccinations_state_shape()$ratio_admin_dist)
    populationpopup <- paste0("State: ", vaccinations_state_shape()$STUSPS, "<br>Population: ", vaccinations_state_shape()$population)
    
    # create color palettes
    dosesDistributedPalette <- colorNumeric(palette = "Greens", domain = vaccinations_state_shape()$doses_distributed)
    dosesAdministeredPalette <- colorNumeric(palette = "Greens", domain = vaccinations_state_shape()$doses_administered)
    distributedPer100kPalette <- colorNumeric(palette = "Greens", domain = vaccinations_state_shape()$dist_per_100k)
    administeredPer100kPalette <- colorNumeric(palette = "Greens", domain = vaccinations_state_shape()$admin_per_100k)
    ratioAdministeredDistributedPalette <- colorNumeric(palette = "Greens", domain = vaccinations_state_shape()$ratio_admin_dist)
    populationPalette <- colorNumeric(palette = "Greens", domain = vaccinations_state_shape()$population)
    
    # create map
    leaflet(vaccinations_state_shape(), options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = dosesdistributedpopup,
                  color = ~dosesDistributedPalette(vaccinations_state_shape()$doses_distributed),
                  group = "Vaccine doses distributed"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = dosesadministeredpopup,
                  color = ~dosesAdministeredPalette(vaccinations_state_shape()$doses_administered),
                  group = "Vaccine doses administered"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = distper100kpopup,
                  color = ~distributedPer100kPalette(vaccinations_state_shape()$dist_per_100k),
                  group = "Vaccine doses distributed per 100k population"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = adminper100kpopup,
                  color = ~administeredPer100kPalette(vaccinations_state_shape()$admin_per_100k),
                  group = "Vaccine doses administered per 100k population"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = ratioadminperdistpopup,
                  color = ~ratioAdministeredDistributedPalette(vaccinations_state_shape()$ratio_admin_dist),
                  group = "Ratio of vaccinations administered over distributed"
      ) %>%
      addPolygons(stroke=FALSE,
                  smoothFactor = 0.2,
                  fillOpacity = 0.8,
                  popup = populationpopup,
                  color = ~populationPalette(vaccinations_state_shape()$population),
                  group = "Population"
      ) %>%
      addLayersControl(
        baseGroups=c("Vaccine doses distributed",
                     "Vaccine doses administered",
                     "Vaccine doses distributed per 100k population",
                     "Vaccine doses administered per 100k population",
                     "Ratio of vaccinations administered over distributed",
                     "Population"),
        position = "topright",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$vaccination_history <- renderPlotly({
    shiny::validate(
      need(!is.na(filtered_vaccinations_hist()$date), 'Loading...')
    )
    
    filtered_vaccinations_hist() %>%
      # group on day to find the latest run for each day
      dplyr::group_by(date) %>%
      dplyr::filter(runid == max(runid)) %>%
      data.frame() %>%
      dplyr::mutate(pop_percent_admin = (doses_administered/population)*100) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(avg_pop_percent_admin = round(mean(pop_percent_admin, na.rm=TRUE), 2)) %>%
      plot_ly(x = ~date, y = ~avg_pop_percent_admin, mode = "lines") %>%
      layout(title = "Average Population Percent Administered Vaccination",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Average Population Percent Administered Vaccination"))
  })
  
})
