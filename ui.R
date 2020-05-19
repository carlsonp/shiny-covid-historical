library(shiny)
library(plotly)
library(shinyWidgets)
library(DT)

shinyUI(fluidPage(
  
  headerPanel('US Covid-19'),
  sidebarPanel(
    uiOutput("state_filter"),
    tags$div(
      tags$a(href="https://covidtracking.com/", "Covid Tracking Data", target="_blank")
    ),
    uiOutput("data_as_of"),
    tags$a(href="https://github.com/carlsonp/shiny-covid-historical", "Source Code", target="_blank")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Daily Deaths", plotlyOutput("us_daily_deaths_graph")),
      tabPanel("Daily Testing", plotlyOutput("us_daily_testing_graph")),
      tabPanel("Daily Testing Negative", plotlyOutput("us_daily_testing_neg_graph")),
      tabPanel("Daily Testing Positive", plotlyOutput("us_daily_testing_pos_graph")),
      tabPanel("Daily Positive Percentage", 
               tags$p("Ideally this should be below 5%"),
               plotlyOutput("us_daily_positive_percentage_testing_graph")
      ),
      tabPanel("Daily Hospitalized",
               tags$p("Not all states are reporting hospitalizations."),
               plotlyOutput("us_daily_hospitalized_graph")
      ),
      tabPanel("Immunity Percentage", 
               tags$p("This is highly suspect since many people have had COVID without being tested.  In addition, immunity may not occur or only last a short time."),
               tags$p("((positives + deaths) / population)*100"),
               plotlyOutput("us_total_immunity_graph")
      ),
      tabPanel("Worst Hit US States", 
               tags$p("State population data from Wikipedia."),
               DT::DTOutput("worst_states_table")
      ),
      tabPanel("Worst States Testing", 
               tags$p("State population data from Wikipedia."),
               DT::DTOutput("worst_states_testing_table")
      ),
      tabPanel("Missing Data", plotOutput("missing_data")),
      tabPanel("Data Quality Grade", 
               tags$p("The most recent scores/grades from the Covid Tracking Project."),
               tags$a(href="https://covidtracking.com/about-data#state-data-quality-grades", "https://covidtracking.com/about-data#state-data-quality-grades", target="_blank"),
               plotlyOutput("us_data_quality"),
               DT::DTOutput("data_quality_table")
      )
    )
  )

))