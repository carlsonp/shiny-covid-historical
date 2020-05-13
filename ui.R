library(shiny)
library(plotly)
library(shinyWidgets)
library(DT)

shinyUI(fluidPage(
  
  headerPanel('US Covid-19'),
  sidebarPanel(
    uiOutput("state_filter"),
    tags$div(
      tags$a(href="https://covidtracking.com/", "Covid Tracking Data")
    ),
    uiOutput("data_as_of")
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
      tabPanel("Immunity Percentage", plotlyOutput("us_total_immunity_graph")),
      tabPanel("Worst Hit US States", DT::DTOutput("worst_states_table")),
      tabPanel("Worst States Testing", DT::DTOutput("worst_states_testing_table")),
      tabPanel("Missing Data", plotOutput("missing_data")),
      tabPanel("Data Quality Grade", plotlyOutput("us_data_quality"))
    )
  )

))