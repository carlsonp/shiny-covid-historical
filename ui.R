library(shiny)
library(plotly)
library(shinyWidgets)
library(DT)
library(leaflet)
library(shinydashboard)

dashboardPage(
  
  dashboardHeader(title = "US Covid-19"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Historical", tabName = "historical", icon = icon("chart-bar")),
      menuItem("Map", tabName = "map", icon = icon("map-marker")),
      menuItem("Vaccinations", tabName = "vaccinations", icon = icon("syringe")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "historical",
        fixedRow(
          column(4,
            uiOutput("state_filter")
          ),
          column(4,
             tags$div(
               tags$a(href="https://covidtracking.com/", "Covid Tracking Data", target="_blank"),
               uiOutput("data_as_of")
             )
          )
        ),
        
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
                   # https://dreamrs.github.io/shinyWidgets/reference/numericInputIcon.html
                   numericInputIcon("infectionscaught", "Percentage Infections Caught:", 10, min = 0.5, max = 100, step=0.5, icon = list(NULL, icon("percent"))),
                   numericInputIcon("deathscaught", "Percentage Deaths Caught:", 80, min = 0.5, max = 100, step=0.5, icon = list(NULL, icon("percent"))),
                   uiOutput("immunity_calc"),
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
      ),
      tabItem(tabName = "map",
        fixedRow(
          column(3,
                 uiOutput("map_state_filter")
          ),
          column(3,
                 tags$div(
                   tags$a(href="https://github.com/nytimes/covid-19-data", "NYTimes COVID Data by County", target="_blank"),
                   uiOutput("nytimes_data_as_of")
                 )
          ),
          column(3,
                 tags$div(
                   tags$a(href="https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html", "US Census County Population Estimates", target="_blank"),
                 )
          ),
          column(3,
                 tags$div(
                   tags$a(href="https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html", "US Census County Maps", target="_blank"),
                 )
          )
        ),
        tabsetPanel(
          tabPanel("Cases by State County",
            leafletOutput("county_cases_leaflet")
          ),
          tabPanel("Worst Hit State Counties",
            DT::DTOutput("worst_hit_counties_table")
          )
        )
      ),
      tabItem(tabName = "vaccinations",
          fixedRow(
            column(4,
               uiOutput("vaccination_state_filter")
            ),
            column(4,
               tags$div(
                 tags$a(href="https://covid.cdc.gov/covid-data-tracker/#vaccinations", "CDC Covid Vaccination Data", target="_blank"),
                 uiOutput("vaccination_data_as_of")
               )
            )
          ),
          tabsetPanel(
            tabPanel("Vaccinations by State",
              DT::DTOutput("vaccinations_table")
            ),
            tabPanel("Vaccinations by State Map",
              leafletOutput("vaccinations_map")
            )
          )
      ),
      tabItem(tabName = "about",
        tags$a(href="https://github.com/carlsonp/shiny-covid-historical", "Source Code", target="_blank"),
        tags$p("Please submit bugs or feature requests via Github issues from the link above.")
      )
    )
  )
)