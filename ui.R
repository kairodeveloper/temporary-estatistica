library(shiny)
library(readr)
library(dplyr)
library(plotly)
library(ggplot2)

shinyUI(
  fluidPage(
  pageWithSidebar(
    headerPanel("Crime in Vancouver - Kairo e Fabio"),
    sidebarPanel(
      h2("Numero de crimes gerais"),
      selectInput("anoSelect", 
                  "Todos os anos?",
                  choices = c("SIM", "NAO")),
      conditionalPanel(
        condition = "input.anoSelect == 'NAO'",
        sliderInput("yearSelect", "Selecione o ano", min = 2003, max = 2017, value = 2010, step = 1)
      ),
      selectInput("tiposDeCrimes", 
                  "Tipo de crime",
                  choices = c("Todos os crimes", 
                    "Theft from Vehicle", 
                    "Mischief",
                    "Break and Enter Residential/Other",
                    "Offence Against a Person",
                    "Other Thef",
                    "Theft of Vehicle",
                    "Break and Enter Commercial",
                    "Theft of Bicycle",
                    "Vehicle Collision or Pedestrian Struck (with Injury)",
                    "Vehicle Collision or Pedestrian Struck (with Fatality)",
                    "Homicide"
                    ))
    ),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel(
                    "Medidas de tendencia central", 
                    tableOutput("mtc"),
                    column(12,
                      tableOutput('table')
                    )
                   ),
                  tabPanel(
                    "Medidas de dispersao", 
                    tableOutput("mdd"),
                    column(12,
                           tableOutput('table1')
                    )
                  ),
                  tabPanel(
                    "Assimetria e Curtose", 
                    tableOutput("aec"),
                    column(12,
                           tableOutput('table2')
                    ),
                    column(12, plotlyOutput("plot2"))
                    
                  ),
                  tabPanel("Medidas de Correlacao", tableOutput("mdc")),
                  tabPanel(
                    "Graficos", 
                    plotlyOutput("plot1")
                  )
      
    )
  ) 
)
)
)