
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source("setup.R")

shinyUI(fluidPage(
  fluidRow(
          column(4, 
           wellPanel(selectInput("county",
                       "Select a county:",
                       choices = unique(county_choices$county),
                       selected = 'Colorado') 
                     ))
           ),
  fluidRow(
           
           column(6, 
                  plotlyOutput("netMigAge"),
                  downloadButton('netMigAgeData', 'Download Data (CSV)')
           ),
           column(6, 
                  plotlyOutput("estimates")
           )
        ),
  fluidRow(
           
           column(6, 
                  plotlyOutput("projections")
           ),
           column(6, 
                  plotlyOutput("components")
           )
  )
  ))
