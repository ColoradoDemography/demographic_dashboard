
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
source("setup.R")

shinyServer(function(input, output) {

county=reactive({filter(read.csv("county_names.csv"), county==input$county)%>%
                  select(countyfips)%>%
    as.numeric()})


  
  output$netMigAge=renderPlotly({mig_age_p(county())})
  
  nm_data=reactive({mig_age_d(county(), input$county)})
  
  output$netMigAgeData=downloadHandler(
    filename= function(){
      paste(unique(nm_data()$County), "Net Migration by Age.csv", sep="_")
    },
    content= function(file){
      write.csv(nm_data(), file, row.names=FALSE)
    }
    
  )
  

})
