
library(plotly)

source("setup.R")

function(input, output, session) {

county=reactive({filter(read.csv("county_names.csv"), county==input$county)%>%
                  select(countyfips)%>%
    as.numeric()})

#### Population Estimates ####
  output$estimates=renderPlotly({estimates_p(county())})
  
  est_data=reactive({estimates_d(county(), input$county)})
  
  output$estimatesData=downloadHandler(
    filename= function(){
      paste(unique(est_data()$County), paste0("Population Estimates 1985 to ", as.character(max(est_data()$year)), ".csv"))
    },
    content= function(file){
      write.csv(est_data(), file, row.names=FALSE)
    }
    
  )
  
### Net Migration by Age ####
  
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

  #### Population Projections ####
  output$projections=renderPlotly({projections_p(county(), 2010)})
  
  proj_data=reactive({projections_d(county(), input$county, 2010)})
  
  output$projectionsData=downloadHandler(
    filename= function(){
      paste(unique(proj_data()$County), paste0("Population Projections 2010 to 2050.csv"))
    },
    content= function(file){
      write.csv(proj_data(), file, row.names=FALSE)
    }
    
  )  
  
#### Components of Change ####
  output$components=renderPlotly({components_p(county())})
  
  comp_data=reactive({components_d(county(), input$county)})
  
  output$componentsData=downloadHandler(
    filename= function(){
      paste(unique(comp_data()$County), paste0("Births, Deaths, and Net Migration 1985 to ", max(comp_data()$year), ".csv"))
    },
    content= function(file){
      write.csv(comp_data(), file, row.names=FALSE)
    }
    
  )
  
##### Percent Change By Age Group #####
  output$projections_age=renderPlotly({projections_ageGroup(county())})
}
