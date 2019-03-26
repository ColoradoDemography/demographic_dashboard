library(dplyr)
library(tidyr)
library(plotly)




# load("county_forecast.rdata")
# load("county_profile.rdata")
load("county_migbyage.rdata")
# county_profile=read.csv("county_profile.csv")

county_choices=read.csv("county_names.csv", stringsAsFactors = FALSE)%>%
  select(county)

#### Net Migration by Age Graph and Data #####

## Generates a Plotly Chart
mig_age_p=function(fips){
  
  data=county_migbyage%>%    
    mutate(countyfips=as.numeric(countyfips))%>%
    filter(countyfips==fips, age<90)
  
  plot_ly(data, x=~age, y=~netMigration, type="bar", line=list(color = "rgb(31,74,126)"), marker=list(color="rgb(31,74,126)"))%>%
    layout(
      title="Net Migration by Age, 2000 to 2010",
      xaxis=list(
        title=" Age"),
      yaxis=list(
        title=" Net Migration"),
      margin=list(t=60)
    )

}

## Generates the data download
mig_age_d=function(fips, name){

  x=county_migbyage%>%
    mutate(countyfips=as.numeric(countyfips))%>%
    filter(countyfips==fips, age<90)%>%
    bind_cols(data.frame(County=rep(name, 90)))%>%
    select(County, Age=age, NetMigration=netMigration)
    
  
  return(x)
}


#### Population Estimates Graph and Data ####


## Generates a Plotly Chart
estimates_p=function(fips){
  
  data=codemogAPI::county_profile(fips, 1985:2017, vars="totalpopulation")
  
  plot_ly(data, x= ~year, y= ~as.numeric(totalpopulation), type= "bar", marker=list(color = "rgb(31,74,126)"))%>%
    layout(
      title=paste("Population Estimates 1985 to", as.character(max(data$year))),
      xaxis=list(
        title="Year"),
      yaxis=list(
        title="Total Population"),
      margin=list(t=60)
    )
  
}

## Generates the data download
estimates_d=function(fips, name){
  
  x=codemogAPI::county_profile(fips, 1985:2017, vars="totalpopulation")
    
  
  
  return(x)
}


#### Population Projections Graph and Data ####


## Generates a Plotly Chart
projections_p=function(fips, est_year){
  
  
  # CO=codemogAPI::county_sya(0, 3000)%>% # Creates data for the state as a whole since that isn't in the data frame.   
  #   filter(year>=est_year)%>%
  #   mutate(totalpopulation=as.numeric(totalpopulation))%>%
  #   group_by(year)%>%
  #   summarize(totalpopulation=sum(totalpopulation))%>%
  #   mutate(countyfips=0)%>%
  #   select(countyfips, year, totalpopulation)
  
  data=codemogAPI::county_sya(fips, 3000)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    # bind_rows(CO)%>%
    filter(countyfips==fips, year>=est_year)%>%
    group_by(countyfips, year)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(countyfips, year, totalpopulation)
    
  
  plot_ly(data, x=~year, y=~totalpopulation, type= "bar", marker=list(color = "rgb(31,74,126)"))%>%
    layout(
      title=paste("Population Projections", as.character(est_year), "to 2050"),
      xaxis=list(
        title="Year"),
      yaxis=list(
        title="Total Population"),
      margin=list(t=60)
    )
  
}

## Generates the data download
projections_d=function(fips, name, est_year){
  
  CO=codemogAPI::county_sya(0,3000)%>% # Creates data for the state as a whole since that isn't in the data frame.   
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(year>=est_year)%>%
    group_by(year)%>%
    summarize(totalpopulation=sum(totalpopulation))%>%
    mutate(countyfips=0)%>%
    select(countyfips, year, totalpopulation)

  x=codemogAPI::county_sya(fips, 3000)%>%   
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    bind_rows(CO)%>%
    filter(countyfips==fips, year>=est_year)%>%
    group_by(countyfips, year)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    bind_cols(data.frame(County=rep(name, length(unique(CO$year)))))%>%
    select(County, Year=year, TotalPopulation=totalpopulation)
  
  
  
  return(x)
}


#### Components of Change Graph and Data ####


## Generates a Plotly Chart
components_p=function(fips){
  
  data=codemogAPI::county_profile(fips, 1985:2017, vars="births,deaths,netmigration")%>%
    mutate(births=as.numeric(births),
           deaths=as.numeric(deaths),
           netmigration=as.numeric(netmigration),
      naturalIncrease=births-deaths)%>%
    select(countyfips, year, naturalIncrease, netMigration=netmigration)
  
  
  plot_ly(data, x=~year,y=~(naturalIncrease+netMigration), type="scatter", marker=list(color="rgb(31,74,126)"), line=list(color="rgb(31,74,126)", width=2.5, dash="solid"), name= "Total Population Change")%>%
    add_trace(x=~year,y=~naturalIncrease, type="scatter", marker=list(color="rgb(92,102,112)"), line=list(color="rgb(92,102,112)", width=2.5, dash="dot"), name= "Natural Increase")%>%
    add_trace(x=~year,y=~netMigration, type="scatter", marker=list(color="rgb(0,149,58)"), line=list(color = "rgb(0,149,58)", width=2.5, dash="dot"), name="Net Migration")%>%
    layout(
      barmode="stacked",
      title=paste("Births, Deaths, and Net Migration 1985 to", as.character(max(data$year))),
      xaxis=list(
        title="Year"),
      yaxis=list(
        title="Population Change"),
      margin=list(t=60)
    )
  
}



## Generates the data download
components_d=function(fips, name){
  
  x=codemogAPI::county_profile(fips, 1985:2017, vars="births,deaths,netmigration")%>%
    mutate(births=as.numeric(births),
           deaths=as.numeric(deaths),
           netmigration=as.numeric(netmigration),
           naturalIncrease=births-deaths)%>%
    #bind_cols(data.frame(County=rep(name, length(unique(x$year)))))%>%
    select(County=county, year, births, deaths, naturalIncrease, netMigration=netmigration)
  
  
  
  return(x)
}

#### Population Projections change by Age Group Graph ####

projections_ageGroup=function(fips){
  year1 <- 2018
  year2 <- 2025
  
  series1 <- codemogAPI::county_sya(fips,year1)
  series2 <- codemogAPI::county_sya(fips,year2)
  agedata <- inner_join(series1, series2, by="age")
  if(fips == 0) { # Adjustment for State 
    agedata <- agedata[,c(2,5,11)]
    } else {
    agedata <- agedata[,c(3,7,14)]
    }
  agedata$y <- ifelse(agedata$age <= 17,1,   #Le 17
                      ifelse(agedata$age <= 24, 2,  #18-24
                      ifelse(agedata$age <=  54, 3, #25-54
                      ifelse(agedata$age <= 64, 4,  #55-64
                      ifelse(agedata$age <= 74, 5,  #65-74
                      ifelse(agedata$age <= 84, 6, 7))))))
 # Calculation for All Ages 
  agesum1 <- agedata %>% summarize(totalpop1=sum(as.numeric(as.character(totalpopulation.x))),
                                   totalpop2=sum(as.numeric(as.character(totalpopulation.y)))) %>%
             mutate(y = 0,
                    x = (((totalpop2/totalpop1)-1)*100)) %>%
             select(x,y)
  
 # Calculation for Age Categories
  agesum2 <- agedata %>% group_by(y)  %>%
             summarize(totalpop1=sum(as.numeric(as.character(totalpopulation.x))),
                                   totalpop2=sum(as.numeric(as.character(totalpopulation.y)))) %>%
             mutate(x = (((totalpop2/totalpop1)-1)*100)) %>%
                select(x,y)
  
  data <- bind_rows(agesum1,agesum2)
  
  # Adjusting x values
  data$x <- round(data$x,0)
  data$xlen <- ifelse(data$x >= -4 & data$x <= 0, data$x - 1,
            ifelse(data$x > 0 & data$x <= 4, data$x + 1,data$x))
  data$xpos <- ifelse(data$x <= 0, data$x + 1.2,data$x - 1.2)  # Adjustment for Chart annotations
 
  
  data$y <- factor(data$y, levels=c(0,1,2,3,4,5,6,7),
                   labels = c("All Ages","0 to 17","18 to 24","25 to 54",
                              "55 to 64","65 to 74","75 to 84","85 & over"))

  yform <- list(categoryorder = "array",
                categoryarray = c('85 & over',
                                  '75 to 84',
                                  '65 to 74',
                                  '55 to 64',
                                  '25 to 54',
                                  '18 to 24',
                                  '0 to 17',
                                  'All Ages'))
 plot <-  plot_ly(data,x=~xlen,y=~y, type = 'bar', orientation = 'h',marker=list(color = "rgb(31,74,126)")) %>%
    layout(yaxis=yform,
           title=paste0("Projected Population Change by Age Group, ",year1," to ",year2),
           xaxis=list(
             title="Percent Change"))  %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = data$xpos,  y = data$y,
                    text = paste0(data$x,'%'),
                    font = list(family = 'Arial', size = 12, color= 'rgb(248, 248, 255)'),  
                    showarrow = FALSE)  

return(plot)  
}





