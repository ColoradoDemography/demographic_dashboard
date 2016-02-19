library(dplyr)
library(tidyr)
library(plotly)



load("county_forecast.rdata")
load("county_profile.rdata")
load("county_migbyage.rdata")




county_choices=read.csv("county_names.csv", stringsAsFactors = FALSE)%>%
  select(county)

#### Net Migration by Age Graph and Data #####

## Generates a Plotly Chart
mig_age_p=function(fips){
  
  data=county_migbyage%>%    
    mutate(countyfips=as.numeric(countyfips))%>%
    filter(countyfips==fips, age<90)
  
  plot_ly(data, x=age, y=netMigration, line=list(color = "rgb(31,74,126)"))%>%
    layout(
      title="Net Migration by Age, 2000 to 2010",
      xaxis=list(
        title=" Age"),
      yaxis=list(
        title=" Net Migration")
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
  
  data=county_profile%>%    
    filter(countyfips==fips)%>%
    select(countyfips, year, householdPopulation, groupQuartersPopulation)
  
  plot_ly(data, x=year, y=householdPopulation+groupQuartersPopulation, type= "bar", marker=list(color = "rgb(31,74,126)"))%>%
    layout(
      title=paste("Population Estimates 1980 to", as.character(max(data$year))),
      xaxis=list(
        title="Year"),
      yaxis=list(
        title="Total Population")
    )
  
}

## Generates the data download
estimates_d=function(fips, name){
  
  x=county_profile%>%    
    filter(countyfips==fips)%>%
    select(countyfips, year, householdPopulation, groupQuartersPopulation)%>%
    mutate(TotalPopulation=householdPopulation+groupQuartersPopulation)%>%
    bind_cols(data.frame(County=rep(name, length(unique(county_profile$year)))))%>%
    select(County, Year=year, TotalPopulation)
    
  
  
  return(x)
}


#### Population Projections Graph and Data ####


## Generates a Plotly Chart
projections_p=function(fips, est_year){
  
  
  CO=county_forecast%>% # Creates data for the state as a whole since that isn't in the data frame.   
    filter(year>est_year)%>%
    group_by(year)%>%
    summarize(totalPopulation=sum(totalPopulation))%>%
    mutate(countyfips=0)%>%
    select(countyfips, year, totalPopulation)
  
  data=county_forecast%>%
    bind_rows(CO)%>%
    filter(countyfips==fips, year>=est_year)%>%
    group_by(countyfips, year)%>%
    summarize(totalPopulation=sum(totalPopulation))%>%
    select(countyfips, year, totalPopulation)
    
  
  plot_ly(data, x=year, y=totalPopulation, type= "bar", marker=list(color = "rgb(31,74,126)"))%>%
    layout(
      title=paste("Population Projections", as.character(est_year), "to 2050"),
      xaxis=list(
        title="Year"),
      yaxis=list(
        title="Total Population")
    )
  
}

## Generates the data download
projections_d=function(fips, name, est_year){
  
  CO=county_forecast%>% # Creates data for the state as a whole since that isn't in the data frame.   
    filter(year>est_year)%>%
    group_by(year)%>%
    summarize(totalPopulation=sum(totalPopulation))%>%
    mutate(countyfips=0)%>%
    select(countyfips, year, totalPopulation)
  
  x=county_forecast%>%   
    bind_rows(CO)%>%
    filter(countyfips==fips, year>=est_year)%>%
    group_by(countyfips, year)%>%
    summarize(totalPopulation=sum(totalPopulation))%>%
    bind_cols(data.frame(County=rep(name, length(unique(county_forecast$year)))))%>%
    select(County, Year=year, TotalPopulation=totalPopulation)
  
  
  
  return(x)
}


#### Components of Change Graph and Data ####


## Generates a Plotly Chart
components_p=function(fips){
  
  data=county_profile%>%    
    filter(countyfips==fips)%>%
    select(countyfips, year, naturalIncrease, netMigration)
  
  
  plot_ly(data, x=year, y=netMigration, type= "bar", marker=list(color = "rgb(92,102,112)"), name="Net Migration")%>%
    add_trace( y=naturalIncrease, marker=list(color="rgb(0,149,58)"), name= "Natural Increase")%>%
    layout(
      barmode="overlay",
      title=paste("Births, Deaths, and Net Migration 1985 to", as.character(max(data$year))),
      xaxis=list(
        title="Year"),
      yaxis=list(
        title="Population Change")
    )
  
}

## Generates the data download
components_d=function(fips, name){
  
  x=county_profile%>%    
    filter(countyfips==fips)%>%
    bind_cols(data.frame(County=rep(name, length(unique(county_profile$year)))))%>%
    select(County, year, naturalIncrease, netMigration)
  
  
  
  return(x)
}












############## Graph Stuff ######################

codemog_pal=c(rgb(31,73,125, max=255),
              rgb(192,80,77, max=255),
              rgb(101, 80, 60, max=255),
              rgb(239, 117, 33, max=255),
              rgb(119, 171, 67, max = 255),
              rgb(208, 210, 211, max = 255),
              rgb(210, 210, 210, max = 255))

theme_codemog <- function(base_size = 12, base_family = "sans"){
  codemog_pal=c(
    dkblu=rgb(31,73,125, max=255),
    dkred=rgb(192,80,77, max=255),
    dkgray = rgb(78, 87, 88, max = 255),
    medgray = rgb(210, 210, 210, max = 255),
    ltgray = rgb(208, 210, 211, max = 255),
    green = rgb(119, 171, 67, max = 255)
  )
  theme(
    line = element_line(),
    rect = element_blank(),
    text = element_text(colour = codemog_pal['dkgray'], size=base_size),
    axis.title = element_text(family=base_family, colour=codemog_pal['dkgray']),
    axis.text = element_text(colour=codemog_pal['dkgray'], family=base_family),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.background = element_rect(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    panel.grid = element_line(colour = NULL),
    panel.grid.major = element_line(colour = codemog_pal['medgray'], size=base_size*.05),
    panel.grid.minor = element_line(colour = codemog_pal['medgray'], size=base_size*.05),
    plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
    plot.margin = unit(c(.2, .2, .2, .2), "lines"),
    strip.background=element_rect())
}