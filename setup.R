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
  
  plot_ly(data, x=age, y=netMigration, line=list(color = "rgb(31,74,126)"))%>%
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
  
  data=codemogAPI::county_profile(fips, 1985:2015, vars="totalpopulation")
  
  plot_ly(data, x=year, y=totalpopulation, type= "bar", marker=list(color = "rgb(31,74,126)"))%>%
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
  
  x=codemogAPI::county_profile(fips, 1985:2014, vars="totalpopulation")
    
  
  
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
    
  
  plot_ly(data, x=year, y=totalpopulation, type= "bar", marker=list(color = "rgb(31,74,126)"))%>%
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
    filter(year>est_year)%>%
    group_by(year)%>%
    summarize(totalpopulation=sum(totalpopulation))%>%
    mutate(countyfips=0)%>%
    select(countyfips, year, totalpopulation)
  
  x=codemogAPI::county_sya(fips, 3000)%>%   
    bind_rows(CO)%>%
    filter(countyfips==fips, year>=est_year)%>%
    group_by(countyfips, year)%>%
    summarize(totalpopulation=sum(totalpopulation))%>%
    bind_cols(data.frame(County=rep(name, length(unique(county_forecast$year)))))%>%
    select(County, Year=year, TotalPopulation=totalpopulation)
  
  
  
  return(x)
}


#### Components of Change Graph and Data ####


## Generates a Plotly Chart
components_p=function(fips){
  
  data=codemogAPI::county_profile(fips, 1985:2015, vars="births,deaths,netmigration")%>%
    mutate(births=as.numeric(births),
           deaths=as.numeric(deaths),
           netmigration=as.numeric(netmigration),
      naturalIncrease=births-deaths)%>%
    select(countyfips, year, naturalIncrease, netMigration=netmigration)
  
  
  plot_ly(data, x=year,y=naturalIncrease+netMigration, line=list(color="rgb(31,74,126)", width=2.5, dash="solid"), name= "Total Population Change")%>%
    add_trace(x=year,y=naturalIncrease, line=list(color="rgb(0,149,58)", width=2.5, dash="dot"), name= "Natural Increase")%>%
    add_trace(x=year,y=netMigration, type= "line", line=list(color = "rgb(92,102,112)", width=2.5, dash="dot"), name="Net Migration")%>%
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

components_p(123)

## Generates the data download
components_d=function(fips, name){
  
  x=codemogAPI::county_profile(fips, 1985:2014, vars="births,deaths,netmigration")%>%
    mutate(births=as.numeric(births),
           deaths=as.numeric(deaths),
           netmigration=as.numeric(netmigration),
           naturalIncrease=births-deaths)%>%
    bind_cols(data.frame(County=rep(name, length(unique(county_profile$year)))))%>%
    select(County, year, naturalIncrease, netMigration=netmigration)
  
  
  
  return(x)
}


