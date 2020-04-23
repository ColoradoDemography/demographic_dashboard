library(dplyr)
library(tidyr)
library(plotly)

#commands to create county_forecast.rdata and county_profile.rdata after CSVs are imported
# county_forecast <- read.csv("county_sya.csv", sep = ';')
# save(county_forecast, file = "county_forecast.rdata")
# county_profile <- read.csv("county_profiles.csv", sep = ';')
# save(county_profile, file = "county_profile.rdata")


#load data
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
  
  #data=codemogAPI::county_profile(fips, 1985:2018, vars="totalpopulation")
  data=county_profile%>%
    filter(countyfips==fips)%>%
    select(countyfips, year, householdpopulation, groupquarterspopulation)
  
  plot_ly(data, x= ~year, y= ~as.numeric(householdpopulation+groupquarterspopulation), type= "bar", marker=list(color = "rgb(31,74,126)"))%>%
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
  
  #x=codemogAPI::county_profile(fips, 1985:2018, vars="totalpopulation")
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
  
  
  # CO=codemogAPI::county_sya(0, 3000)%>% # Creates data for the state as a whole since that isn't in the data frame.   
  #   filter(year>=est_year)%>%
  #   mutate(totalpopulation=as.numeric(totalpopulation))%>%
  #   group_by(year)%>%
  #   summarize(totalpopulation=sum(totalpopulation))%>%
  #   mutate(countyfips=0)%>%
  #   select(countyfips, year, totalpopulation)
  
  CO=county_forecast%>%
    filter(year>=est_year)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    group_by(year)%>%
    summarize(totalpopulation=sum(totalpopulation))%>%
    mutate(countyfips=0)%>%
    select(countyfips, year, totalpopulation)
  
  # data=codemogAPI::county_sya(fips, 3000)%>%
  #   mutate(totalpopulation=as.numeric(totalpopulation))%>%
  #   # bind_rows(CO)%>%
  #   filter(countyfips==fips, year>=est_year)%>%
  #   group_by(countyfips, year)%>%
  #   summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
  #   select(countyfips, year, totalpopulation)
  
  data=county_forecast%>%
    bind_rows(CO)%>%
    filter(countyfips==fips, year>=est_year)%>%
    group_by(countyfips, year)%>%
    summarize(totalpopulation=sum(totalpopulation))%>%
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
  
  # CO=codemogAPI::county_sya(0,3000)%>% # Creates data for the state as a whole since that isn't in the data frame.   
  #   mutate(totalpopulation=as.numeric(totalpopulation))%>%
  #   filter(year>=est_year)%>%
  #   group_by(year)%>%
  #   summarize(totalpopulation=sum(totalpopulation))%>%
  #   mutate(countyfips=0)%>%
  #   select(countyfips, year, totalpopulation)
  
  CO=county_forecast%>% # Creates data for the state as a whole since that isn't in the data frame.   
    #mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(year>=est_year)%>%
    group_by(year)%>%
    summarize(totalpopulation=sum(totalpopulation))%>%
    mutate(countyfips=0)%>%
    select(countyfips, year, totalpopulation)
   
  # x=codemogAPI::county_sya(fips, 3000)%>%   
  #   mutate(totalpopulation=as.numeric(totalpopulation))%>%
  #   bind_rows(CO)%>%
  #   filter(countyfips==fips, year>=est_year)%>%
  #   group_by(countyfips, year)%>%
  #   summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
  #   bind_cols(data.frame(County=rep(name, length(unique(CO$year)))))%>%
  #   select(County, Year=year, TotalPopulation=totalpopulation)
  
  x=county_forecast%>%   
    #mutate(totalpopulation=as.numeric(totalpopulation))%>%
    bind_rows(CO)%>%
    filter(countyfips==fips, year>=est_year)%>%
    group_by(countyfips, year)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    bind_cols(data.frame(County=rep(name, length(unique(county_forecast$year)))))%>%
    select(County, Year=year, TotalPopulation=totalpopulation)
  
  return(x)
}


#### Components of Change Graph and Data ####


## Generates a Plotly Chart
components_p=function(fips){
  
  # data=codemogAPI::county_profile(fips, 1985:2018, vars="births,deaths,netmigration")%>%
  #   mutate(births=as.numeric(births),
  #          deaths=as.numeric(deaths),
  #          netmigration=as.numeric(netmigration),
  #     naturalIncrease=births-deaths)%>%
  #   select(countyfips, year, naturalIncrease, netMigration=netmigration)
  
  data=county_profile%>%
    filter(countyfips==fips)%>%
    select(countyfips, year, naturalincrease, netmigration)
  
  
  plot_ly(data, x=~year,y=~(naturalincrease+netmigration), type="scatter", marker=list(color="rgb(31,74,126)"), line=list(color="rgb(31,74,126)", width=2.5, dash="solid"), name= "Total Population Change")%>%
    add_trace(x=~year,y=~naturalincrease, type="scatter", marker=list(color="rgb(92,102,112)"), line=list(color="rgb(92,102,112)", width=2.5, dash="dot"), name= "Natural Increase")%>%
    add_trace(x=~year,y=~netmigration, type="scatter", marker=list(color="rgb(0,149,58)"), line=list(color = "rgb(0,149,58)", width=2.5, dash="dot"), name="Net Migration")%>%
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
  
  # x=codemogAPI::county_profile(fips, 1985:2018, vars="births,deaths,netmigration")%>%
  #   mutate(births=as.numeric(births),
  #          deaths=as.numeric(deaths),
  #          netmigration=as.numeric(netmigration),
  #          naturalIncrease=births-deaths)%>%
  #   #bind_cols(data.frame(County=rep(name, length(unique(x$year)))))%>%
  #   select(County=county, year, births, deaths, naturalIncrease, netMigration=netmigration)

  x=county_profile%>%
    filter(countyfips==fips)%>%
    bind_cols(data.frame(County=rep(name, length(unique(county_profile$year)))))%>%
    select(County=county, year, births, deaths, naturalincrease, netmigration)
  
  
  return(x)
}

#### Population Projections change by Age Group Graph ####

projections_ageGroup=function(fips){
  year1<-2020
  year2<-2025
  CO=county_forecast%>% # Creates data for the state as a whole since that isn't in the data frame.   
    #mutate(totalpopulation=as.numeric(totalpopulation))%>%
    #filter(year>=est_year)%>%
    group_by(year,age)%>%
    summarize(totalpopulation=sum(totalpopulation))%>%
    mutate(countyfips=0)%>%
    select(countyfips, year, totalpopulation, age)
  county_forecast<-bind_rows(county_forecast,CO)
  x017y1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=0, age<=17)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x017y2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=0, age<=17)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg017<-(((x017y2/x017y1)-1)*100)
  x1624y1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=16, age<=24)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x1624y2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=16, age<=24)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg1624<-(((x1624y2/x1624y1)-1)*100)
  x2554y1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=25, age<=54)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x2554y2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=25, age<=54)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg2554<-(((x2554y2/x2554y1)-1)*100)
  x5564y1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=55, age<=64)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x5564y2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=55, age<=64)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg5564<-(((x5564y2/x5564y1)-1)*100)
  x6574y1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=65, age<=74)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x6574y2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=65, age<=74)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg6574<-(((x6574y2/x6574y1)-1)*100)
  x7584y1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=75, age<=84)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x7584y2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=75, age<=84)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg7584<-(((x7584y2/x7584y1)-1)*100)
  x85y1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=85, age<=100)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x85y2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=85, age<=100)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg85<-(((x85y2/x85y1)-1)*100)
  xtotaly1=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  xtotaly2=county_forecast%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chgtotal<-(((xtotaly2/xtotaly1)-1)*100)%>%
    select(totalpopulation)
#  x<-c(as.integer(Chgtotal),as.integer(Chg85),as.integer(Chg6574),as.integer(Chg5564),as.integer(Chg2554),
#       as.integer(Chg1624),as.integer(Chg017))
  x<-c(round(Chgtotal[,1],digits=1),round(Chg85[,1], digits=1),round(Chg7584[,1],digits=1),round(Chg6574[,1],digits=1),
       round(Chg5564[,1],digits=1),round(Chg2554[,1],digits=1),round(Chg1624[,1],digits=1),round(Chg017[,1],digits=1))
  
  y<-c('All ages', '85 & over','75 to 84','65 to 74','55 to 64',
       '25 to 54','16 to 24','0 to 17')
  data <- data.frame(y,x)
  yform <- list(categoryorder = "array",
                categoryarray = c('85 & over',
                                  '75 to 84',
                                  '65 to 74',
                                  '55 to 64',
                                  '25 to 54',
                                  '16 to 24',
                                  '0 to 17',
                                  'All ages'))
  plot_ly(data,x=~x,y=~y, type = 'bar', orientation = 'h',marker=list(color = "rgb(31,74,126)"))%>%
    layout(yaxis=yform,
           title=paste("Projected Population Change by Age Group, 2020 to 2025"),
           xaxis=list(
             title="Percent Change"),
           margin=list(t=60))%>%
    add_annotations(xref = 'x', yref = 'age',
                    x = x,  y = y,
                    text = paste(round(x, 0),'%          '),
                    font = list(family = 'Arial', size = 14, color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE)
  
}





