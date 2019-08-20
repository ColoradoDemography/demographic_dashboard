
rm(list = ls())
library(dplyr)
library(tidyr)
library(plotly)
# library(codemogAPI)

#### Population Projections change by Age Group Graph ####

projections_ageGroup=function(fips){

  year1<-2018
  year2<-2025
  x017y1=codemogAPI::county_sya(fips,year1)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=0, age<=17)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x017y2=codemogAPI::county_sya(fips,year2)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=0, age<=17)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg017<-(((x017y2/x017y1)-1)*100)
  x1624y1=codemogAPI::county_sya(fips,year1)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=16, age<=24)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x1624y2=codemogAPI::county_sya(fips,year2)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=16, age<=24)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg1624<-(((x1624y2/x1624y1)-1)*100)
  x2554y1=codemogAPI::county_sya(fips,year1)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=25, age<=54)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x2554y2=codemogAPI::county_sya(fips,year2)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=25, age<=54)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg2554<-(((x2554y2/x2554y1)-1)*100)
  x5564y1=codemogAPI::county_sya(fips,year1)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=55, age<=64)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x5564y2=codemogAPI::county_sya(fips,year2)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=55, age<=64)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg5564<-(((x5564y2/x5564y1)-1)*100)
  x6574y1=codemogAPI::county_sya(fips,year1)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=65, age<=74)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x6574y2=codemogAPI::county_sya(fips,year2)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=65, age<=74)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg6574<-(((x6574y2/x6574y1)-1)*100)
  x85y1=codemogAPI::county_sya(fips,year1)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1,age >=85, age<=100)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  x85y2=codemogAPI::county_sya(fips,year2)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2,age >=85, age<=100)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chg85<-(((x85y2/x85y1)-1)*100)
  xtotaly1=codemogAPI::county_sya(fips,year1)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year1)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  xtotaly2=codemogAPI::county_sya(fips,year2)%>%
    mutate(totalpopulation=as.numeric(totalpopulation))%>%
    filter(countyfips==fips, year==year2)%>%
    summarize(totalpopulation=sum(as.numeric(as.character(totalpopulation))))%>%
    select(totalpopulation)
  Chgtotal<-(((xtotaly2/xtotaly1)-1)*100)%>%
    select(totalpopulation)
  #  x<-c(as.integer(Chgtotal),as.integer(Chg85),as.integer(Chg6574),as.integer(Chg5564),as.integer(Chg2554),
  #       as.integer(Chg1624),as.integer(Chg017))
  x<-c(round(Chgtotal[,1],digits=1),round(Chg85[,1], digits=1),round(Chg6574[,1],digits=1),round(Chg5564[,1],digits=1),
       round(Chg2554[,1],digits=1),round(Chg1624[,1],digits=1),round(Chg017[,1],digits=1))
  
  y<-c('All ages', '85 & over','65 to 74','55 to 64',
       '25 to 54','16 to 24','0 to 17')
  data <- data.frame(y,x)
  yform <- list(categoryorder = "array",
                categoryarray = c('85 & over',
                                  '65 to 74',
                                  '55 to 64',
                                  '25 to 54',
                                  '16 to 24',
                                  '0 to 17',
                                  'All ages'))
  plot_ly(data,x=~x,y=~y, type = 'bar', orientation = 'h',marker=list(color = "rgb(31,74,126)"))%>%
    layout(yaxis=yform,
           title=paste("Projected Population Change by Age Group, 2018 to 2025"),
           xaxis=list(
             title="Percent Change"),
           margin=list(t=60))%>%
    add_annotations(xref = 'x', yref = 'y',
                    x = x,  y = y,
                    text = paste(round(x, 0),'%          '),
                    font = list(family = 'Arial', size = 14, color= 'rgb(0,0,0)'),  #color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE)
  
}


growth2 <- function(fips) {
 
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

fips <- 0

x <- projections_ageGroup(fips)
x

y <- growth2(fips)
y$plot

write.csv(y$data,"J:/Estimates/Admin/AppDevelopment/demographic_dashboard master/data_chk.csv")
