library(plotly)
library(shiny)
function(req) {
  htmlTemplate("index.html",
               county=selectInput("county","Select a county:", choices = unique(county_choices$county), selected = 'Colorado'),
               net_mig_plot=plotlyOutput("netMigAge"),
               net_mig_dl=downloadButton('netMigAgeData', 'Download Data (CSV)'),
               estimates_plot=plotlyOutput("estimates"),
               estimates_dl=downloadButton('estimatesData', 'Download Data (CSV)'),
               projections_plot=plotlyOutput("projections"),
               projections_dl=downloadButton('projectionsData', 'Download Data (CSV)'),
               components_plot=plotlyOutput("components"),
               components_dl=downloadButton('componentsData', 'Download Data (CSV)')
               )
}

