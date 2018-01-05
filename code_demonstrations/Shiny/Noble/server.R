#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(Noble)
library(zoo)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  months=reactive({
    substr(zoo::as.Date(Noble::NEON.avail(dpID = "DP1.00001.001")$Month[Noble::NEON.avail(dpID = "DP1.00001.001")[,colnames(Noble::NEON.avail(dpID = "DP1.00001.001"))==input$site]=="x"]), start=0, stop=7)
  }) 
  output$windInfo=reactive({input$wind_hover})
  observe({
    x = input$site
    if(is.null(x))
      x=character(0)
    
    updateSelectInput(session, "month", choices = months(), selected = tail(months()))
  })
  output$windRose <- renderPlot({
    
    Noble::plot.wind.rose(site=input$site,bgn.month = input$month, end.month = input$month)
    
  })
  
})
