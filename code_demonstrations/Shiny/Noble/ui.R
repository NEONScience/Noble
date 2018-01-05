#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(Noble)

shinyUI(fluidPage(
  navbarPage("Noble"),
  tabPanel("Wind Rose Plotting"),
  tabPanel("Test"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(choices=Noble::tis_site_config$SiteID,
                  inputId = "site",
                  label="Site"),
      selectInput(choices=NULL, inputId = "month", label = "Month")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId="windRose", hover = "wind_hover")
    )
  )
))


