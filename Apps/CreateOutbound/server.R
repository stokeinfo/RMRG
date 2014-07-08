#R Shiny App for creating RMRG outbound data files 
#server.R

#load required libraries
library(shiny)

# #source required scripts
# source("prepmaster.R")
# source("createoutbound.R")

# Define server logic required
shinyServer(function(input, output) {
  
  output$masterStructure <- renderTable({
    
    input$master    
  })  
})