#R Shiny App for creating RMRG outbound data files 
#server.R

#load required libraries
library(shiny)

#source required scripts
source("prepmaster.R")
#source("createoutbound.R")

# Define server logic required
shinyServer(function(input, output) {
  
  output$masterHead <- renderTable({
    
    inFile <- input$master
    
    if (is.null(inFile))
      return(NULL)
    
    master <- read.csv(inFile$datapath, sep=",", header=TRUE, stringsAsFactors=FALSE, nrows=6)
    
    #remove all the trailing columns with X names with prepmaster.R
    prepmaster(master)
    
  })  
})