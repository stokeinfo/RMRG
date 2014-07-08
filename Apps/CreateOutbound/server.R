#R Shiny App for creating RMRG outbound data files 
#server.R

#load required libraries
library(shiny)

#source required scripts
source("prepmaster_shiny.R")
source("createCBC.R")

# Define server logic required
shinyServer(function(input, output) {
  
  CBCoutbound <- reactive({
    #reading in input master file
    inFile <- input$master
    
    if (is.null(inFile))
      return(NULL)
    
    master <- read.csv(inFile$datapath, sep=",", header=TRUE, stringsAsFactors=FALSE, nrows=6)
    
    #remove all the trailing columns with X names with prepmaster.R
    master <- prepmaster(master)
    #create CBC outbound with createCBC.R
    CBCoutbound <- createCBC(master)
  })
  
  
  #download CBCoutbound
  output$CBCoutDL <- downloadHandler(
    filename = "CBCoutbound.csv",
    content = function(file) {
      write.csv(CBCoutbound(), file)
    })
  
#   #output CBCoutbound preview
#   output$CBCoutboundHead <- renderTable({    
# #     #create CBC outbound with createCBC.R
# #     CBCoutbound <- createCBC(master())
#     head(CBCoutbound())    
#   })  
})