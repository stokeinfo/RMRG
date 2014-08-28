source("prepmasterV2_shiny.R")
source("createoutboundV2_shiny.R")

shinyServer(function(input, output){
  
  #use reactive function to read in master file, prep, and hold
  DLfile <- reactive({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    #shows the file properties with datapath being the local temp filename     
    inFile <- input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }
    master <- read.csv(inFile$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    master <- prepmaster(master)
    
    outfile <- switch(input$filechoice, 
                     "CBC" = "CBC",
                     "OneClick" = "OneClick",
                     "WebRecon" = "WebRecon",
                     "Experian" = "Experian"
                     )
    
  createoutbound(master, filechoice=input$filechoice)    
  })
  
  
  ###outputs
  output$downloadData <- downloadHandler(    
    filename = function(){
      switch(input$filechoice,
             "CBC" = paste("CBCoutbound_", Sys.Date(), ".csv", sep=""),
             "OneClick" = paste("ONECLICKoutbound_", Sys.Date(), ".csv", sep=""),
             "WebRecon" = paste("WEBRECONoutbound_", Sys.Date(), ".csv", sep=""),
             "Experian" = "CA.C0550263.S2299570.FILE1.TXT"
      )
    },
    content = function(file) {
      if(input$filechoice == "Experian"){
        write.table(DLfile(), file, sep=",", na="", row.names=FALSE, col.names=FALSE, quote=FALSE)
      } else {
        write.csv(DLfile(), file, row.names=FALSE)
      }
    }
  )    
  
  output$preview <- renderText({
#     b <- switch(input$filechoice, 
#                 "CBC" = "CBC",
#                 "OneClick" = "OneClick",
#                 "WebRecon" = "WebRecon",
#                 "Experian" = "Experian"
#                 )
    paste(input$filechoice, "file preview...")
  })
    
  output$table <- renderTable({
    
    head(DLfile())
  })
  
})