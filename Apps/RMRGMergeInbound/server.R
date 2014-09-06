#MergeInbound server.R script
source(adsphones.R)
source(prepmasterV2_shiny.R)

shinyServer(function(input, output){
  
  #create ADSphones.csv file and hold in reactive function until user downloads
  master <- reactive({
    
    #reading in master
    masterIN <- input$masterUP    
    if(is.null(masterIN)){
      return(NULL)
    }    
    master <- read.csv(masterIN$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    #prepping master
    prepmaster(master)
    
  ADSphones <- reactive({
    
    #reading in cbc
    cbcIN <- input$cbcUP    
    if(is.null(cbcIN)){
      return(NULL)
    }
    cbc <- read.csv(cbcIN$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    #reading in oneclick    
    oneclickIN <- input$oneclickUP
    if(is.null(oneclickIN)){
      return(NULL)
    }
    oneclick <- read.csv(onclickIN$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    #now create ADSphones dataframe with adsphones function
    adsphones(master=master(), cbc=cbc, oneclick=oneclick)
    
  })
    
    
  #merge experian and webrecon data into mergedmaster in reactive function
  mergedmaster <- reactive({
    
    #reading in experian
    experianIN <- input$experianUP
    if(is.null(experianIN)){
      return(NULL)
    }
    experian <- read.table(experianIN$datapath, sep=",", header=TRUE)
    
    #reading in webrecon
    webreconIN <- input$webreconUP
    if(is.null(webreconIN)){
      return(NULL)
    }
    webrecon <- read.csv(webreconIN$datapath, header=TRUE, sep=",", stringsAsFactors=FALSE)
    
    #now create mergedmaster by sending to mergedmaster.R
    mergedmaster(master=master, experian=experian, webrecon=webrecon)    
    
  })
  
  #outputs...
  
  output$ADSphones <- downloadHandler(
    filename = function(){paste("ADSphones_",Sys.Date(), ".csv", sep="")},
    content = function(file){
      write.csv(ADSphones(), file)
    })
  
  output$ADSphones <- downloadHandler(
    filename = function(){paste("mergedmaster_", Sys.Date(), ".csv", sep="")},
    content = function(file){
      write.csv(mergedmaster(), file)
    })
  
  
})