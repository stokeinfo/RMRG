#MergeInbound server.R script
source("prepmasterV2_shiny.R")
source("adsphones.R")
source("mergedmaster.R")

#line below increases the allowed file size to 100MB
options(shiny.maxRequestSize=30*1024^2)

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
  })
    
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
    oneclick <- read.csv(oneclickIN$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    #now create ADSphones dataframe with adsphones function
    adsphones(master=master(), cbc=cbc, oneclick=oneclick)
    
  })
    
    
  #merge experian and webrecon data into mergedmaster in reactive function
  merged <- reactive({
    
    #reading in experian
    experianIN <- input$experianUP
    if(is.null(experianIN)){
      return(NULL)
    }
    exp <- read.table(experianIN$datapath, sep=",", header=TRUE)
    
    #reading in webrecon
    webreconIN <- input$webreconUP
    if(is.null(webreconIN)){
      return(NULL)
    }
    wr <- read.csv(webreconIN$datapath, header=TRUE, sep=",", stringsAsFactors=FALSE)
    
    #now create mergedmaster by sending to mergedmaster.R
    mergedmaster(master=master(), experian=exp, webrecon=wr)    
    
  })
  
  #outputs...
  
  output$ADSphones <- downloadHandler(
    filename = function(){paste("ADSphones_",Sys.Date(), ".csv", sep="")},
    content = function(file){
      write.csv(ADSphones(), file)
    })
  
  output$merged <- downloadHandler(
    filename = function(){paste("mergedmaster_", Sys.Date(), ".csv", sep="")},
    content = function(file){
      write.csv(merged(), file)
    })
  
  
})