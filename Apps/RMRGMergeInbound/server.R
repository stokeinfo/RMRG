#MergeInbound server.R script

source(prepmasterV2_shiny.R)

shinyServer(function(input, output){
  
  #create ADSphones.csv file and hold in reactive function until user downloads
  ADSphones <- reactive({
    
    #reading in master
    masterIN <- input$masterUP    
    if(is.null(masterIN)){
      return(NULL)
    }    
    master <- read.csv(masterIN$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    #prepping master
    master <- prepmaster(master)
    
    #reading in cbc
    cbcIN <- input$cbcUP    
    cbc <- read.csv(cbcIN$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    #reading in oneclick
    
    oneclickIN <- input$oneclickUP
    oneclick <- read.csv(onclickIN$datapath, header=T, sep=",", stringsAsFactors=FALSE)
    
    #now create ADSphones dataframe with adsphones function
    
    
  })
  
  #read in cbc file and hold in reactive function
  cbcR <- reactive({
    inFile <- input$cbcUP
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    read.csv(inFile$datapath, header=T, sep=",", stringsAsFactors=FALSE)
  })
  
  #read in oneclick file and hold in reactive function
  oneclickR <- reactive({
    inFile <- input$oneclickUP
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    read.csv(inFile$datapath, header=T, sep=",", stringsAsFactors=FALSE)
  })
  
  #read in webrecon file and hold in reactive function
  
})