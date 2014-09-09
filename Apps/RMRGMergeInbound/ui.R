#MergeInbound ui.R script

library(shiny)

shinyUI(fluidPage(
  #title
  titlePanel(
    list(
      img(src = "RMRG-Logo-Small1.png", height = 400, width = 400),
      br(),
      "Merge Vendor Files"
    )
  ),
  
  #side panel
  sidebarLayout(
    sidebarPanel(
      h2("Overview"),
      p("RMRG web-app for merging inbound vendor data files into master 
        new biz data file"),
      
      #upload master file input widget
      h4("Step 1: Upload unmerged master new biz file"),
      fileInput("masterUP", label = "Master file must be a .csv file and formatted in the RMRG default master template format"), 
#       p(strong("Master file must be a .csv file and formatted in the RMRG default master template format")),
      
      #upload inbound file widgets
      h4("Step 2: Upload CBC inbound file"),
      fileInput("cbcUP", label = "Must be a .csv file"),
#       p(strong("Must be a .csv file")),
      
      h4("Step 3: Upload OneClick inbound file"),
      fileInput("oneclickUP", label = "Must be a .csv file"),
#       p(strong("Must be a .csv file")),
      
      h4("Step 4: Upload WebRecon inbound file"),
      fileInput("webreconUP", label = "Must be a .csv file"),
#       p(strong("Must be a .csv file")),
      
      h4("Step 5: Upload Experian inbound file"),
      fileInput("experianUP", label = "Must be a .txt file")
#       p(strong("Must be a .txt file"))
      
    ),
    
    mainPanel(
      h4("Step 6: Download merged phone numbers (ADSphones.csv) for ADS LaunchPad"),
      downloadButton("ADSphones", "Download"),
      h4("Step 7: Download complete merged master new biz file"),
      downloadButton("merged", "Download")
      
      )
    )
  ))