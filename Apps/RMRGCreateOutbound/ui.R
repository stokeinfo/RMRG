#scratch ui.R script

library(shiny)

shinyUI(fluidPage(
  #title
  titlePanel(
    list(
      img(src = "RMRG-Logo-Small1.png", height = 400, width = 400),
      br(),
      "Create New Biz Outbound Vendor Files"
    )
    ),
  
  #side panel
  sidebarLayout(
    sidebarPanel(
      h2("Overview"),
      p("RMRG web-app for creating outbound data files from default master 
        data template"),
      
      #UPLOAD MASTER FILE INPUT WIDGET
      fileInput("file1", label = h3("Step 1: Upload Master Data File")), 
      p(strong("Note: Master file must be a .csv file and formatted in the RMRG default master template format")),
      
      selectInput("filechoice", label = h3("Step 2: Select an outbound file to download"),
                  choices = list("CBC",
                                 "OneClick",
                                 "WebRecon", 
                                 "Experian"),
                  selected = "CBC")),
    
    mainPanel(
      h3("Step 3: Download file"),
      downloadButton("downloadData", "Download"),
      textOutput("preview"),      
      tableOutput("table")
      
      )
    )
  ))