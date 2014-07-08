#R Shiny App for creating RMRG outbound data files 
#ui.R

library(shiny)

shinyUI(fluidPage(
  #app title
  titlePanel("Create RMRG Outbound Data Files"),
  
  #Sidebar
  sidebarLayout(
    sidebarPanel(
      h2("Overview"),
      p("RMRG web-app for creating outbound data files from default master 
        data template"),
      
      #INPUT WIDGET
      fileInput("master", label = h3("Upload Master Data File")), 
      p(strong("Note: Master file must be a .csv file and formatted in the RMRG default template format"))
      ),
    
    mainPanel(
#       downloadButton('CBCoutbound', label = 'Download CBC Outbound file'),
#       br(),br(),
#       downloadButton('ONECLICKoutbound', label = 'Download OneClick outbound file'),
#       br(),br(),
#       downloadButton('EXPERIANoutbound', label = 'Download Experian outbound file'),
#       br(),br(),
#       downloadButton('WEBRECONoutbound', label = 'Download WebRecon outbound file'),
#       br(),br(),
      downloadButton('CBCoutDL', label = 'Download CBC Outbound file')
#       h2("CBC outbound preview"),
#       tableOutput("CBCoutboundHead")
#       br(),br(),
#       downloadButton('CBCoutbound', label = 'Download CBC Outbound file'),
      
    )
  )
))