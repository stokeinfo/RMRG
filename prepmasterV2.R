prepmaster <- function(folder=NULL, filename=NULL){
  
#   Notes
#   'folder' is the forward directory that contains the new biz data. Always named the date the new biz data was recieved
#   filename string is whatever the filename is of the master new biz data
  
  setwd("C:/Users/Jonathan/Desktop/Stoke Informatics/Clients/RMRG")
  
#   check to make sure arguments entered correctly
  if(is.null(folder)){
    stop("must provide folder name that contains the master file")
  }
  
  if(is.null(filename)){
    stop("must provide master filename as string")
  }
  
  #read in master file
  setwd(paste("./", folder, sep=""))
    
  master <- read.csv(filename, sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  #which columns start with "X"... delete
  deletecolumns <- grep("^X", names(master))
  

  if(length(deletecolumns) > 1){
    master <- master[-deletecolumns]
  }
    
  #remove delete columns, no longer needed
  rm(deletecolumns)
  return(master)
}

