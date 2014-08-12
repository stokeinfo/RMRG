prepmaster <- function(){
  
  setwd("C:/Users/Jonathan/Desktop/Stoke Informatics/Clients/RMRG")
  
  #read in master
  dir <- "C:/Users/Jonathan/Desktop/Stoke Informatics/Clients/RMRG/72313"
  setwd(dir)
  
  
  master <- read.csv("TSLO 07232014.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  #which columns start with "X"... delete
  deletecolumns <- grep("^X", names(master))
  master <- master[-deletecolumns]
  #remove delete columns, no longer needed
  rm(deletecolumns)
  return(master)
}

