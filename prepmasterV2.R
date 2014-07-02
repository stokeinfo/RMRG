prepmaster <- function(){
  
  setwd("C:/Users/Jonathan/Desktop/Stoke Informatics/Clients/RMRG")
  
  #read in master
  master <- read.csv("052014-RMRG Purchase.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  #which columns start with "X"... delete
  deletecolumns <- grep("^X", names(master))
  master <- master[-deletecolumns]
  #remove delete columns, no longer needed
  rm(deletecolumns)
  return(master)
}

