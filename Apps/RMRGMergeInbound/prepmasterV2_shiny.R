prepmaster <- function(master){
  
  #which columns start with "X"... delete
  deletecolumns <- grep("^X", names(master))
  

  if(length(deletecolumns) > 1){
    master <- master[-deletecolumns]
  }
    
  #remove delete columns, no longer needed
  rm(deletecolumns)
  return(master)
}

