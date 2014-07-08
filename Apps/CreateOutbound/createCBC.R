#CBCoutbound function for CreateOutbound Shiny App

createCBC <- function(master){
  
  #######CBC Outbound#######
  
  CBCoutboundNames <- c("SSN", "FName", "LName", "Address", "City", "State",
                        "Zip", "HomePhone", "MobilePhone", "AddlPhone1", "AddlPhone2", "AddlPhone3",
                        "AddlPhone4", "AddlPhone5", "AddlPhone6")
  
  #subset master to create CBCoutbound
  CBCoutbound <- master[, c("SSN", "FName", "LName", "Street.1", "City", "State", "Zip.Code", 
                            "Home.Phone.Number", "Cell.Phone.Number")]
  
  #adding in extra "out_MobilePhone" columns
  column <- 10:15 #seq(from=10, to=15, by=1)
  
  for(i in 1:6){
    CBCoutbound[, column[i]] <- NA
  }
  
  #add in column names
  colnames(CBCoutbound) <- CBCoutboundNames
  # colnames(CBCoutbound)[10:15] <- "out_MobilePhone"
  
  #remove duplicate SSNs
  CBCoutbound <- CBCoutbound[!duplicated(CBCoutbound$SSN), ]
  
  return(CBCoutbound)
}