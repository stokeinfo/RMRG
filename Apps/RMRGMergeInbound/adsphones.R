adsphones <- function(master, cbc, oneclick){
  #where cbcUP and oneclickUP are dataframes of inbound data from cbc and oneclick
  
  ########OneClick Merge#######  
  # Checking numbers from OneClick against those in master
  #home phone numbers
  newhome <- which(oneclick$out_HomePhone != "" 
                   & !is.na(oneclick$out_HomePhone)
                   & !(oneclick$out_HomePhone %in% master$HOMEPHONE)) 
  #length(newhome)
  #all(!(oneclick$out_HomePhone[newhome] %in% master$Home.Phone.Number))
  
  
  #work phone numbers
  newwork <- which(oneclick$out_WorkPhone != "" 
                   & !is.na(oneclick$out_WorkPhone)
                   & !(oneclick$out_WorkPhone %in% master$POEPHONE))
  #length(newwork)
  #all(!(oneclick$out_WorkPhone[newwork] %in% master$Work.Phone.Number))
  
  
  #mobile phone numbers
  newmobile <- which(oneclick$out_MobilePhone != "" 
                     & !is.na(oneclick$out_MobilePhone)
                     & !(oneclick$out_MobilePhone %in% master$MOBILEPHONE))
  #length(newmobile)
  #all(!(oneclick$out_MobilePhone[newmobile] %in% master$Cell.Phone.Number))
  
  
  #reference phone numbers
  newref <- which(oneclick$out_Ref1Phone != "" 
                  & !is.na(oneclick$out_Ref1Phone)
                  & !(oneclick$out_Ref1Phone %in% master$REFPHONE))
  #length(newref)
  #all(!(oneclick$out_Ref1Phone[newref] %in% master$RefPhone))
  
  
  #subset dataframes of new phones with associated SSN and add rows for action codes
  OCnames <- c("SSN", "PHONE", "FNAME", "LNAME")
  OCnewhomedf <- oneclick[newhome, c("SSN", "out_HomePhone", "FirstName", "LastName")]
  names(OCnewhomedf) <- OCnames
  if(length(newhome) > 0){
    OCnewhomedf$ACTIONCODE <- "HOME"
  }
  
  
  OCnewworkdf <- oneclick[newwork, c("SSN", "out_WorkPhone", "FirstName", "LastName")]
  names(OCnewworkdf) <- OCnames
  if(length(newwork) > 0){
    OCnewworkdf$ACTIONCODE <- "POE"
  }
  
  
  OCnewmobiledf <- oneclick[newmobile, c("SSN", "out_MobilePhone", "FirstName", "LastName")]
  names(OCnewmobiledf) <- OCnames
  if(length(newmobile) > 0){
    OCnewmobiledf$ACTIONCODE <- "MOBILE"
  }
  
  
  OCnewrefdf <- oneclick[newref, c("SSN", "out_Ref1Phone", "FirstName", "LastName")]
  names(OCnewrefdf) <- OCnames
  if(length(newref) > 0){
    OCnewrefdf$ACTIONCODE <- "REFERENCE"
  }
  
  
  #rbind 
  oneclick2 <- rbind(OCnewhomedf, OCnewworkdf, OCnewmobiledf, OCnewrefdf)
  
  # #remove objects to clean up environment. Don't need if this is a function
  # rm(OCnewhomedf, OCnewworkdf, OCnewmobiledf, OCnewrefdf, newhome, newwork, newmobile, newref)
  
  #concatenate first and last names with OC: at the beginning to indicate OneClick
  oneclick2$CONTACT <- paste("OC:", oneclick2$FNAME, oneclick2$LNAME)
  
  #reorder columns
  oneclick2 <- oneclick2[, c("SSN", "PHONE", "CONTACT", "ACTIONCODE")]
  

  ########CBC Merge########
  
    
  cbcnames <- c("SSN", "FNAME", "LNAME", "ADD1", "CITY", "STATE",
                       "ZIP", "HOMEPHONE", "ADDPHONE1", "MOBILEPHONE")
  
  names(cbc) <- cbcnames
  #rm(CBCinboundnames)
  
  
  #   compare homephones in inbound to those in outbound. In the future this will get compared to the master file
  #   CBCoutbound <- read.csv("./scripts/prepped_outbound/CBCoutbound.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  
  newhome <- which(cbc$HOMEPHONE != "" 
                   & !is.na(cbc$HOMEPHONE)
                   & !(cbc$HOMEPHONE %in% master$HOMEPHONE)) 
  
  
  newmobile <- which(CBCinbound$MOBILEPHONE != "" 
                     & !is.na(CBCinbound$MOBILEPHONE)
                     & !(CBCinbound$MOBILEPHONE %in% master$MOBILEPHONE))
  
  #ignore the extensions in the addl phones
  CBCinbound$ADDPHONE1 <- substr(CBCinbound$ADDPHONE1, 1, 10)
  
  addlmobile <- which(CBCinbound$ADDPHONE1 != "" 
                      & !is.na(CBCinbound$ADDPHONE1)
                      & !(CBCinbound$ADDPHONE1 %in% master$HOMEPHONE)
                      & !(CBCinbound$ADDPHONE1 %in% master$MOBILEPHONE))
  
  #subset dataframes of new phones with associated SSN and add rows for action codes
  CBCnames <- c("SSN", "PHONE", "FNAME", "LNAME")
  
  CBCnewhomedf <- CBCinbound[newhome, c("SSN", "HOMEPHONE", "FNAME", "LNAME")]
  names(CBCnewhomedf) <- CBCnames
  if(length(newhome) > 0){
    CBCnewhomedf$ACTIONCODE <- "HOME"
  }
  
  
  CBCnewmobiledf <- CBCinbound[newmobile, c("SSN", "MOBILEPHONE", "FNAME", "LNAME")]
  names(CBCnewmobiledf) <- CBCnames
  if(length(newmobile) > 0){
    CBCnewmobiledf$ACTIONCODE <- "MOBILE"
  }
  
  
  CBCaddldf <- CBCinbound[addlmobile, c("SSN", "ADDPHONE1", "FNAME", "LNAME")]
  names(CBCaddldf) <- CBCnames
  if(length(addlmobile) > 0){
    CBCaddldf$ACTIONCODE <- "ADDITIONAL"
  }
  
  
  
  #rbind 
  CBCinbound2 <- rbind(CBCnewhomedf, CBCnewmobiledf, CBCaddldf)
  
  # #remove objects to clean up environment. Don't need if this is a function
  # rm(CBCnewhomedf, CBCnewmobiledf, CBCaddldf, newhome, newmobile, addlmobile)
  
  #concatenate first and last names with OC: at the beginning to indicate OneClick
  CBCinbound2$CONTACT <- paste("CBC:", CBCinbound2$FNAME, CBCinbound2$LNAME)
  
  #reorder
  CBCinbound2 <- CBCinbound2[, c("SSN", "PHONE", "CONTACT", "ACTIONCODE")]
  
  #duplicates to oneclick?
  print(paste(length(which(CBCinbound2$PHONE %in% oneclick2$PHONE)), "records in CBC are duplicates to OneClick, deleting duplicate records now..."))
  
  CBCinbound2 <- CBCinbound2[!CBCinbound2$PHONE %in% oneclick2$PHONE, ]
  
  #rbind oneclick and CBC together
  ADSphones <- rbind(oneclick2, CBCinbound2)
  
  write.csv(ADSphones, file=paste("./upload/",productname,"_ADSphones_", Sys.Date(),".csv"), row.names=FALSE)
  
}