#Rocky Mountain Recovery Group 6/4/14
#Script for merging inbound data from vendors into master file
#master arg in function is the prepped and ready master file.
mergeinbound <- function(master){
  
  ########OneClick Merge#######
  oneclick <- read.csv("./inbound/RMRG_1291_RMRG_1291_RockyMountainPurchased523OneClick.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  # Checking numbers from OneClick against those in master
  #home phone numbers
  newhome <- which(oneclick$out_HomePhone != "" 
                   & !is.na(oneclick$out_HomePhone)
                   & !(oneclick$out_HomePhone %in% master$Home.Phone.Number)) 
  #length(newhome)
  #all(!(oneclick$out_HomePhone[newhome] %in% master$Home.Phone.Number))
  
  
  #work phone numbers
  newwork <- which(oneclick$out_WorkPhone != "" 
                   & !is.na(oneclick$out_WorkPhone)
                   & !(oneclick$out_WorkPhone %in% master$Work.Phone.Number))
  #length(newwork)
  #all(!(oneclick$out_WorkPhone[newwork] %in% master$Work.Phone.Number))
  
  
  #mobile phone numbers
  newmobile <- which(oneclick$out_MobilePhone != "" 
                     & !is.na(oneclick$out_MobilePhone)
                     & !(oneclick$out_MobilePhone %in% master$Cell.Phone.Number))
  #length(newmobile)
  #all(!(oneclick$out_MobilePhone[newmobile] %in% master$Cell.Phone.Number))
  
  
  #reference phone numbers
  newref <- which(oneclick$out_Ref1Phone != "" 
                  & !is.na(oneclick$out_Ref1Phone)
                  & !(oneclick$out_Ref1Phone %in% master$RefPhone))
  #length(newref)
  #all(!(oneclick$out_Ref1Phone[newref] %in% master$RefPhone))
  
  
  
  #subset dataframes of new phones with associated SSN and add rows for action codes
  OCnames <- c("SSN", "PHONE", "FIRSTNAME", "LASTNAME")
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
  oneclick2$CONTACT <- paste("OC:", oneclick2$FIRSTNAME, oneclick2$LASTNAME)
  
  #reorder columns
  oneclick2 <- oneclick2[, c("SSN", "PHONE", "CONTACT", "ACTIONCODE")]
  
  # #oneclick is dirty data...numbers aren't repeats against the same class in the master file
  # #but they are repeats against different classes
  # all(!(oneclick2$PHONE %in% master$Home.Phone.Number))
  # all(!(oneclick2$PHONE %in% master$Work.Phone.Number))
  # all(!(oneclick2$PHONE %in% master$Cell.Phone.Number))
  # all(!(oneclick2$PHONE %in% master$RefPhone))
  # 
  # #4 dataframes below show cross class repeats in the data from Oneclick. It's more than 25% of data from OneClick
  # length(which(oneclick2$PHONE %in% master$Home.Phone.Number)) #24 are repeats against home phones in master
  # OChomerepeats <- oneclick2[which(oneclick2$PHONE %in% master$Home.Phone.Number), ]
  # 
  # length(which(oneclick2$PHONE %in% master$Work.Phone.Number)) #32 are repeats against work in master
  # OCworkrepeats <- oneclick2[which(oneclick2$PHONE %in% master$Work.Phone.Number), ]
  # 
  # length(which(oneclick2$PHONE %in% master$Cell.Phone.Number)) #466 are repeats against cells in master
  # OCcellrepeats <- oneclick2[which(oneclick2$PHONE %in% master$Cell.Phone.Number), ]
  # 
  # length(which(oneclick2$PHONE %in% master$RefPhone)) #28 are repeats against reference phones in master
  # OCreferencerepeats <- oneclick2[which(oneclick2$PHONE %in% master$RefPhone), ]
  
  
  ########CBC########
  
  CBCinbound <- read.csv("./inbound/CBC_inbound.csv", header=FALSE, stringsAsFactors=FALSE)[, 1:10]
  
  CBCinboundnames <- c("SSN", "FName", "LName", "Address", "City", "State",
                       "Zip", "HomePhone", "AddlPhone", "MobilePhone")
  
  colnames(CBCinbound) <- CBCinboundnames
  #rm(CBCinboundnames)
  
  
  #compare homephones in inbound to those in outbound. In the future this will get compared to the master file
  CBCoutbound <- read.csv("./scripts/prepped_outbound/CBCoutbound.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  
  newhome <- which(CBCinbound$HomePhone != "" 
                   & !is.na(CBCinbound$HomePhone)
                   & !(CBCinbound$HomePhone %in% CBCoutbound$HomePhone)) 
  
  
  newmobile <- which(CBCinbound$MobilePhone != "" 
                     & !is.na(CBCinbound$MobilePhone)
                     & !(CBCinbound$MobilePhone %in% CBCoutbound$MobilePhone))
  
  #ignore the extensions in the addl phones
  CBCinbound$AddlPhone <- substr(CBCinbound$AddlPhone, 1, 10)
  
  addlmobile <- which(CBCinbound$AddlPhone != "" 
                      & !is.na(CBCinbound$AddlPhone)
                      & !(CBCinbound$AddlPhone %in% CBCoutbound$HomePhone)
                      & !(CBCinbound$AddlPhone %in% CBCoutbound$MobilePhone))
  
  #subset dataframes of new phones with associated SSN and add rows for action codes
  CBCnames <- c("SSN", "PHONE", "FIRSTNAME", "LASTNAME")
  
  CBCnewhomedf <- CBCinbound[newhome, c("SSN", "HomePhone", "FName", "LName")]
  names(CBCnewhomedf) <- CBCnames
  if(length(newhome) > 0){
    CBCnewhomedf$ACTIONCODE <- "HOME"
  }
  
  
  CBCnewmobiledf <- CBCinbound[newmobile, c("SSN", "MobilePhone", "FName", "LName")]
  names(CBCnewmobiledf) <- CBCnames
  if(length(newmobile) > 0){
    CBCnewmobiledf$ACTIONCODE <- "Mobile"
  }
  
  
  CBCaddldf <- CBCinbound[addlmobile, c("SSN", "AddlPhone", "FName", "LName")]
  names(CBCaddldf) <- CBCnames
  if(length(addlmobile) > 0){
    CBCaddldf$ACTIONCODE <- "ADDITIONAL"
  }
  
  
  
  #rbind 
  CBCinbound2 <- rbind(CBCnewhomedf, CBCnewmobiledf, CBCaddldf)
  
  # #remove objects to clean up environment. Don't need if this is a function
  # rm(CBCnewhomedf, CBCnewmobiledf, CBCaddldf, newhome, newmobile, addlmobile)
  
  #concatenate first and last names with OC: at the beginning to indicate OneClick
  CBCinbound2$CONTACT <- paste("CBC:", CBCinbound2$FIRSTNAME, CBCinbound2$LASTNAME)
  
  #reorder
  CBCinbound2 <- CBCinbound2[, c("SSN", "PHONE", "CONTACT", "ACTIONCODE")]
  
  #duplicates to oneclick?
  print(paste(length(which(CBCinbound2$PHONE %in% oneclick2$PHONE)), "records in CBC are duplicates to OneClick, deleting duplicate records now..."))
  
  CBCinbound2 <- CBCinbound2[!CBCinbound2$PHONE %in% oneclick2$PHONE, ]
  
  #rbind oneclick and CBC together
  ADSphones <- rbind(oneclick2, CBCinbound2)
  
  write.csv(ADSphones, file="./scripts/toupload/ADSphones.csv", row.names=FALSE)
  
  
  ########Experian Merge########
  #read in data
  
  EXPERIANinbound <- read.table("./inbound/COLL.C0550263.S2299570.060514.15323683.TXT", sep=",", header=TRUE)
  
  scorerows <- which(EXPERIANinbound$SSN %in% master$SSN)
  
  #create subset of SSN and scores
  EXPresults <- EXPERIANinbound[scorerows, c("SSN", "SCORE.OR.EXCLUSION.1","X...SCR.FACTOR.CDS..1")]
  
  # #block below will not be necessary in the future since account ids will be used
  # ############Inspecting for duplicates in EXPresults##############
  # #how many unique SSNs are in EXPresults, 2606? Comparing this to the dim(EXPresults) shows there are
  # #2602 unique SSNs
  # length(unique(EXPresults$SSN))
  # 
  # #which SSNs have duplicates in EXPresults
  # unique(EXPresults$SSN[duplicated(EXPresults$SSN)])
  # 
  # #shows duplicates. Real number of occurances in EXPresults is equal to n+1. So since this reveals 
  # #4 SSNs with no repeats, the total number of duplicate items is equal to 8. Shown below...
  # EXPresults$SSN[duplicated(EXPresults$SSN)]
  # 
  # #which rows in EXPresults have duplicates, how long == 8 like mentioned above.
  # length(which(EXPresults$SSN %in% unique(EXPresults$SSN[duplicated(EXPresults$SSN)])))
  # 
  # #show rows in EXPresults with the duplicated SSNs
  # EXPresults[which(EXPresults$SSN %in% unique(EXPresults$SSN[duplicated(EXPresults$SSN)])),]
  # #show the complete rows in EXPERIANinbound with duplicated SSNs, should be 8
  # EXPERIANinbound[EXPERIANinbound$SSN %in% unique(EXPresults$SSN[duplicated(EXPresults$SSN)]), ]
  # 
  # #show the repeat SSN complete entries (repeat meaning after the first occurance)
  # unique(EXPresults[duplicated(EXPresults$SSN), ])
  # 
  # 
  # 
  # #########look at duplicate SSNs in master#########
  # #how many unique SSNs in master?
  # length(unique(master$SSN))
  # 
  # #show the repeats. shows there is one SSN that is repeated in EXPresults but not the master (423135663). 
  # #perhaps the additional came from a previous debt list?
  # master$SSN[duplicated(master$SSN)]
  # unique(master$SSN[duplicated(master$SSN)])
  # 
  # #show those rows
  # master[master$SSN %in% unique(master$SSN[duplicated(master$SSN)]),]
  
  #do the merge and then isolate these duplicates to illustrate that we can't make it programmatic  because 
  #primary keys using SSN alone not sufficient to give a score that relates to each of these customers' debts
  #The unique identifier is RMRG number, the master list does not have the RMRG number...
  
  
  #merge 
  master2 <- merge(master, EXPresults, by.x="SSN", by.y="SSN", all.y=TRUE)
  
  # #master2 has the same duplicated entries as EXPresults
  # unique(master2$SSN[duplicated(master2$SSN)])
  # unique(EXPresults$SSN[duplicated(EXPresults$SSN)])
  # 
  # 
  # #and adds in a duplicate for 423135663 which is not a duplicate in the master
  # unique(master$SSN[duplicated(master$SSN)])
  # 
  # #show the rows in master2 with duplicates, these ones you would want to flag as potentially inaccurate
  # #when doing any segmentation. Until we are using the correct keys to merge, these should be deleted 
  # #from list because there is now way of validating if the score for each debt is correct. 
  # master2[master2$SSN %in% unique(master2$SSN[duplicated(master2$SSN)]), ]
  # 
  # length(master2[master2$SSN %in% unique(master2$SSN[duplicated(master2$SSN)]), "SSN" ])
  
  #deleting all the duplicated rows for now
  master2 <- master2[!master2$SSN %in% unique(master2$SSN[duplicated(master2$SSN)]), ]
  
  #so where we had 2 duplicates in EXPresults and 2 duplicates in master, we end up with 4 duplicate entries
  #where we had two duplicates in EXPresults and a corresponding single in master, we get two: multiplication!
  #to fix we need to merge on account number since the scores are unique to the debt not the customer. 
  
  
  #change score name to something readable 
  
  colnames(master2)[46:47] <- c("IncScore","DollarScore")
  
  
  ########WebRecon########
  webrecon <- read.csv("./inbound/webrecon_inbound.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
  
  #only need the 11th column to flag accounts for removal
  litigiousaccounts <- webrecon$Account
  
  #which rows in master3 have litigious accounts?
  flagrows <- which(master2$UDH.AccountID %in% litigiousaccounts)
  
  #flag these rows as litigious
  master2$LitigiousAccount <- "N"
  master2$LitigiousAccount[flagrows] <- "Y"
  
  # #validation
  # master2[master2$UDH.AccountID %in% litigiousaccounts, c("UDH.AccountID", "LitigiousAccount")]
  # all(master$UDH.AccountID[master$UDH.AccountID %in% litigiousaccounts] %in% webrecon$Account)
  # all(master2[master2$LitigiousAccount == "Y", "UDH.AccountID"] %in% litigiousaccounts)
  
  #write the final merged file
  write.csv(master2, file="./scripts/toupload/mergedmaster.csv", row.names=FALSE)
  
  print("All inbound files have been merged and are now ready to uploade to ADS. Prepped files are in the 'toupload' forward directory")
}





