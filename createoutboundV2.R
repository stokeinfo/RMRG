#Rocky Mountain Recovery Group 5/29/14
#Function for creating outbound data files to vendors
#master arg in function is the prepped and ready master file.
createoutbound <- function(master){
  
  #######CBC Outbound#######
  
  CBCoutboundNames <- c("SSN", "FNAME", "LNAME", "ADD1", "CITY", "STATE",
                        "ZIP", "HOMEPHONE", "MOBILEPHONE", "ADDPHONE1", "ADDPHONE2", "ADDPHONE3",
                        "ADDPHONE4", "ADDPHONE5", "ADDPHONE6")
  
  #subset master to create CBCoutbound
  CBCoutbound <- master[, c("SSN", "FNAME", "LNAME", "ADD1", "CITY", "STATE", "ZIP", 
                            "HOMEPHONE", "MOBILEPHONE")]
  
  #adding in extra "out_MobilePhone" columns
  column <- 10:15 #seq(from=10, to=15, by=1)
  
  for(i in 1:6){
    CBCoutbound[, column[i]] <- NA
  }
  
  #add in column names
  colnames(CBCoutbound) <- CBCoutboundNames
  # colnames(CBCoutbound)[10:15] <- "out_MobilePhone"
  
#   since keys are now ACCT, do not need to remove duplicate SSN
#   #remove duplicate SSNs
#   CBCoutbound <- CBCoutbound[!duplicated(CBCoutbound$SSN), ]
  
  # #validation
  # which(df$SSN %in% unique(CBCoutbound$SSN[duplicated(CBCoutbound$SSN)]))
  
  if(!file.exists("TSLOoutbound")){
    dir.create("TSLOoutbound")
  }
  
  #create outbound csv
  write.csv(CBCoutbound, paste("./TSLOoutbound/", "CBCoutbound_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
  
  
  ########Credit Score Outbound########
  #Filename must be CA.C0550263.S2299570.FILE1.TXT
  
  # column A will always be populated with "CA1B" on every upload file
  # column B always "0500"
  # column C always "312250"
  # column O always "2H"
  # column Y always "I4"
  
  #Subset master with appropriate master data
  #no SSN number???
  SCOREoutbound <- master[, c("ACCT","LNAME", "FNAME",  "ADD1", "CITY", "STATE", "ZIP", "HOMEPHONE",
                              "POEPHONE", "CHGOFFDATE", "ASSIGNED", "LOANDATE", "POE", "LSTPMNTDATE",
                              "LSTPMTAMT")]
  
  #need to split area codes from home and work phone number
  #make phone numbers character vectors 
  # SCOREoutbound$Home.Phone.Number <- as.character(SCOREoutbound$Home.Phone.Number)
  # SCOREoutbound$Work.Phone.Number <- as.character(SCOREoutbound$Work.Phone.Number)
  #create vector with area codes
  HOMEAC <- substr(SCOREoutbound$HOMEPHONE, 1,3)
  POEAC <- substr(SCOREoutbound$POEPHONE, 1,3)
  #add in area code columns
  SCOREoutbound$HOMEAC <- HOMEAC
  SCOREoutbound$POEAC <- POEAC
  
  
  ###This block is temporary to remove space between AC and phone number from TSLO list
  SCOREoutbound$HOMEPHONE <- gsub(" ", "", SCOREoutbound$HOMEPHONE)
  SCOREoutbound$POEPHONE <- gsub(" ", "", SCOREoutbound$POEPHONE)
  ###

  #delete area code from phone numbers
  SCOREoutbound$HOMEPHONE <- substr(SCOREoutbound$HOMEPHONE,4,10)
  SCOREoutbound$POEPHONE <- substr(SCOREoutbound$POEPHONE,4,10)
  
  # #Phone Validation
  # #need to use this to find the missing 4
  # length(SCOREoutbound$Work.Phone.Number)
  # sum(length(which(nchar(SCOREoutbound$Work.Phone.Number) > 10)),
  # length(which(nchar(SCOREoutbound$Work.Phone.Number) == 0)),
  # length(which(nchar(SCOREoutbound$Work.Phone.Number) == 10)))
  # #shows the rows that are 1:9
  # which(nchar(SCOREoutbound$Work.Phone.Number) %in% 1:9)
  # SCOREoutbound$Work.Phone.Number[which(nchar(SCOREoutbound$Work.Phone.Number) %in% 1:9)]
  # # > which(nchar(SCOREoutbound$Work.Phone.Number) %in% 1:9)
  # # [1]  317 1699 2286 2446
  # # > SCOREoutbound$Work.Phone.Number[which(nchar(SCOREoutbound$Work.Phone.Number) %in% 1:9)]
  # # [1] "0"    "0"    "000"  "9999"
  
  #add in required Experian metadata columns
  SCOREoutbound$RecordFormat <- "CA1B"
  SCOREoutbound$RecordLength <- "0500"
  SCOREoutbound$SubCode <- "2299570"
  SCOREoutbound$"2HKeyword" <- "2H"
  SCOREoutbound$ModelScore1 <- "I4"
  #SCOREoutbound$LastPaymentDate <- NA
  SCOREoutbound$PlacementStatus <- "R"
  SCOREoutbound$DebtType <- "Z"
  SCOREoutbound$POEAddressIndicator <- "N"
  #SCOREoutbound$LastPaymentAmnt <- NA
  
  #adding in placeholder columns
  SCOREoutbound$p96 <- NA
  SCOREoutbound$p169 <- NA
  SCOREoutbound$p238 <- NA
  SCOREoutbound$p307 <- NA
  SCOREoutbound$p329 <- NA
  SCOREoutbound$p331 <- NA
  SCOREoutbound$p332 <- NA
  SCOREoutbound$p333 <- NA
  SCOREoutbound$p334 <- NA
  SCOREoutbound$p335 <- NA
  SCOREoutbound$p336 <- NA
  SCOREoutbound$p337 <- NA
  SCOREoutbound$p338 <- NA
  SCOREoutbound$p341 <- NA
  SCOREoutbound$p343 <- NA
  SCOREoutbound$p349 <- NA
  SCOREoutbound$p351 <- NA
  SCOREoutbound$p353 <- NA
  SCOREoutbound$p370 <- NA
  SCOREoutbound$p381 <- NA
  SCOREoutbound$p384 <- NA
  SCOREoutbound$p401 <- NA
  SCOREoutbound$p465 <- NA
  
  
  
  
  #Subset in required order by names in SCOREoutbound
  EXPERIANoutbound <- SCOREoutbound[, c("RecordFormat", "RecordLength", "SubCode","ACCT", "p96", "LNAME", "FNAME", "p169", "ADD1",
                                        "p238", "CITY", "STATE", "ZIP", "p307", "2HKeyword", "p329", "p331", "p332", "p333", "p334",
                                        "p335", "p336", "p337", "p338", "ModelScore1", "p341", "p343", "p349", "p351", "p353", "p370",
                                        "HOMEAC", "HOMEPHONE", "p381", "p384", "POEAC", "POEPHONE", "p401", "LSTPMNTDATE",
                                        "CHGOFFDATE", "ASSIGNED", "LOANDATE", "POE", "PlacementStatus", "DebtType",
                                        "POEAddressIndicator", "p465", "LSTPMTAMT")] 
    
  #format dates
  #using combo of base and lubridate
  library(lubridate)
  EXPERIANoutbound$CHGOFFDATE <- format(mdy(EXPERIANoutbound$CHGOFFDATE), "%Y%m%d")
  EXPERIANoutbound$LOANDATE <- format(mdy(EXPERIANoutbound$LOANDATE), "%Y%m%d")
  
  #strip out extra spaces
  library(stringr)
  # #experimentation
  # a <- EXPERIANoutbound$Total.Due[1]
  # #trim white space on both sides
  # b <- str_trim(a, side = "both")
  # #then extract the number
  # str_extract(b, "\\d(.*)")
  # 
  # #or a one liner!!!
  # str_extract(a, "\\d(.*)\\d")
  
  ASSIGNED <- EXPERIANoutbound$ASSIGNED
  
#   ASSIGNED <- str_extract(EXPERIANoutbound$ASSIGNED, "\\d(.*)\\d")
  
  #numbers over 999 have commas, need to strip out to properly turn to numeric
  ASSIGNED1 <- gsub(",","", ASSIGNED)
  
  #make whole number and nine digits with leading zeros
  ASSIGNED <- sprintf("%09d", round(as.numeric(ASSIGNED),0))
  
  #looks good so send back into EXPERIANoutbound
  EXPERIANoutbound$ASSIGNED <- ASSIGNED
  
  #Filename must be CA.C0550263.S2299570.FILE1.TXT
  write.table(EXPERIANoutbound, "./TSLOoutbound/CA.C0550263.S2299570.FILE1.TXT", sep=",", na="", row.names=FALSE, col.names=FALSE)
  
  
  ########OneClick Outbound########
  #Needs
  #"UDH.AccountID","SSN", "LName", "FName", "Street.1", "City", "State", "Zip.Code"
  ONECLICKoutbound <- master[, c("ACCT","SSN", "LNAME", "FNAME", "ADD1", "CITY", "STATE", "ZIP")]
  write.csv(ONECLICKoutbound, paste("./TSLOoutbound/", "ONECLICKoutbound_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
  
  
  ########WebRecon####
  #Needs
  #full name, zip, state, account
  WEBRECONoutbound <- master[, c("FNAME","LNAME", "ZIP", "STATE", "ACCT")]
  write.csv(WEBRECONoutbound, paste("./TSLOoutbound/", "WEBRECONoutbound_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
  
  print("Outbound files have been created and are in the forward directory.")
}
