mergedmaster <- function(master, experian, webrecon){
  
  ###Experian
  #WILL NEED TO CONFIRM COLUMN NAME OF ACCOUNTS IN THE EXPERIAN RESULTS... it's ACCOUNT.NUMBER
  scorerows <- which(experian$ACCOUNT.NUMBER %in% master$ACCT)
  
  #create subset of SSN and scores
  experian <- experian[scorerows, c("ACCOUNT.NUMBER", "SCORE.OR.EXCLUSION.1","X...SCR.FACTOR.CDS..1")]
  
  #merge 
  master <- merge(master, experian, by.x="ACCT", by.y="ACCT", all.y=TRUE)
  
  #change score name to something readable 
  
  names(master)[46:47] <- c("INCSCORE","DOLLARSCORE")
  
  
  ###WebRecon  
  #only need the 11th column to flag accounts for removal
  litigiousaccounts <- webrecon$Account
  
  #which rows in master3 have litigious accounts?
  flagrows <- which(master$ACCT %in% litigiousaccounts)
  
  #flag these rows as litigious
  master$LITIGIOUS <- "N"
  master$LITIGIOUS[flagrows] <- "Y"
  
  return(master)
}