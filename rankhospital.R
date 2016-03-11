
# folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
# hospitalPath <- paste(folderPath, "hospital-data.csv", sep = "")
# oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
# 
# hospital <- read.csv(hospitalPath)
# outcome <- read.csv(oocmPath, colClasses = "character")
# 
# outcome[,11] <- as.numeric(outcome[,11])
# hist(outcome[,11])

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
  oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
  dat <- read.csv(oocmPath)
  
  if(outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    ## Check that state and outcome are valid
    
    if(state %in% dat$State)
    {
      ## Return hospital name in that state with lowest 30-day death
      # col index -> heart attack - 16, heart failure - 22 , pneumonia - 28
      # num = {best, worst, <intermittent rank>}
      index <- switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      # filter for state
      dat <- dat[dat$State == state,]
      
      # remove NA
      dat <- dat[dat[index] != "Not Available",]
      
      # cast as double 
      dat[,index] <- sapply(dat[,index], function(x) as.double(as.character(x)))
      
      # keep only hospital name and numeric value column
      dat <- dat[,c(2,index)]
      
      # order the data frame by numeric value, column name
      dat <- dat[order(dat[,2], dat[,1]),]
      
      # add ranks
      dat$Rank <- rank(dat[,2], ties.method = "first")
      
      # assign the rank
      if(num == "best"){
        rnk <- min(dat$Rank)
      }
      if(num == "worst"){
        rnk <- max(dat$Rank)
      }
      if(is.numeric(num)){
        rnk <- num
      }
      
      # find the hospital name
      hospName <- as.character(dat[dat$Rank == rnk,]$Hospital.Name)
      
      if(length(hospName) == 0){
        hostName <- NA
      }
      
      return(hospName)
    }
    else
    {
      print("invalid state")
    }
  }
  else{
    print("invalid outcome")
  }
}

# test the code
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

# this will help in constrcution of the code
# 
# folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
# oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
# dat <- read.csv(oocmPath)
# 
# state <- "TX"
# vals <- c("heart attack","heart failure","pneumonia")
# cause <- "heart failure"
# index <- switch(cause, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23 )
# rank <- 4
# 
# dat <- dat[dat$State == state,]
# dat <- dat[dat[index] != "Not Available",]
# dat[,index] <- sapply(dat[,index], function(x) as.double(as.character(x)))
# dat <- dat[,c(2,index)]
# 
# dat <- dat[order(dat[,2], dat[,1]),]
# 
# dat$Rank <- rank(dat[,2], ties.method = "first")
# 
# num <- 4
# 
# if(num == "best"){
#   rnk <- min(dat$Rank)
# }
# if(num == "worst"){
#   rnk <- max(dat$Rank)
# }
# if(is.numeric(num)){
#   rnk <- num
# }
# 
# hospName <- as.character(dat[dat$Rank == rnk,]$Hospital.Name)
# 
# rnk
# 
# colnames(dat)
# 
# class(num)
# 
# hospNames <- dat1[dat1$Rank == rank,]$Hospital.Name
# class(dat1[,index])
# 
# 
# 
# 
# hospNames <- dat1[dat1[,index] == rank,]$Hospital.Name
# hospNames <- sort(hospNames)
# as.character(hospNames[1])
# 
# min(as.numeric(dat1[17][dat1[17]!= "Not Available"])) 
# 
# 
# rnk <- order(dat1[,index])
# dat1$Rank <- rnk
# 
# as.character(dat1[dat1$Rank == 4,]$Hospital.Name)
# 
