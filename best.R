
# folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
# hospitalPath <- paste(folderPath, "hospital-data.csv", sep = "")
# oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
# 
# hospital <- read.csv(hospitalPath)
# outcome <- read.csv(oocmPath, colClasses = "character")
# 
# outcome[,11] <- as.numeric(outcome[,11])
# hist(outcome[,11])

best <- function(state, outcome) {
  ## Read outcome data
  if(outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    ## Check that state and outcome are valid
    folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
    oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
    dat1 <- read.csv(oocmPath)
    
    if(state %in% dat1$State)
    {
      ## Return hospital name in that state with lowest 30-day death
      # col index -> heart attack - 16, heart failure - 22 , pneumonia - 28
      index <- switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      
      dat1 <- dat1[dat1$State == state,]
      minVal <- min(as.double(dat1[index][dat1[index] != "Not Available"]))
      
      hospNames <- dat1[dat1[index] == minVal,]$Hospital.Name
      hospNames <- sort(hospNames)
      as.character(hospNames[1])
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
best("BB", "heart attack")
best("MD", "pneumonia")

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

## this will help in constrcution of the code

# folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
# oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
# dat <- read.csv(oocmPath)
# 
# state <- "MD"
# vals <- c("heart attack","heart failure","pneumonia")
# cause <- "pneumonia"
# index <- switch(cause, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23 )
# 
# dat1 <- dat[dat$State == state,]
# 
# minVal <- min(as.double(dat1[index][dat1[index] != "Not Available"]))
# 
# hospNames <- dat1[dat1[index] == minVal,]$Hospital.Name
# hospNames <- sort(hospNames)
# as.character(hospNames[1])
# 
# min(as.numeric(dat1[17][dat1[17]!= "Not Available"])) 

#matrix(data = colnames(dat1), ncol = ncol(dat1), nrow = 1)



