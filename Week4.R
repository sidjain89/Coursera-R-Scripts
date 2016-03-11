
folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
hospitalPath <- paste(folderPath, "hospital-data.csv", sep = "")
oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")

hospital <- read.csv(hospitalPath)
outcome <- read.csv(oocmPath, colClasses = "character")

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

best <- function(state, outcome) {
  ## Read outcome data
  if(outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    ## Check that state and outcome are valid
    if(state %in% dat$State)
    {
      ## Return hospital name in that state with lowest 30-day death
      hospitals <- 
    }
    else
    {
       print("invalid state")
    }
  }
  else{
    print("invalid outcome")
  }

  
  ## rate
}

summary(outcome)
unique(outcome$State)


da <- outcome[outcome$30]


