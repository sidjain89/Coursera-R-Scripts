
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
  oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
  dat <- read.csv(oocmPath)
  
  # initialize an empty data frame
  df <- data.frame(HospitalName=character(), State=character(), stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  if(outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    
    # get the list of the states
    states <- dat$State
    states <- unique(dat$State)
    states <- sort(states)
    
    # loop through states to build the data frame
    for(state in states)
    {
      # will use a subset dat1 of the data frame dt
      # filter for state
      dat1 <- dat[dat$State == state,]
      
      index <- switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      
      # remove NA
      dat1 <- dat1[dat1[index] != "Not Available",]
      
      # cast as double 
      dat1[,index] <- sapply(dat1[,index], function(x) as.double(as.character(x)))
      
      # keep only hospital name, state and numeric value column
      dat1 <- dat1[,c(2,7,index)]
      
      # order the data frame by numeric value, column name
      dat1 <- dat1[order(dat1[,2], dat1[,1]),]
      
      # add ranks
      dat1$Rank <- rank(dat1[,2], ties.method = "first")
      
      #fetch the desired rank
      if(num == "best"){
        rnk <- min(dat1$Rank)
      }
      if(num == "worst"){
        rnk <- max(dat1$Rank)
      }
      if(is.numeric(num)){
        rnk <- num
      }

      
      # add the hospital name and state into the data frame
      if(nrow(dat1[dat1$Rank == rnk,]) == 0)
      {
#         print("1")
#         df <- rbind(df, data.frame("NA", state))
      }
      else {
        # print("2")
        df <- rbind(df, dat1[dat1$Rank == rnk,][,c(1,2)])
      }
    }
  }
  else {
    print("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(df)
}

r <- rankall("heart failure", 10)
r[r$State == "NV",]

r <- rankall("heart attack", 4)
r[r$State == "HI",]

r <- rankall("pneumonia", "worst")
r[r$State == "NJ",]

# test the code
# head(rankall("heart attack", "20"), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)




## raw code aka construction material
# folderPath <- "C:/Users/Siddharth/Documents/Coursera R/data/week4/"
# oocmPath <- paste(folderPath, "outcome-of-care-measures.csv", sep = "")
# dat <- read.csv(oocmPath)
# 
# states <- dat$State
# states <- unique(dat$State)
# states <- sort(states)
# 
# outcome <- "heart attack"
# 
# index <- switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
# 
# state <- "AK"
# num <- 20
# 
# # initialize an empty data frame
# df <- data.frame(HospitalName=character(), 
#                  State=character(), 
#                  stringsAsFactors=FALSE)
# 
# # filter for state
# dat <- dat[dat$State == state,]
# 
# # remove NA
# dat <- dat[dat[index] != "Not Available",]
# 
# # cast as double 
# dat[,index] <- sapply(dat[,index], function(x) as.double(as.character(x)))
# 
# # keep only hospital name, state and numeric value column
# dat <- dat[,c(2,7,index)]
# 
# # order the data frame by numeric value, column name
# dat <- dat[order(dat[,2], dat[,1]),]
# 
# # add ranks
# dat$Rank <- rank(dat[,2], ties.method = "first")
# 
# # assign the rank
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
# # find the hospital name
# 
# nrow(dat[dat$Rank == rnk,])
# rw <- data.frame("NA", state)
# df <- rbind(df, dat[dat$Rank == rnk,][,c(1,2)])
# 
# head(df)
# 
# class(dat[dat$Rank == rnk,])
# 
# if(length(hospName) == 0){
#   hostName <- NA
# }
# 
# 
# for(state in states)
# {
#   print(state)
#   
# }