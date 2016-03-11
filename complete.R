
complete <- function(directory, id = 1:332){
  dPath <- "C:\\Users\\Siddharth\\Documents\\Coursera R\\data\\specdata\\"
  ret_df <- data.frame(id = integer(), nobs = integer()) # declare an empty frame
  
  for(val in id)
  {
    char1 <- as.character(sprintf("%03d", val))
    filePath <- paste(dPath, char1, ".csv", sep = "")
    dat <- read.csv(filePath)
    vals <- dat$sulfate
    valn <- dat$nitrate 
    ctr1 <- length(vals[!is.na(vals)])
    ctr2 <- length(val[!is.na(valn)])
    minCtr <- min(ctr1, ctr2)
    #x <- paste("ctr1 = ", as.character(ctr1), " ctr2 = ", as.character(ctr2))
    #print(x)
    newRow <- c(val, minCtr)
    #print(newRow)
    ret_df <- rbind(ret_df, newRow)
  }
  
  colnames(ret_df) <- c("id", "nobs") # set column names
  #print(ret_df)
  return(ret_df)
}

#complete("specdata", c(2,4,8,10,12, 275))
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

