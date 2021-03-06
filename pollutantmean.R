

pollutantmean <- function(directory, pollutant, id = 1:332){
  meanVal <- NA
  if(pollutant %in% c("sulfate", "nitrate")){
    dPath <- "C:\\Users\\Siddharth\\Documents\\Coursera R\\data\\specdata\\"
    fileList <- list.files( path = dPath,
                            full.names = TRUE)
    # print(fileList)
    
    valList <- list("0")
    
    for(val in id)
    {
      # print(typeof(val))
      char1 <- as.character(sprintf("%03d", val))
      filePath <- paste(dPath, char1, ".csv", sep = "")
      dat <- read.csv(filePath)
      if(pollutant == "sulfate"){ values <- dat$sulfate }
      else {values <- dat$nitrate}
      valList <- c(valList,values[!is.na(values)])
      
      #print(values)
      #print(typeof(data)) 
      
    }
    
    # print(is.list(valList))
    
    valList <- valList[-1] # pop the first element
    vals <- unlist(valList)
    meanVal <- mean(vals)
    return(meanVal)
    #print(meanVal)
  }
}

#pollutantmean(directory = "specdata", pollutant = "nitrate", id = 23)

mv<- pollutantmean("specdata", "sulfate", 70:72)
mv

