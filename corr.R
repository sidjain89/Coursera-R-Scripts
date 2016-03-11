
corr <- function(directoy, threshold = 0){
  dPath <- "C:\\Users\\Siddharth\\Documents\\Coursera R\\data\\specdata\\"
  fileList <- list.files(dPath, full.names = TRUE)
  corList <- c(-1)

  for(filePath in fileList){
    #print(typeof(filePath))
    dat <- read.csv(filePath)
    val_sulfates <- dat$sulfate
    val_nitrates <- dat$nitrate
    nList <- c(-1)
    vList <- c(-1)
    
    maxLen = length(dat$nitrate)
    #print(maxLen)
    
    for(i in 1:maxLen)
    {
      vs <- val_sulfates[i]
      vn <- val_nitrates[i]
      
      if(!is.na(vs) && !is.na(vn))
      {
        nList <- append(nList, vn)
        vList <- append(vList, vs)
      }
    }
    
    #print(nList[1:100])
    
    nList <- nList[-1]
    vList <- vList[-1]
    
    if(length(nList) > threshold){
      corrVal <- cor(nList, vList)
      
      #print(corrVal)
      
      corList <- append(corList, corrVal)
      
    }
  }
  
  return(corList[-1])
}
    
    

#corr("specdata", 150)

cr <- corr("specdata")
summary(cr)