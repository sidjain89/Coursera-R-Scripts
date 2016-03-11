library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")

ozone <- ozone[order(ozone$ozone),]

head(ozone)

# create a matrix - this will hold 10 samples

ll <- matrix(NA, nrow = 10, ncol = 155)

# do bagging - create 10 samples
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1], replace = T)
  ozone0  <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temprature ^ data, data = ozone0, span = 0.2)
  ll[i,] <- predict(loess0, newdata = data.frame(ozone = 1:155))
}

# plot the ozone data
plot(ozone$ozone,ozone$temperature, pch = 19, cex = 0.5)

for(i in 1:10)
{ 
  #plot all of the samples
  lines(1:155, ll[i,], col = "grey", lwd = 2)
}

# plot mean of the sample
lines(1:155, apply(ll,2,mean), col = "red", lwd = 4)

