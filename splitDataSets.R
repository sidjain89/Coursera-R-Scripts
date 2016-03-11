library(datasets)
data("airquality")

head(airquality)

s <- split(airquality, airquality$Month)
head(s)

list_of_means <- lapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind", "Temp")], na.rm = TRUE))
list_of_means
typeof(list_of_means)

list_of_means <- sapply(s, function(x) colMeans(x[,c(1,2,3,4)], na.rm = TRUE))
list_of_means

typeof(list_of_means)

z <- rnorm(10) 
f1 <- gl(2,5)
f2 <- gl(5,2)
f1
f2
interaction(f1,f2)

str(split(z, list(f1,f2), drop = TRUE))
