
#Writing a function that accepts csv file and substitutes all their NAs 
replace_NAs <- function(file, v) {
#Transforming the dataset into a new dataset of different dimensionality
  library(reshape2)
  x <-read.csv("indoor-location-test.csv", header = TRUE)
#The first three columns & 4th column as wifi and 5th as wifi_values  
  xmelt <- melt(x, id=c("Source","Room","Time"))  
  names(xmelt)[4:5] <-c("wifi","wifi_values") 
#Subsetting away the na's and making a new dataset
  xnan <- na.omit(xmelt)
  n_na <- aggregate(xnan$wifi_values, by= list(xnan$Room, xnan$wifi), mean)
  names(n_na) <- c("Room","wifi","mean_na")
#subset the rooms with a wifi mean value of greater than -90
  mean_n_na <- subset(n_na, mean_na>-90)
#merging the two datasets to get the overall mean
  alldata <- merge(xmelt, mean_n_na, by=c("Room","wifi"), all = TRUE) 
#manipulating the data to impute missing values using If-else
  alldata$new_wifi_value <- ifelse (is.na(alldata$wifi_value),
          ifelse(is.na(alldata$mean_na),-100, 
          ifelse(alldata$mean_na> -90, alldata$mean_na, -100)), alldata$wifi_value)
#eliminating unwanted rows  
  alldata <- alldata[,-c(5:6)]
#subsetting only the specific wifi's
  transdata <- subset(alldata, wifi %in% v)
#Re-ordering the columns to a pattern demanded
  transdata <- transdata[,c(3,1,4,2,5)]
#Now transforming the data to the original format 
  t <- dcast(transdata, Source + Room + Time ~ wifi, value.var = "new_wifi_value")
#Detaching the package reshape2
  detach(package:reshape2, unload = TRUE)
  t
}


