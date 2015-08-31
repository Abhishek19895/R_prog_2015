
#The following function intakes the csv file to compute the wifi mean
select_wifi_mean <- function(file) {
#loadig the file
  x <- read.csv(file, header = TRUE)
#Transforming the dataset into a new dataset of different dimensionality
  library(reshape2)
#The first three columns & 4th column as wifi and 5th as wifi_values  
  xmelt <- melt(x, id=c("Source","Room","Time"))  
  names(xmelt)[4:5] <-c("wifi","wifi_values") 
#Subsetting the set for only concerned columns
  xmelt <- xmelt[,c(2,4,5)]
#cleaning the data to handle all NAs, by eliminating them
  xmelt<-na.omit(xmelt)
  d <- ave(xmelt$wifi_values, by= list(xmelt$Room, xmelt$wifi), FUN = mean)
  xmelt<-cbind(xmelt, d)
  d1 <- xmelt[,c("Room","wifi","d")]
#Picking out the unique entries for other operations
  d2 <- d1[!duplicated(d1),]
#now calulating column to store the number of rooms per wifi-signal with strength>=-90
#Taking the subset of the above data-frame & doing a counts
  d2rooms <- d2[d2[,3]>=-90,]
  d2rooms$Rooms <- ave(as.vector(d2rooms$Room),by=list(d2rooms$wifi), FUN = length)  
  d2rooms$Rooms <- as.numeric(d2rooms$Room)
#Detaching the package Reshape2 from the environment
  detach(package:reshape2, unload = TRUE)
#Printing wifi with 7rooms with avg>-90
  wifinames <- unique(d2rooms[d2rooms[,4]>=7,][,2])
  as.character(wifinames)
}



