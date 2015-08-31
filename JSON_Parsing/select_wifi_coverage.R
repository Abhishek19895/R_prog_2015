
#The following function intakes the csv file to compute the wifi mean
select_wifi_coverage <- function(file) {
  #reading the file
  x <- read.csv(file, header = TRUE)
  #Transforming the dataset into a new dataset of different dimensionality
  library(reshape2)
  #The first three columns & 4th column as wifi and 5th as wifi_values  
  xmelt <- melt(x, id=c("Source","Room","Time"))  
  names(xmelt)[4:5] <-c("wifi","wifi_values") 
  #Subsetting the set for only concerned columns
  xmelt <- xmelt[,c(2,4,5)]
  total <- aggregate(xmelt$wifi_values, by= list(xmelt$Room, xmelt$wifi), length)
  names(total) <- c("Room","wifi","total")
#Subsetting away the na's away
  xnan <- na.omit(xmelt)
  total_n_na <- aggregate(xnan$wifi_values, by= list(xnan$Room, xnan$wifi), length)
  names(total_n_na) <- c("Room","wifi","total_n_na")
#merging the two files to get both total & total_na
  xfile <- merge(total, total_n_na, by=c("Room","wifi"), all.x = TRUE)
#replacing all NAs with zero in the aggregated set
  xfile[is.na(xfile)] <- 0
#detaching the package as use is complete
  detach(package:reshape2, unload = TRUE)
#adding a column for calculating %nas  
  xfile$prcnt_not_na <- (xfile$total_n_na * 100/ xfile$total)   
#Now subsetting for rooms greater than 70%
  x70 <- subset(xfile, prcnt_not_na>=70.0) 
#now removing all the un-needed columns we get
  x70 <-x70[,1:2]
#Tabulating the wifi with rooms
  xroom <- aggregate(x70$Room, by = list(x70$wifi), length)
  names(xroom) <- c("wifi","No_Rooms")
#subset the wifi with greater than 7 rooms
  wifi <- xroom$wifi[xroom$No_Rooms>7]
#printing the wifi's in string format
  as.character(wifi)
}
