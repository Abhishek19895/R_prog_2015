#Question -10
#loading the data
Fecal

#Performing calculations in R
Fecal$x_diff  <-  Fecal$Levels  -  400
#sign 
Fecal$Sign_value  <-  sign(Fecal$x_diff)
#Absolute
Fecal$x_diff  <-  abs(Fecal$x_diff)
#removing rows with x-diff =0
Fecal  <-  subset(Fecal,  Fecal$x_diff  !=  0)
length(Fecal[  ,1  ])
#ordering
Fecal  <-  Fecal[order(Fecal$x_diff),] 
#Storing repetitions
Table  <-  data.frame(table(Fecal$x_diff))
names(Table)[1]  <-  "Value"
#Assigning ranks based on cummulative sums
Table$Max  <-  cumsum(Table$Freq)
Table$Min  <-  Table$Max  -  Table$Freq  +  1
Table$Rank  <-  (Table$Max  +  Table$Min)/2

#Merging with the bigger table
Merge  <-  merge(Fecal,  Table,  by.x  =  "x_diff",  
                 by.y  =  "Value",  all.x  =  TRUE) 
Merge$Prod  <-  Merge$Rank  *  Merge$Sign_value
sum(Merge$Prod)
