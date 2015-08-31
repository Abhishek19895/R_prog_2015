#Question- 4
HeartRate

#Performing calculations in R
HeartRate$temp_diff  <-  HeartRate$Body.Temp  -  98.6
#sign 
HeartRate$Sign_value  <-  sign(HeartRate$temp_diff)
#Absolute
HeartRate$temp_diff  <-  abs(HeartRate$temp_diff)
HeartRate$temp_diff  <-  round(HeartRate$temp_diff, 2)
#removing rows with x-diff =0
HeartRate  <-  subset(HeartRate,  temp_diff  !=  0)
#ordering
HeartRate  <-  HeartRate[order(HeartRate$temp_diff),  ] 
#Stroing repetitions
Table  <-  data.frame(table(HeartRate$temp_diff))
names(Table)[1]  <-  "Value"
#Assigning ranks based on cummulative sums
Table$Max  <-  cumsum(Table$Freq)
Table$Min  <-  Table$Max  -  Table$Freq  +  1
Table$Rank  <-  (Table$Max  +  Table$Min)/2

#Merging with the bigger table
Merge  <-  merge(HeartRate,  Table,  by.x  =  "temp_diff"
                 ,  by.y  =  "Value",  all.x  =  TRUE) 
Merge$Prod  <-  Merge$Rank  *  Merge$Sign_value
sum(Merge$Prod)




