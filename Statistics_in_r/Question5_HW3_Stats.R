#Question- 5
Experts  <- Experts[,11  :  12]

#Performing calculations in R
Experts$x_diff  <-  Experts$Average -  mean(Experts$DJIA)
#sign 
Experts$Sign_value  <- sign(Experts$x_diff)
#Absolute
Experts$x_diff  <-  abs(Experts$x_diff)
Experts$x_diff  <-  round(Experts$x_diff,  2)
#removing rows with x-diff = 0
Experts  <- subset(Experts,  Experts$x_diff  !=  0)
#ordering
Experts  <- Experts[order(Experts$x_diff),] 
#Storing repetitions
Table  <- data.frame(table(Experts$x_diff))
names(Table)[1]  <- "Value"
#Assigning ranks based on cummulative sums
Table$Max  <- cumsum(Table$Freq)
Table$Min  <- Table$Max  -  Table$Freq  +  1
Table$Rank  <- (Table$Max  +  Table$Min)/2

#Merging with the bigger table
Merge  <-  merge(Experts,  Table,  by.x  =  "x_diff",
                 by.y  =  "Value",  all.x  =  TRUE) 
Merge$Prod  <-  Merge$Rank  *  Merge$Sign_value
sum(Merge$Prod)



