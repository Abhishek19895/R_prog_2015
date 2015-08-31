#Question- 6
EHR  <-  EHR[,c(1:  2)  ]
EHR  <-  EHR[  order(EHR$Body.Temp),  ]

#Storing repetitions
Table  <- data.frame(EHR$Body.Temp)
names(Table)[1]  <- "Value"
#Assigning ranks based on cummulative sums
Table$Max  <-  cumsum(Table$Freq)
Table$Min  <-  Table$Max  -  Table$Freq  +  1
Table$Rank  <-  (Table$Max  +  Table$Min)/2
#Merging with the bigger table
Merge  <-  merge(EHR,  Table,  by.x  =  "Body.Temp"
                 ,  by.y  =  "Value",  all.x  =  TRUE) 
Merge  <-  Merge[,-c(3 : 6)]

EHR_Male  <-  subset(Merge,  Merge$Gender  ==  "Male")
EHR_Female  <-  subset(Merge,  Merge$Gender  ==  "Female")

#Summating Ranks
sum(EHR_Male$Rank)
sum(EHR_Female$Rank)

n1  <-  length(EHR_Male[,  1])
n2  <-  length(EHR_Female[,  1])





