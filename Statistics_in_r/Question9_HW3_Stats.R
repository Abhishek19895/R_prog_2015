#Question: 9
#loading the dataset and namig it "Odor"
Odor 
#rearranging the table
Odor.1 <- data.frame(Odor$NoOdor)
Odor.1$Type ="NoOdor"
names(Odor.1)[1] = "Value"

Odor.2 <- data.frame(Odor$LemonOdor)
Odor.2$Type ="LemonOdor"
names(Odor.2)[1] = "Value"

new.odor <- rbind(Odor.2,Odor.1)
new.odor <- na.omit(new.odor)

#ordering
new.odor  <- new.odor[order(new.odor$Value),] 
#Storing repetitions
Table  <- data.frame(table(new.odor$Value))
names(Table)[1]  <- "Value"

#Assigning ranks based on cummulative sums
Table$Max  <-  cumsum(Table$Freq)
Table$Min  <-  Table$Max  -  Table$Freq  +  1
Table$Rank  <-  (Table$Max  +  Table$Min)/2
#Merging with the bigger table
Merge  <-  merge(new.odor,  Table,  by.x  =  "Value"
                 ,  by.y  =  "Value",  all.x  =  TRUE)
Merge  <-  Merge[,-c(3 : 5)]

Odor_NoOdor  <-  subset(Merge,  Merge$Type  ==  "NoOdor")
Odor_LemonOdor  <-  subset(Merge,  Merge$Type  ==  "LemonOdor")

#Summating Ranks
sum(Odor_NoOdor$Rank)
sum(Odor_LemonOdor$Rank)

n1  <-  length(EHR_Male[,  1])
n2  <-  length(EHR_Female[,  1])

