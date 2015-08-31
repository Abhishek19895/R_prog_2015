
#defining the function that uploads CSVs file from the path 
# Function contains only the path

min_num_trees <- function(directory) {
#using the path to collect all the needed files  
#loading the exp files
  files1 <- dir(directory, pattern = '*_exp.csv', full.names = TRUE) 
#loading the ci files
  files2 <- dir(directory, pattern = '*_ci.csv', full.names = TRUE) 
#Innitializing storage variables
  a <- NULL
  b <- NULL
  for (i in 1:length(files1))  {
#Calculating the Mean & median for the data-set
#Using a for loop to pull all the files 
#and append them with a campaign column
    table1 <-read.csv(files1[i], header=TRUE)
    table1 <- cbind(table1, campaign = as.numeric(strsplit(
      strsplit(files1[i], "/")[[1]][2],"_")[[1]][1]))
    a<-rbind(a, table1)
  }
  for (i in 1:length(files2)) {
#Calculating the Mean & median for the data-set
#Using a for loop to pull all the files 
#and append them with a campaign column
    table2 <- read.csv(files2[i], header=TRUE)
    table2 <- cbind(table2, campaign = as.numeric(strsplit(
      strsplit(files2[i], "/")[[1]][2], "_")[[1]][1]))
    b <- rbind(b, table2)
  }
#picking only the concerned columns
  b <- b[,c("low_ci","high_ci","campaign")]  
#Merging the above data-frame by Column Campaign and 
  fulldata <- merge(a, b, by="campaign", all=TRUE)
  realdata <- fulldata[c(fulldata$auc>fulldata$low_ci & fulldata$auc<fulldata$high_ci ),]
#Including only necessary columns 
  realdata <- realdata[,c("campaign","num_trees")]
#Calculating min num_trees for each campaign & printing it
  mintrees <- aggregate(realdata$num_trees,
                       by=list(realdata$campaign), min)
  names(mintrees) <- c("campaign","num_trees")
  print(mintrees[order(mintrees$campaign),])
}

