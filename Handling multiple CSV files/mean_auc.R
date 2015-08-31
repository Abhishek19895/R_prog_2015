

#defining the function that uploads CSVs file from the path 
# Function contains only 2 parameter the path & file-number

mean_auc <- function(directory, campaign) {
#using the path to collect all the needed files  
  files <- dir(directory, pattern = '*_exp.csv', full.names = TRUE) 
  a <- NULL
  for (i in 1: length(files)) {
#Calculating the Mean & median for the data-set
#Using a for loop to pull all the files 
# and append them with a campaign column
    table <- read.csv(files[i], header = TRUE)
    table <- cbind(table, Campaign_id = as.numeric(strsplit(
      strsplit(files[i], "/")[[1]][2],"_")[[1]][1]))
    a <- rbind(a, table)
  }
  a <- subset(a, Campaign_id == campaign)
#End of For loop for data extraction
# Calculating mean for the subset & printing it
  mean_auc <- aggregate(a$auc, by = list(a$Campaign_id), mean)
  names(mean_auc) <- c("Campaign_Id","mean_auc")
  mean_auc
}

