

#defining the function that uploads CSVs file from the path 
# Function contains only 2 parameter the path & threshold

num_trees_auc <- function(directory, threshold) {
#using the path to collect all the needed files  
  files <- dir(directory, pattern = '*_exp.csv', full.names = TRUE) 
  a<-NULL
  for (i in 1:length(files)) {
#Calculating the Mean & median for the data-set
#Using a for loop to pull all the files 
#and append them with a campaign column
    table <- read.csv(files[i], header=TRUE)
    table <- cbind(table, Campaign_id = as.numeric(strsplit(
      strsplit(files[i], "/")[[1]][2],"_")[[1]][1]))
    a <- rbind(a, table)
  }
#End of For loop for data extraction
#Calculating mean for the subset & printing it
  mean_auc <- aggregate(a$auc, by=list(a$Campaign_id), mean)
  names(mean_auc) <- c("Campaign_id","mean_auc_campaign")
#Subsetting based on threshold   
  mean_subset_auc <- mean_auc[mean_auc$mean_auc_campaign>threshold,]
#Now selecting data with the given campaigns using merge
  campaign_data <- merge(a, mean_subset_auc, by="Campaign_id", all.y=TRUE)  
#Computing mean for this data-set for num_trees
  overall_mean <- aggregate(campaign_data$auc,
                          by=list(campaign_data$num_trees), mean)
  names(overall_mean) <- c("num_trees","mean_auc")
  print(overall_mean)
}
