
#the below function will load JSON files and give the wifi-dfs
indoor2dataframe <- function(jsonfile) {
#loading the packages required for JSON in R
library(rjson)
#Using the function from the first assignment to get wifi names
#Extracting from JSON File a vector of wifi names sorted alphabetically
#naming in hexadecimal format
  get_wifi <- function(jsonfile) {
  #Innitializing a variable to store all names
    namevector <- list()
    jsondata <- fromJSON(file=jsonfile, method = "C")
  #Now parsing the Json data in a format to extract the needed column
  #of a list, we will run a for loop at 3 different hierarchial levels.
  #level-1 for accesing the Json file  
    for (i in 1: length(jsondata)) {  
    #level-2 for accesing at the "Data" column file 
      for (j in 1: length(jsondata[[i]]$Data)) {
      #level-3 at the inner most list level
        for (k in 1: length(jsondata[[i]]$Data[[j]]$Quantities )) {
          namevector <- c(namevector, jsondata[[i]]$Data[[j]]$Quantities[[k]]$Name)
        }       #End of k loop  
      }    #end of j loop
    }    #end of i loop
  #printing the unique proper names from the names_vector 
    names <- unlist(namevector)
  #printing in Alphabetical order
    sort(unique(names[grep(":", names, value = FALSE)
                    ]#removing improper wifi_names
    )#unique function
    )#Sort function
  }#end of get_wifi() function

#Getting back to the master function -indoor2dataframe()
#obtaining the subset of the JSON data to get wifi propeties
  jsondata <- fromJSON(file=jsonfile, method = "C")[[1]]$Data
#For wifi_names using the above defined function
  names <- get_wifi(jsonfile)

#--[I have used Professor Yannet Interian's function to help solve this from here
m=5
#Running a for loop to get all the values and binding it to a dataframe -indoor
  indoor <- NULL #Initializing the data-frame that stores the final values
  for (m in 1:length(jsondata)) {
  #Storing the value of Room variable for each iteration  
    room <- jsondata[[m]]$Properties[[1]]$Value
  #Building a dataframe with Source="wifi" & Time variable column
    d <- data.frame("Source"="wifi", "Room"=room, "Time"=jsondata[[m]]$MTime)
  #Horizontally scaling the dataframe adding n(wifi) columns   
    d[names] <- rep(NA, length(names))
  #Updating wifi value for each corresponding wifi  
    n <- sapply(jsondata[[m]]$Quantities, function(x) x$Name)
    v <- sapply(jsondata[[m]]$Quantities, function(x) x$Number)
    nv <- data.frame(n,v)
  #Making sure only the correct wifi names get updated
    nv <- subset(nv,n %in% names)
    n <- as.character(unlist(nv[,1])) ; v <- nv[,2]
    d[n] <- v
  #Binding the value to the final data-frame that is printed  
    indoor <- rbind(indoor, d)
  }#End of outer for-loop  
#Detaching packages from the R environment after usage 
  detach(package:rjson, unload=TRUE)
#eliminating rows will all NAs as Output
  indoor[rowSums(is.na(indoor))!=6, ]
}#end of master function - indoor2dataframe




