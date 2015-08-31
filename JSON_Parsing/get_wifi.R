
#the following function will read a json file and report a sorted vector 
# A note of caution -larger files will need more time to give output 
# for this function

#Extracting from JSON File a vector of wifi names sorted alphabetically
#naming in hexadecimal format
get_wifi <- function(jsonfile) {
#Innitializing a variable to store all names
  namevector <- list()
#loading the packages required for JSON in R
  library(rjson)
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
#Detaching packages from the R environment after usage 
  detach(package:rjson, unload=TRUE)
#printing the unique proper names from the names_vector 
  names <- unlist(namevector)
#printing in Alphabetical order
  sort(unique(names[grep(":", names, value = FALSE)
                          ]#removing improper wifi_names
             )#unique function
      )#Sort function
}#end of get_wifi() function



