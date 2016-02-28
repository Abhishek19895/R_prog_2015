
#loading the libraries
library(lubridate)  ;  library(caret)  ;  library(Hmisc)  ;  library(ade4)  
library(VIM)  ;  library(readr) 

#loading the datasets
setwd('~/Dropbox/Machine_Learning_Project_Kaggle/')
train_homesite  <-  read.csv('train.csv',  stringsAsFactors  =  TRUE)
test_homesite  <-  read.csv('test.csv',  stringsAsFactors  =  TRUE)
train_y  <-  train_homesite$QuoteConversion_Flag




#------------------------#------------------------#------------------------
#---------------------# Transforming the training set #-------------------
#------------------------#------------------------#-----------------a-------
#removing Response
drops  <-  c("QuoteConversion_Flag")
train  <-  train_homesite[  ,!(names(train_homesite)  %in%  drops)]

#Creating a categorical variable for Date (Quarter by Quarter performance)
train$Quarter  <-  quarter(train[,  2])
train$Year  <-  as.POSIXlt(train[,  2])$year  +  1900  #(Year on Year performance)
train$month  <-  as.integer(format(as.Date(train[,  2]), "%m"))
train$day  <-  weekdays(as.Date(train[,  2]))
rm(train_homesite) #dropping the extra dataset

#Cleaning the column with a comma value
train$GeographicField10A  <-  train$GeographicField1A  *  train$GeographicField5A
train$PropertyField6  <-  train$PropertyField8  *  train$PropertyField12
train$Field10  <-  as.numeric(gsub(",",  "",  train$Field10))

#Modifying important variables
train$GeographicField10A  <-  train$GeographicField10A
train$PersonalField9  <-  ifelse(train$PersonalField9  >  2,  0,  1)  
train$SalesField5  <-  ifelse(train$SalesField5  >  4,  0,  1)  
train$SalesField4  <-  ifelse(train$SalesField4  >  4,  0,  1)  
train$PersonalField37  <-  ifelse(train$PersonalField37  >  0,  1,  0)  
train$PersonalField10B  <-  ifelse(train$PersonalField10B  >  0  
                                   &  train$PersonalField10B  <  17,  1,  0)
train$PropertyField29[is.na(train$PropertyField29)]  <-  100 #Dummy
train$PropertyField29  <-  ifelse(train$PropertyField29  >  0,  1,  0)  
train$PersonalField84[is.na(train$PersonalField84)]  <-  100 #Dummy
train$PersonalField84  <-  ifelse(train$PersonalField84  >  2,  0,  1)  

#Treating bad binary variables: changing the levels
train$Field12  <-  ifelse(train$Field12  ==  "N",  0,  1)
train$PropertyField37  <-  ifelse(train$PropertyField37  ==  "N",  0,  1)
train$PropertyField2A  <-  ifelse(train$PropertyField2A  ==  -1,  0,  1)
train$PropertyField5  <-  ifelse(train$PropertyField5  ==  "N",  0,  1)
train$PropertyField11A  <-  ifelse(train$PropertyField11A  ==  -1,  0,  1)
train$PropertyField30  <-  ifelse(train$PropertyField30  ==  "N",  0,  1)
train$PropertyField32  <-  ifelse(train$PropertyField32  ==  "N",  0,  1)
train$GeographicField5A  <-  ifelse(train$GeographicField5A  ==  -1,  0,  1)
train$GeographicField10B  <-  ifelse(train$GeographicField10B  ==  -1,  0,  1)
train$GeographicField14A  <-  ifelse(train$GeographicField14A  ==  -1,  0,  1)
train$GeographicField18A  <-  ifelse(train$GeographicField18A  ==  -1,  0,  1)
train$GeographicField21A  <-  ifelse(train$GeographicField21A  ==  -1,  0,  1)
train$GeographicField22A  <-  ifelse(train$GeographicField22A  ==  -1,  0,  1)
train$GeographicField23A  <-  ifelse(train$GeographicField23A  ==  -1,  0,  1)
train$GeographicField56A  <-  ifelse(train$GeographicField56A  ==  -1,  0,  1)
train$GeographicField60A  <-  ifelse(train$GeographicField60A  ==  -1,  0,  1)
train$GeographicField61A  <-  ifelse(train$GeographicField61A  ==  -1,  0,  1)
train$GeographicField62A  <-  ifelse(train$GeographicField62A  ==  -1,  0,  1)

#A function that takes a general dataset and splits it into 3 parts: 
#  Nominal dataset, Categorical dataset & Binary dataset

#Pass your dataset, and this will split it into Identity, Categorical, Ordinal & Binary
split_data  <-  function(data)  {
  total.cols  <-  ncol(data)
  #setting up vectors to contain column numbers of their category
  all.chr  <-  c()  ;  all.nom  <-  c()  ;  all.binary  <-  c()  ;  bad  <-  c()
  #excluding the row identifier in the training dataset
  for  (i  in  1:total.cols)  {
    len  <-  length(unique(data[,i]))  ;  class.i  <-  class(data[,i])
    #Conditional statements to tag as "Categorical",  "Continous",  "Nominal" & "Binary"
    if  (len  ==  2)  {
      all.binary  <-  c(all.binary,  i)
    } 
    else if  ((class.i  ==  "character") |  (class.i  ==  "factor"))  {
      all.chr  <-  c(all.chr,  i)
    } 
    else if  ((class.i  ==  "numeric") | (class.i  ==  "integer"))  {
      all.nom  <-  c(all.nom,  i)
    }
    else  {bad  <-  c(bad,  i)}
  }#End of For loop  
  #producing the 3 datasets
  Nom.data  <-  data[,  all.nom]  ;  Bin.data  <-  data[,  all.binary]
  Chr.data  <-  data[,  all.chr]
  #returing the 3 values
  return(list("Categorical"  =  Chr.data,  "Nominal"  =  Nom.data
               ,  "Binary"  =  Bin.data))
}#End of function

#Calling the function to make the splits
categorical.train  <-  split_data(train)$Categorical
Nominal.train  <-  split_data(train)$Nominal
Binary.train  <-  split_data(train)$Binary

#treating missing value





#------------------------#------------------------#------------------------
#---------------------# Transforming the test set #-------------------
#------------------------#------------------------#------------------------
test  <-  test_homesite

#Creating a categorical variable for Date (Quarter by Quarter performance)
test$Quarter  <-  quarter(test[,  2])
test$Year  <-  as.POSIXlt(test[,  2])$year + 1900  #(Year on Year performance)
test$month  <-  as.integer(format(as.Date(test[,  2]), "%m"))
test$day  <-  weekdays(as.Date(test[,  2]))
rm(test_homesite) #dropping the extra dataset

#Cleaning the column with a comma value
test$GeographicField10A  <-  test$GeographicField1A  *  test$GeographicField5A
test$PropertyField6  <-  test$PropertyField8  *  test$PropertyField12
test$Field10  <-  as.numeric(gsub(",",  "",  test$Field10))

#Modifying important variables
test$PersonalField9  <-  ifelse(test$PersonalField9  >  2,  0,  1)  
test$SalesField5  <-  ifelse(test$SalesField5  >  4,  0,  1)  
test$SalesField4  <-  ifelse(test$SalesField4  >  4,  0,  1)  
test$PersonalField37  <-  ifelse(test$PersonalField37  >  0,  1,  0)  
test$PersonalField10B  <-  ifelse(test$PersonalField10B  >  0  
                                  &  test$PersonalField10B  <  17,  1,  0)
test$PropertyField29[is.na(test$PropertyField29)]  <-  100 #Dummy
test$PropertyField29  <-  ifelse(test$PropertyField29  >  0,  1,  0)  
test$PersonalField84[is.na(test$PersonalField84)]  <-  100 #Dummy
test$PersonalField84  <-  ifelse(test$PersonalField84  >  2,  0,  1)  

#Treating bad binary variables: changing the levels
test$Field12  <-  ifelse(test$Field12  ==  "N",  0,  1)
test$PropertyField2A  <-  ifelse(test$PropertyField2A  ==  -1,  0,  1)
test$PropertyField5  <-  ifelse(test$PropertyField5  ==  "N",  0,  1)
test$PropertyField11A  <-  ifelse(test$PropertyField11A  ==  -1,  0,  1)
test$PropertyField30  <-  ifelse(test$PropertyField30  ==  "N",  0,  1)
test$PropertyField37  <-  ifelse(test$PropertyField37  ==  "N",  0,  1)
test$PropertyField32  <-  ifelse(test$PropertyField32  ==  "N",  0,  1)
test$GeographicField5A  <-  ifelse(test$GeographicField5A  ==  -1,  0,  1)
test$GeographicField10B  <-  ifelse(test$GeographicField10B  ==  -1,  0,  1)
test$GeographicField14A  <-  ifelse(test$GeographicField14A  ==  -1,  0,  1)
test$GeographicField18A  <-  ifelse(test$GeographicField18A  ==  -1,  0,  1)
test$GeographicField21A  <-  ifelse(test$GeographicField21A  ==  -1,  0,  1)
test$GeographicField22A  <-  ifelse(test$GeographicField22A  ==  -1,  0,  1)
test$GeographicField23A  <-  ifelse(test$GeographicField23A  ==  -1,  0,  1)
test$GeographicField56A  <-  ifelse(test$GeographicField56A  ==  -1,  0,  1)
test$GeographicField60A  <-  ifelse(test$GeographicField60A  ==  -1,  0,  1)
test$GeographicField61A  <-  ifelse(test$GeographicField61A  ==  -1,  0,  1)
test$GeographicField62A  <-  ifelse(test$GeographicField62A  ==  -1,  0,  1)

#Pass your dataset, and this will split it into Continous, Categorical, Ordinal & Binary
#Calling the function to make the splits
categorical.test  <-  split_data(test)$Categorical
Nominal.test  <-  split_data(test)$Nominal
Binary.test  <-  split_data(test)$Binary

#Treating categorical variables for Test & Train set
categorical.train  <-  acm.disjonctif(categorical.train)
categorical.test  <-  acm.disjonctif(categorical.test)
#Filtering out Bad levels
drops  <-  c(setdiff(names(categorical.train),names(categorical.test)))
categorical.train  <-  categorical.train[,  !(names(categorical.train)  %in%  drops)]
drops  <-  c(setdiff(names(categorical.test),names(categorical.train)))
categorical.test  <-  categorical.test[,  !(names(categorical.test)  %in%  drops)]

#Appending Response to a final dataset along with other datasets
train  <-  cbind(Nominal.train,  Binary.train,  categorical.train)  
n.test  <-  cbind(Nominal.test,  Binary.test,  categorical.test)  

#Adding an Interaction term
train$field100  <-  train$PropertyField37  *  train$SalesField5#Train set
train$field101  <-  train$PropertyField37  *  train$PersonalField10A
train$field102  <-  train$PersonalField12 ^ 2
train$field103  <-  train$PropertyField37  *  train$Field7
train$field104  <-  train$PropertyField37  *  train$SalesField4
train$field105  <-  train$PropertyField37  *  train$PersonalField10B
train$field106  <-  train$PropertyField37  *  train$PersonalField12
train$field107  <-  train$PropertyField37  *  train$SalesField1A
train$field108  <-  train$PropertyField37  *  train$PersonalField13
train$field109  <-  train$PropertyField37  *  train$CoverageField6A
train$field110  <-  train$PropertyField37  *  train$CoverageField11B
train$field111  <-  train$PropertyField37  *  train$SalesField6
train$field112  <-  train$PropertyField37  *  train$SalesField3
train$field113  <-  train$PropertyField37  *  train$Field10
train$field114  <-  train$PropertyField37  *  train$GeographicField12A
train$field115  <-  train$PropertyField37  *  train$PersonalField2
train$field116  <-  train$PropertyField37  *  train$PersonalField9
train$field117  <-  train$PropertyField37  *  train$SalesField1B
train$field118  <-  train$PropertyField37  *  train$PropertyField35
train$field119  <-  train$PropertyField37  *  train$PropertyField32
train$field120  <-  train$PropertyField37  *  train$SalesField2B
train  <-  cbind(train,  train_y)

#Test set
n.test$field100  <-  n.test$PropertyField37  *  n.test$SalesField5#n.test set
n.test$field101  <-  n.test$PropertyField37  *  n.test$PersonalField10A
n.test$field102  <-  n.test$PersonalField12 ^ 2
n.test$field103  <-  n.test$PropertyField37  *  n.test$Field7
n.test$field104  <-  n.test$PropertyField37  *  n.test$SalesField4
n.test$field105  <-  n.test$PropertyField37  *  n.test$PersonalField10B
n.test$field106  <-  n.test$PropertyField37  *  n.test$PersonalField12
n.test$field107  <-  n.test$PropertyField37  *  n.test$SalesField1A
n.test$field108  <-  n.test$PropertyField37  *  n.test$PersonalField13
n.test$field109  <-  n.test$PropertyField37  *  n.test$CoverageField6A
n.test$field110  <-  n.test$PropertyField37  *  n.test$CoverageField11B
n.test$field111  <-  n.test$PropertyField37  *  n.test$SalesField6
n.test$field112  <-  n.test$PropertyField37  *  n.test$SalesField3
n.test$field113  <-  n.test$PropertyField37  *  n.test$Field10
n.test$field114  <-  n.test$PropertyField37  *  n.test$GeographicField12A
n.test$field115  <-  n.test$PropertyField37  *  n.test$PersonalField2
n.test$field116  <-  n.test$PropertyField37  *  n.test$PersonalField9
n.test$field117  <-  n.test$PropertyField37  *  n.test$SalesField1B
n.test$field118  <-  n.test$PropertyField37  *  n.test$PropertyField35
n.test$field119  <-  n.test$PropertyField37  *  n.test$PropertyField32
n.test$field120  <-  n.test$PropertyField37  *  n.test$SalesField2B

#Exporting the files
setwd('~/Desktop/Kaggle/Homesite/')  #Chaging directory for result
rm(list  =  ls()[!(ls()  %in%  c('train',  'n.test'))]) #Cleaning the memory
write.csv(train,  "n_train.csv") #Shifting modelling to H20 
write.csv(n.test,  "n_test.csv") #Shifting modelling to H20





#------------------------#------------------------#------------------------
#---------------------# Making Predictions with H20 functions #-------------------
#------------------------#------------------------#------------------------
#Calling the libraries
library(h2o)  ;  library(h2oEnsemble) 

#innitializing the set up
h2o.init(nthreads  =  -1, #All threads
         max_mem_size  =  "8G",  min_mem_size  =  '4G') #Memory for H20 cloud
h2o.removeAll()  #Clean stat
#Instance : Successfully connected to http://127.0.0.1:54321/ 
#Session ID: _sid_9397 

#Importing the files  
df  <-  h2o.importFile(  
  path  =  normalizePath("~/Desktop/Kaggle/Homesite/n_train.csv"))

supertest  <-  h2o.importFile(  #Kaggle Test Set
  path  =  normalizePath("~/Desktop/Kaggle/Homesite/n_test.csv"))

#Treating the response variable : Factor
df[,  length(df)]  <-  as.factor(df[,  length(df)])

#Splitting into traning, test & control
splits  <-  h2o.splitFrame(
  df,
  c(.6,  .2),  #60% train, 20 % test & 20% Valid
  seed   =  10004  #randomization
)

#Training, test & validation sets
supertrain  <-  df  #The Full train
train  <-  h2o.assign(splits[[1]],  "train.hex")
test  <-  h2o.assign(splits[[2]],  "test.hex")
valid  <-  h2o.assign(splits[[3]],  "valid.hex")




#------------------------#------------------------#------------------------
#---------------------# Making Predictions with Random Forests #-------------------
#------------------------#------------------------#------------------------
rf1  <-  h2o.randomForest(
  training_frame  =  supertrain,
  validation_frame  =  valid,
  x  =  1:(length(df)  -  1), #Predictors
  y  =  length(df), #Response
  ignore_const_cols  =  TRUE, #Remove Consts
  model_id  =  "h2o_rf_homesite", #Instance
  balance_classes  =  TRUE, #Unbalanced data
  max_depth  =  15,  #max depth
  binomial_double_trees  =  TRUE,#Binary Class
  mtries  =  75,  #mtry variables
  stopping_metric  =  "AUC", #AUC stop
  stopping_rounds  =  3, #Stopping criteria
  stopping_tolerance  =  .0001, #Stopping threshold 
  score_each_iteration  =  T,#Train & validation for each Tree
  seed  =  100001)

#checking model performance
summary(rf1) #checking the performance
rf1@model$validation_metrics  #Validation performance
h2o.confusionMatrix(rf1) #Confusion Matrix
h2o.auc(rf1,  train  =  TRUE,  valid  =  TRUE) #AUC  0.9544400 (1st Interaction)
rf1@model$variable_importances[1:50, "variable"] #Important variables






#------------------------#------------------------#------------------------
#---------------------# Making Predictions with GBM  #-------------------
#------------------------#------------------------#------------------------
gbm1  <-  h2o.gbm(
  training_frame  =  supertrain,
  validation_frame  =  valid,
  x  =  1:(length(df)  -  1), #Predictors
  y  =  length(df), #Response
  distribution  =  'bernoulli', #Binomial
  ntrees  =  50,  #Adding trees
  ignore_const_cols  =  TRUE, #Remove Consts
  learn_rate  =  .42, #Parameter tuning
  sample_rate  =  .8,  #Out of box error
  col_sample_rate  =  .8, #Random Subsampling 
  balance_classes  =  TRUE,#Binary class 
  stopping_rounds  =  3,
  stopping_metric  =  "AUC", #AUC being the evaluation metric
  stopping_tolerance  =  .0001,
  score_each_iteration  =  T, 
  model_id  =  "h20_gbm_homesite",
  seed  =  2000001)  #h20's seed

summary(gbm1)   #GBM performance
gbm1@model$validation_metrics   #Validation performance
h2o.confusionMatrix(gbm1)
h2o.auc(gbm1,  train  =  TRUE,  valid  =  TRUE) #AUC  
gbm1@model$variable_importances[1:40, "variable"] #Important variables


#------------------------#------------------------#------------------------
#---------------------# Making Predictions on Test set #-------------------
#------------------------#------------------------#------------------------
rf_predictions  <-  h2o.predict( #Random Forest
  object  =  rf1,
  newdata  =  supertest)
rf.data  <-  as.data.frame(rf_predictions)#Predictions

gbm_predictions  <-  h2o.predict( #GBM
  object  =  gbm1,
  newdata  =  supertest)
gbm.data  <-  as.data.frame(gbm_predictions)

#Binding with the old filer
n.test  <-  cbind(n.test,  gbm.data)
n.test  <-  n.test[,  c(1,  length(n.test))] #Selecting the concerned columns
names(n.test)  <-  c("QuoteNumber",  "QuoteConversion_Flag")
write.csv(n.test,  file  =  "result.csv",  row.names  =  F)  #Exporting to csv





#------------------------#------------------------#------------------------
#---------------------# Clearing the environment #-------------------
#------------------------#------------------------#------------------------
detach("package:MASS",  unload  =  TRUE)
detach("package:caret",  unload  =  TRUE)
detach("package:lubridate",  unload  =  TRUE)
detach("package:Hmisc",  unload  =  TRUE)
h2o.shutdown(prompt  =  FALSE)
detach("package:h2oEnsemble",  unload  =  TRUE)
detach("package:h2o",  unload  =  TRUE)
rm(list  =  ls())  #Clearing environment

