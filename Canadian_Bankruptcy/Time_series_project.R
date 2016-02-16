# Load libraries
library(tseries)  ;  library(forecast)  ;  library(corrplot)  ;  library(car)
library(lmtest)  ;  library(lawstat)  ;  library(graphics)  ;  library(e1071)

###################################
setwd("/Users/abhisheksingh29895/Desktop/courses/CURRENT/Time_Series/Project")
#clearing the environment
rm(list  =  ls())
# loading the training & test sets
train  <-  read.csv("train.csv",  header  =  TRUE)
test  <-  read.csv("test.csv",  header  =  TRUE)

#Understanding the training set
str(train)#All numeric & ints
fivenum(train$Population)#understanding population variable
pairs(train[,2:5],  col  =  "light blue",  main  =  "correlation between variables")
cor_matrix  <-  cor(train[,2:5])  ;  corrplot(cor_matrix,  method  =  "pie")
cat("So we witness that population shows a direct relation with Bankruptcy Rate (Response)
    and House_Price_Index \n")

#Fitting a Lm model on the training set
model  <-  lm(Bankruptcy_Rate  ~  Unemployment_Rate  +  Population  +  House_Price_Index
              ,  data  =  train)
summary(model)#Checking model summary
cat("This is a good model as it has an R-Square",summary(model)$r.squared
    ,"close to Adjusted R-Square",summary(model)$adj.r.squared
    ,"and all coefficients are significant")

#Doing a box-cox transformation on this dataset to get a better match for the
#charachterics
par(mar  =  c(1,  1,  1,  1))
boxCox(model,  main  =  "Transformation of predictor")
cat("So going by the box-Cox plot it seems  that the response variable needs to raised to 
    power .5")

#Iteration1: With the box-cox transformation
model.1  <-  lm(Bankruptcy_Rate^.5  ~  Unemployment_Rate  +  Population  
                +  House_Price_Index,  data  =  train)
summary(model.1)#Checking model summary
cat("This is a good model as it has an higher R-Square",summary(model.1)$r.squared
    ,"and a higher Adjusted R-Square",summary(model.1)$adj.r.squared
    ,"and all coefficients are significant")
e  <-  model.1$residuals#residuals


###########################   RESIDUAL DIAGNOSTICS
#The model seems to have a decent fit, but lets check for the residual characterstics
#of the plot

#Assumption:1 The residuls should have a mean of Zero
cat("Null Hypothesis (Ho): The residuals have an expected value of Zero")
cat("Alternate Hypothesis (Ha): The residuals do not have an expected value of Zero")
t.test(e)#to verify this
plot(e,  main  =  "Residuals vs t",  ylab  =  "",  col  =  "blue")
abline(h  =  0, col  =  "red")
cat("As the 95% range contains 0, we fail to reject Ho that the residuals have
    an expected value of Zero \n")
cat("However, we also see some seasonal trend in the pattern of residuals")


#Assumption:2 The residuls should have no heteroskedasticity
cat("Null Hypothesis (Ho): The residuals have no heteroskedasticity")
cat("Alternate Hypothesis (Ha): The residuals do have heteroskedasticity")
bptest(model.1)#to verify this
group  <-  c(rep(1,  72),  rep(2,  72),  rep(3,  72),  rep(4,  72))
e1  <-  e
leveneTest(e1,  group) #Levene
bartlett.test(e1,  group) #Bartlett   
residualPlot(model.1,  main  =  "Distribution of Residuals with fitted values",
             col  =  "pink",  pch  =  18)
cat("Given the above tests & plots, We will reject the Null hypothesis that there 
    is  no heteroskedasticity is the distribution\n")


#Assumption3: The residuals belong to a normal distribution
cat("Null Hypothesis (Ho): The residuals come from a distribution that is Normal")
cat("Alternate Hypothesis (Ha): The residuals do not come from a distribution 
    that is Normal")
par(mfrow  =  c(1,  1))
qqnorm(e,  main  =  "QQ-plot of Residuals",  col  =  "light blue",  pch  =  15)
qqline(e,  col  =  "red",  lty  =  2,  lwd  =  2)
shapiro.test(e) #SW test
cat("Given the above tests & plots, We fail to reject the Null hypothesis that 
    the distribution is non-normal \n")


#Assumption4: The residuals have no auto-correlation with each other
cat("Null Hypothesis (Ho): The residuals have no significant auto-correlation 
    with each other")
cat("Alternate Hypothesis (Ha): The residuals have significant auto-correlation 
    with each other")
durbinWatsonTest(e) #DW test for 1-lag randomness
cat("Given the high p-value, We fail to reject the Null hypothesis that 
    the distribution has no auto-correlation \n")


###################################
# Forecastingusing time series techniques
bankruptcy  <-  ts(train$Bankruptcy_Rate)
par(mar  =  c(3,3,3,3))
par(mfrow  =  c(2,  1))
plot(bankruptcy,  col  =  "blue",  main  =  "Bankruptcy Rate vs Time"
     ,  xlab  =  "months from start")
acf(bankruptcy,  col  =  "green",  main  =  "ACF plot of Bankruptcy Rate")

#variance stabilization
lbankruptcy  <-  log(bankruptcy) #log-transform
plot(lbankruptcy,  col  =  "blue",  main  =  "log(Bankruptcy Rate) Vs time"
     ,  xlab  =  "months from start")
acf(lbankruptcy,  col  =  "green",  main  =  "Autocorrelations for log(Bankruptcy Rate)",
    xlab  =  "lag")

#Doing an ordinary difference
lbankruptcy.1  <-  diff(lbankruptcy,  lag  =  1) #seasonally difference
plot(lbankruptcy.1,  col  =  "blue",  main  =  "log of Bankruptcy Rate (d=1)")
acf(lbankruptcy.1,  col  =  "green",  main  =  "ACF plot of log of Bankruptcy Rate (d=1)")

#order selection
par(mfrow  =  c(2,  1))
acf(lbankruptcy.1,  lag.max  =  48,  col  =  "blue"
    ,  main  =  "ACF plot of log of Bankruptcy Rate (d=1)")
pacf(lbankruptcy.1,  lag.max  =  48,  col  =  "green"
,  main  =  "PACF plot of log of Bankruptcy Rate (d=1)")

#Forecast the SARIMA(1,1,2)xc(2,1,3)[12] model
m.ml  <-  arima(lbankruptcy,  order  =  c(2,  1,  0),  method  =  "CSS"
                ,  seasonal  =  list(order  =  c(1,  0,  2),  period  =  12))
m.ml
cat("Since the CSS-ML model gets us better performance, we will go ahead with that")

#Taking the forecasts
f  <-  forecast(m.ml,  h  =  12,  level  =  0.95)
l  <-  f$lower #95% PI LL
h  <-  f$upper #95% PI UL
pred  <-  f$mean #predictions

#Prediction for the test set
par(mfrow  =  c(1,  1))
plot(forecast(m.ml,  h  =  12,  level  =  0.95),  main  =  "Prediction for the test set")
#PPlot for the training set
plot(bankruptcy,  xlim  =  c(0,  288),  ylim  =  c(0,  .05),  col  =  "light blue",
     main  =  "Bankruptcy rate on training set",  ylab  =  "Bankruptcy Rate"
     ,  xlab  =  "Months",  lwd  =  3)
abline(v  =  288,  lwd  =  2,  col  =  "green")
points(289:300,  exp(pred),  type  =  "l",  col  =  "blue",  lty  =  2)
points(289:300,  exp(l),  type  =  "l",  col  =  "brown")
points(289:300,  exp(h),  type  =  "l",  col  =  "brown")
points(1:288,  exp(f$fitted),  type  =  "l",  col  =  "blue",  lty  =  2)

#RMSE on the data
sqrt(mean(lbankruptcy  -  f$fitted)  ^  2)

########################### ADDING co-variates to increase model fit
HPI  <-  ts(train$House_Price_Index) #House Price Index
P  <-  ts(train$Population) #population
UR  <-  ts(train$Unemployment_Rate) #Unemployment Rate
HPI.test  <-  ts(test$House_Price_Index) #test set
P.test  <-  ts(test$Population) #test set
UR.test  <-  ts(test$Unemployment_Rate) #test set

#plot the covariates:
par(mfrow  =  c(3,  1))
plot(HPI,  col  =  "brown",  main  =  "House Price Index")
plot(P,  col  =  "brown",  main  =  "population")
plot(UR,  col  =  "brown",  main  =  "Unemployment Rate")

#ordinary difference:
par(mfrow  =  c(2,  2))
plot(diff(HPI),  col  =  "brown",  main  =  "plot of diff(House Price Index)")
acf(diff(HPI),  col  =  "brown",  main  =  "ACF of diff(House Price Index)")
plot(diff(P),  col  =  "brown",  main  =  "plot of diff(population)")
acf(diff(P),  col  =  "brown",  main  =  "ACF of diff(population)")
cat("So Population shows a fairly unstable plot while Housing Price Index shows
    decaying pattern")

#order selection:
par(mfrow  =  c(2,  1))
acf(diff(HPI),  col  =  "brown",  main  =  "ACF of diff(House Price Index)")
pacf(diff(HPI),  col  =  "brown",  main  =  "PACF of diff(House Price Index)")
#p=q=1 seems fine

#Fit an ARIMA(1,1,1) model with covariate information
m.ml1  <-  arima(lbankruptcy,  order  =  c(2,  1,  0),  method  =  "CSS"
                ,  seasonal  =  list(order  =  c(1,  0,  2),  period  =  12),
                xreg  =  data.frame(HPI))
m.ml1

#Taking the forecasts
f1  <-  forecast(m.ml1,  h  =  12,  level  =  0.95,  xreg  =  data.frame(HPI.test))
l1  <-  f1$lower #95% PI LL
h1  <-  f1$upper #95% PI UL
pred1  <-  f1$mean #predictions

#Prediction for the test set
par(mfrow  =  c(2,  1))
plot(f1,  main  =  "Prediction for the test set with Home Price Index Covariate")
#Plot for the training set
plot(bankruptcy,  xlim  =  c(0,  288),  ylim  =  c(0,  .05),  col  =  "light blue",
     main  =  "Bankruptcy rate Vs time with Home Price Index"
     ,  ylab  =  "Bankruptcy Rate",  xlab  =  "Months",  lwd  =  3)
abline(v  =  288,  lwd  =  2,  col  =  "green",  lty  =  2)
points(289:300,  exp(pred1),  type  =  "l",  col  =  "blue")
points(289:300,  exp(l1),  type  =  "l",  col  =  "brown")
points(289:300,  exp(h1),  type  =  "l",  col  =  "brown")
points(1:288,  exp(f1$fitted),  type  =  "l",  col  =  "blue")

#RMSE on the training set
sqrt(mean(lbankruptcy  -  f1$fitted)  ^  2)

###########################   RESIDUAL DIAGNOSTICS
#The model seems to have a decent fit, but lets check for the residual characterstics
#of the plot

#Assumption:1 The residuls should have a mean of Zero
par(mfrow  =  c(1,  2))
cat("Null Hypothesis (Ho): The residuals have an expected value of Zero")
cat("Alternate Hypothesis (Ha): The residuals do not have an expected value of Zero")
e  <-  m.ml1$residuals
t.test(e)#to verify this
plot(e,  main  =  "Residuals vs time",  ylab  =  "",  col  =  "blue",  xlab  =  "months")
abline(h  =  0, col  =  "red")
cat("As the 95% range contains 0, we fail to reject Ho that the residuals have
    an expected value of Zero \n")
cat("However, we also see some seasonal trend in the pattern of residuals")


#Assumption:2 The residuls should have no heteroskedasticity
cat("Null Hypothesis (Ho): The residuals have no heteroskedasticity")
cat("Alternate Hypothesis (Ha): The residuals do have heteroskedasticity")
group  <-  c(rep(1,  72),  rep(2,  72),  rep(3,  72),  rep(4,  72))
e1  <-  e
leveneTest(e1,  group) #Levene
bartlett.test(e1,  group) #Bartlett   
residualPlot(m.ml1,  main  =  "Distribution of Residuals with fitted values",
             col  =  "pink",  pch  =  18)
cat("Given the above tests & plots, We will reject the Null hypothesis that there 
    is  no heteroskedasticity is the distribution\n")


#Assumption3: The residuals belong to a normal distribution
cat("Null Hypothesis (Ho): The residuals come from a distribution that is Normal")
cat("Alternate Hypothesis (Ha): The residuals do not come from a distribution 
    that is Normal")
qqnorm(e,  main  =  "QQ-plot of Residuals",  col  =  "light blue",  pch  =  15)
qqline(e,  col  =  "red",  lty  =  2,  lwd  =  2)
shapiro.test(e) #SW test
cat("Given the above tests & plots, We fail to reject the Null hypothesis that 
    the distribution is non-normal \n")


#Assumption4: The residuals have no auto-correlation with each other
cat("Null Hypothesis (Ho): The residuals have no significant auto-correlation 
    with each other")
cat("Alternate Hypothesis (Ha): The residuals have significant auto-correlation 
    with each other")
tsdiag(m.ml1) #ACF and Ljung-Box test all in one!
runs.test(e) #Runs test for randomness
cat("Given the high p-value, We fail to reject the Null hypothesis that 
    the distribution has no auto-correlation \n")
