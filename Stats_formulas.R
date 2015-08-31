a <- 5
s <- 2
n <- 20
xbar <- 7
t <- (xbar-a)/(s/sqrt(n))
t
[1] 4.472136
> 2*pt(-abs(t),df=n-1)
[1] 0.0002611934


log(1.42/.58)*.5

#zvalues #Percentiles
qnorm(.975,0,1)
#t-values #Percentiles
qt(.925,20) #for cutoff t-values 
#z-distribution : to get p-value
pnorm(1.983,0,1)
#t-distribution : to get critical value
pt(1.983,20)

#f doistribution
#lower bound
qf(.99, df1=xf, df2=yf) 

#sx2/sy2 ration of intervals
#Upper bound
qf(.99, df1=yf, df2=xf) 
yf=12
xf=8

#Obese
obese_folks <- subset(Obese,Obese$obese==1)
mean_obese <- mean(obese_folks$scl, na.rm = TRUE)
var_obese <- var(obese_folks$scl, na.rm = TRUE)
len <- Obese[,c(4,8)]
len <- na.omit(len)
table(len$obese)
not_obese_folks <- subset(Obese,Obese$obese==0)
mean_not_obese <- mean(not_obese_folks$scl, na.rm = TRUE)
var_not_obese <- var(not_obese_folks$scl, na.rm = TRUE)
