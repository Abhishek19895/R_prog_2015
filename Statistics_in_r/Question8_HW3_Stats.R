------#Question 8 -a)
    #downloading the data
    MotherEducationBirthWeight
years  <-  MotherEducationBirthWeight$YEARSEDUC
weight  <-  MotherEducationBirthWeight$BIRTHWEIGHT

#Correlation
p  <-  cor(years,weight)
p
#z-value
z  <-  p/sqrt(1/(length(years) - 3))
#P-value at the z score 
p.value <- (1-pnorm(z))*2
p.value 

------#Question 8 - b)
    "We do random sampling of the original column"
years.1  <-  sample(years, replace = FALSE)

#Correlation
p.new  <-  cor(years.1,  weight)
p.new
z.new  <-  p.new/sqrt(1/(length(years.1) - 3))

#P-value at the z score 
p.new.value  <-  (1-pnorm(abs(z.new)))*2
p.new.value


------#Question 8 - c)
    "We do random sampling of the original column 25k times"
sample.p <- NULL
#Running a loop a total 25k times
for (i in 1:25000){
    i  <-  sample(years,  replace  =  FALSE)
    #Taking correlation with each sample for weight vector
    p.new  <-  cor(i,  weight)
    sample.p  <-  c(sample.p,  p.new)
}

#drawing a histogram with the same
hist(sample.p,  col  =  "light blue",  xlab  =  "Sample p"
     ,ylab  =  "Frequency",  main  =  "Histogram of Correlations"
     ,  border  = "Blue",  xlim  =  c(-.5,  .5),  ylim  =  c(0,  10000))


------#Question 8 - d)
    #drawing a v-line at the original p'
    abline(v  =  p,  col  =  "red",  lty = 10)
mtext("Dotted Red line is the original correlation", side  =  4)

#Proportion of values crossing this threshold
p.more  <-  sample.p[sample.p  >  p]
more.than.p  <- length(p.more)/length(sample.p)
more.than.p
# printing the two tail p-value
p.value.generated <- 2 * more.than.p
p.value.generated




