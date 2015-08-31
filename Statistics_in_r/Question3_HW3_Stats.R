#### Question: 3 (Acceptance Rejection principle)
# I have used the below url to answer the question
sample1.x  <-  rcauchy(20000)
accept1  <-  c()

length.1  <-  NULL
for (i in 1:length(sample1.x)) {
    #Generating U from a Uniform distribution b/w (1,0)
    #U <=(.8244 * e^(-.5*x*x)*(1+x*x))
    U.1  <-  runif(1)
    if (U.1 <=  .8244 * exp(-0.5 * ((sample1.x[i]) ^ 2)) * (1 + ((sample1.x[i]) ^ 2))) 
    {
        accept1[i] <- 'Yes'
    }
    else  {
        accept1[i] <- 'No'
    }
}

table(length.1.yes$accept)

length.1.yes <- NULL
for (i in 1:20000) {
    length.1  =  data.frame(i,  sample1.x[i],  accept  =  factor(accept1[i], 
                                                                 levels  =  c('Yes','No')))
    length.1.yes  <-  rbind(length.1.yes, length.1)
}

yes.1 <- subset(length.1.yes, length.1.yes$accept == "Yes")

names(yes.1) <- c("Summator","sample_x","Accept")
#picking the first 10,000 yes
yes.1 <- yes.1[1:10000,]
#to get the last name
tail(yes.1, 1)[1]

#Plotting the results using a histogram
h  <-  hist(yes.1$sample_x,  main  =  "X from Cauchy accepted for Normal", 
            xlab  =  "Sample.X",  ylab  =  "Frequency",  ylim  =  c(0, 500)  
            ,col  =  "light blue",  breaks  =  seq(-5,5,0.1))





