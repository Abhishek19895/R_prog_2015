--------------#Question - 2
    #I have used the below url to help answer question 2 & 3
    "http://playingwithr.blogspot.com/2011/06/rejection-sampling.html"

sample.x  <-  rexp(100000, .75)
accept  <-  c()

length <- NULL
for (i in 1:length(sample.x)) {
    #Generating U from a Uniform distribution b/w (1,0)    
    U  <-  runif(1)
    #m = 1.4715    
    if (dexp(sample.x[i], .75) * 1.4715 * U 
        <=  2.25 * sample.x[i] * exp(-1.5*sample.x[i])) {
        accept[i] <- 'Yes'
    }
    else  {
        accept[i] <- 'No'
    }
}

length.yes <- NULL
for (i in 1:15000) {
    length  =  data.frame(i,  sample.x[i],  accept  =  factor(accept[i], 
                                                              levels  =  c('Yes','No')))
    length.yes  <-  rbind(length.yes, length)
}

yes <- subset(length.yes, length.yes$accept == "Yes")

names(yes) <- c("Summator","sample_x","Accept")
#picking the first 10,000 yes
yes <- yes[1:10000,]
#to get the last name
tail(yes, 1)[1]

#Plotting the results using a histogram
h1 <- hist(yes$sample_x,  main  =  "X from exponentiel accepted for Gamma" 
           ,  xlab  =  "Sample.X",  ylab  =  "Frequency",  ylim  =  c(0, 1000)  
           ,col  =  "light blue",  breaks  =  seq(0,15,0.1))
