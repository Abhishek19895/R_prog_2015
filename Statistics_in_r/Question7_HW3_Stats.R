#Question 7
#Generating 5000 simulation of 1 df of Chi Squared distribution
#--we choose ncp as Zero
ch1  <-  rchisq(5000,  1,  ncp  =  0)
ch2  <-  rchisq(5000,  1,  ncp  =  0)
ch3  <-  rchisq(5000,  1,  ncp  =  0)

# I have used the following link for the plot generation
#  --  http://www.statmethods.net/graphs/density.html
ch_total  <-  ch1  +  ch2  +  ch3
h  <-  hist(ch_total,  breaks  =  20,  col  =  "red",
            xlim =  c(0, 30),  ylim =  c(0, 1500),  xlab  =  "Sum of 3 Chi-sq (df=1)", 
            main  =  "Chi- Square of 5000 simulations (20 bins)") 

"The curve for chi Square values)" 
#Drawing the density curve for Chi-Sq (df=3)
ch_d3  <-  rchisq(5000,  3, ncp  =  0)
# add a density estimate with defaults
xfit  <-  seq(min(ch_d3),  max(ch_d3),  length  =  40) 
yfit  <-  dchisq(xfit,  3) 
yfit  <-  yfit  *  diff(h$mids[1:2])  *  length(ch_d3) 
lines(xfit,  yfit,  col  =  "blue",  lwd  =  4)
mtext("3 df Chi Square curve is in blue")




