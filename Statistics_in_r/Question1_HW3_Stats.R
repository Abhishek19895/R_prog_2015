
#Question -1 (To generate the difference in log data)
AAPL0206-- #File for data b/w 2002 to 2006
AAPL0711-- #File for data b/w 2007 to 2011

#using Sapply to generate to log values
AAPL0206_returns  <-  sapply(as.list(AAPL0206[,2:  3]),  log)
AAPL0711_returns  <-  sapply(as.list(AAPL0711[,2:  3]),  log)

#Substracting log values on a time-series to generate returns
Returns  <-  function(x){
    b  <-  NULL
    for (i  in  2:length(AAPL0206_returns[,x])) {
        b[i - 1]  <-  AAPL0206_returns[i, x]  -  
            AAPL0206_returns[i - 1,  x]
    }
    b
}

#Doing the same for 2007 -2011
Returns1  <- function(x) {
    b  <- NULL
    for (i  in  2:length(AAPL0711_returns[,  x])){
        b[i - 1]  <-  AAPL0711_returns[i,  x]
        -  AAPL0711_returns[i - 1,  x]
    }
    b
}

#Returns for both APPLE & S&P for 02-06 & 07-11
#2002-2006
appl_0206_rtrns  <-  Returns(1)
sp_0206_rtrns  <-  Returns(2)
p0206  <-  cor(appl_0206_rtrns,  sp_0206_rtrns)

#2007-2011  
appl_0711_rtrns  <-  Returns1(1)
sp_0711_rtrns  <-  Returns1(2)
p0711  <-  cor(appl_0711_rtrns,  sp_0711_rtrns)

#correlation difference in sample
p0711  -  p0206
