directory <- "~/Documents/USF/MSAN/Module2/ML/Project/homesite-quote-conversion/jack/"
source(paste(directory, "load.R", sep = ""))

library(forecast)

# Average proportion per day
dayavg <- aggregate(factor2num(train_y), list(train_x$Original_Quote_Date), mean)
dayavg_d <- diff(dayavg$x)
dayavg_d_7 <- diff(dayavg_d, lag = 7)
plot(dayavg$x, type = 'l')
plot(dayavg_d, type = 'l')
plot(dayavg_d_7, type = 'l')

par(mfrow = c(2, 1))
acf(dayavg$x, 500)
pacf(dayavg$x, 500)

par(mfrow = c(2, 1))
acf(dayavg_d, 500)
pacf(dayavg_d, 500)

par(mfrow = c(2, 1))
acf(dayavg_d_7, 81)
pacf(dayavg_d_7, 81)
