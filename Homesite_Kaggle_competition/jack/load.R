directory <- "~/Documents/USF/MSAN/Module2/ML/Project/homesite-quote-conversion/jack/"
source(paste(directory, "utilities.R", sep = ""))

library(plyr)
library(data.table)

# Load in data / Preprocess
#------------------------------------------------------------------------------

directory <- "~/Documents/USF/MSAN/Module2/ML/Project/homesite-quote-conversion/data/"
filename <- "train.csv"
train_x <- fread(paste(directory, filename, sep = ''))

# Isolate y variable as its own vector
train_y <- as.factor(train_x$QuoteConversion_Flag)
train_x$QuoteConversion_Flag <- NULL

# Remove constants (nonvariables)
train_x$PropertyField6 <- NULL
train_x$GeographicField10A <- NULL

# Convert Original_Quote_Date to Date
train_x$Original_Quote_Date <- as.Date(as.character(train_x$Original_Quote_Date))
train_x$OQD_Date <- format(train_x$Original_Quote_Date, "%d")
train_x$OQD_Month <- format(train_x$Original_Quote_Date, "%m")
train_x$OQD_Year <- format(train_x$Original_Quote_Date, "%Y")
train_x$OQD_Day <- weekdays(train_x$Original_Quote_Date)
train_x$Original_Quote_Date <- NULL

# Convert Field10 from char to numeric (remove commas)
train_x$Field10 <- as.numeric(sub(',', '', train_x$Field10))

# Convert character variables to factors
train_x <- char2factor(train_x)

#------------------------------------------------------------------------------

# UNIQUE LEVELS PER VARIABLE
unique_levels_per_variable <- function(d) {
  ulpv <- ldply(d, function(col) length(unique(col)))
  names(ulpv) <- c("Varname", "NumUniqueLevels")
  ulpv
}

unique_levels <- unique_levels_per_variable(train_x)
apply(unique_levels["NumUniqueLevels" < 50], 1, function(row) {
  row
})

# HEAD TO HEAD CORRELATIONS
head2head_correlations <- function(X) {
  cormat <- cor(X)
  upper_triangle <- upper.tri(cormat, diag = FALSE)
  cormat[!upper_triangle] <- NA
  cormat <- na.omit(melt(cormat, value.name = "r"))
  cormat <- cormat[order(abs(cormat$r), decreasing = TRUE), ]
  cormat
}

Xy_correlations <- function(X, yname) {
  cormat <- cor(X)
  y_id <- which(unlist(dimnames(cormat)) == yname)[1]
  corvec <- cormat[y_id, ]
  corvec
}

#cormat <- head2head_correlations(news_train) # using data with the factors as ints
#corvec <- Xy_correlations(news_train, 'popular')
#cormat$Var1_y <- corvec[cormat$Var1]
#cormat$Var2_y <- corvec[cormat$Var2]
#knitr::kable(head(cormat, 20))



