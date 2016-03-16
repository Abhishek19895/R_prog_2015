full_df <- read.csv("train.csv")

full_df$QuoteConversion_Flag <- as.factor(full_df$QuoteConversion_Flag)
full_df <- subset(full_df, select = -c(PropertyField6,
                                       GeographicField10A))

f <- full_df[, grep("^Field", names(full_df))]
cf <- full_df[, grep("CoverageField", names(full_df))]
sf <- full_df[, grep("SalesField", names(full_df))]
persf <- full_df[, grep("PersonalField", names(full_df))]
propf <- full_df[, grep("PropertyField", names(full_df))]
gf <- full_df[, grep("GeographicField", names(full_df))]

y <- full_df$QuoteConversion_Flag
