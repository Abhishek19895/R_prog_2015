# utilities

char2factor <- function(dataframe) {
  for (var in names(dataframe)) {
    if (is.character(dataframe[[var]])) {
      dataframe[[var]] <- as.factor(dataframe[[var]])
    }
  }
  dataframe
}

factor2num <- function(column) {
  column <- as.numeric(as.character(column))
  column
}