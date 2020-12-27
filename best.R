## best.R

best <- function(state, outcome=NA) {
  
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  data2 <- data1[data1$State == state]
  
  data2
  
}