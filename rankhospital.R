
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  
  ## Read outcome data
  
  ## Example:
  state <- "PA"
  outcome <- "pneumonia"
  
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  data2 <- data1[data1$State == state, ]
  
  state_rows <- length(data2$State)
  
  if(state_rows == 0) {
    stop("invalid state")
    return (paste("No data found for state =",state))
  }
  
  if(outcome == "heart attack") {
    col <- 11
  } else if (outcome == "heart failure") {
    col <- 17
  } else if (outcome == "pneumonia") {
    col <- 23
  } else {
    stop("invalid outcome")
    return (paste("No data found for outcome = ",outcome))
  }
  
  outcomes <- data2[ , c(2, col) ]
  
  outcomes[,2] <- as.numeric(outcomes[,2])
  
  good_data <- outcomes[ !is.na(outcomes[,2]), ]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  sorted_data <- good_data[
    order( good_data[,2], good_data[,1]) ,
  ]
}