## best.R

best <- function(state, outcome=NA) {
  ## Read outcome data
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  data2 <- data1[data1$State == state, ]
  
  state_rows <- length(data2$State)
  
  if(state_rows == 0) {
    return (paste("No data found for state =",state))
  }
  
  if(outcome == "heart attack") {
    col <- 11
  } else if (outcome == "heart failure") {
    col <- 17
  } else if (outcome == "pneumonia") {
    col <- 23
  } else {
    return (paste("No data found for outcome = ",outcome))
  }
  
  return (names(data2)[col])
  
  outcomes <- data2[ , ]
  
  matchString <- "Hospital.30.Day.Death"
  
  matchLen <- nchar(matchString)
  
  str(names(outcomes))

  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate  
  
  
}

