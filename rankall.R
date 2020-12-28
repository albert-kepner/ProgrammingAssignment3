rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  
  
  ## Read outcome data
  ## Check that state and outcome are valid
  
  ## Read outcome data
  
  ## Example:
  outcome <- "pneumonia"
  num = 'worst'
  
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(is.character(num)) {
    if( ! (num == 'best' | num == 'worst')) {
      stop("invalid keyword for rank")
    }
  }
  
  if(outcome == "heart attack") {
    col <- 11
  } else if (outcome == "heart failure") {
    col <- 17
  } else if (outcome == "pneumonia") {
    col <- 23
  } else {
    stop("invalid outcome")
  }
  
  outcomes <- data1[ , c(7, 2, col) ]
  
  outcomes[,3] <- as.numeric(outcomes[,3])
  
  good_data <- outcomes[ !is.na(outcomes[,3]), ]
  
  states <- unique(data1[,7])
  
  states2 <- unique(good_data[,1])
  
  names(good_data) <- c('state', 'hospital', 'outcome')
  
  by_state <- split(good_data, good_data$state)
  
  accumulator <- character()

  for( s0 in by_state[1:3]) {
    s1 <- data.frame(s0)
    s2 <- rankstate(s1, num)
    accumulator <- c(accumulator, s2)
  }
  accumulator
  
  m <- matrix(accumulator, ncol=2, byrow=TRUE)
  m
  
  df <- data.frame(m)
  names(df) <- c("hospital", "state")
  df
  
}

rankstate <- function(data, num) {
  sorted_data <- data[
    order( data[,3], data[,2]) ,
  ]
  
  sorted_data$rank <- rank(sorted_data[,3], ties.method="first")
  
  max_rank <- max(sorted_data$rank)
  
  if(is.numeric(num)) {
    selected <- sorted_data[sorted_data$rank == num, ]
  } else if(num == 'best') {
    selected <- sorted_data[sorted_data$rank == 1, ]
  } else if(num == 'worst') {
    selected <- sorted_data[nrow(sorted_data), ]
  } else {
    return (c(NA, good_data[1,1]))
  }
  
  if(nrow(selected) == 1) {
    return (c( selected[1,2], selected[1,1]))
  } else {
    return (c(NA, good_data[1,1]))
  }

}