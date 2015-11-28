best <- function(state, outcome) {
        ## Read outcome data
        dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check state and outcome
        listOutcome <- c("heart failure", "heart attack", "pneumonia")
        if (!state %in% dataSet[, "State"]) {
                stop("invalid state")
        }
        
        if (!outcome %in% listOutcome) {
                stop("invalid outcome")
        }
        
        ## return hospital with lowest 30-day death rate
        good <- dataSet[, "State"] == state
        data <- dataSet[good, ]
        
        if (outcome == "heart attack") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        lowest <- suppressWarnings(which.min(data[, mortality]))
        result <- data[lowest, 2]
        
        return(result)
}