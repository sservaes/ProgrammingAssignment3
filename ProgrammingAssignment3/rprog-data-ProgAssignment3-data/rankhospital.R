rankhospital <- function(state, outcome, num = "best") {
        ## read outcome data
        dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## check state and outcome
        listOutcome <- c("heart failure", "heart attack", "pneumonia")
        if (!state %in% dataSet[, "State"]) {
                stop("invalid state")
        }
        
        if (!outcome %in% listOutcome) {
                stop("invalid outcome")
        }
        
        ## return hospital in that state with the given rank
        good <- dataSet[, "State"] == state
        data <- dataSet[good, ]
        
        if (outcome == "heart attack") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        suppressWarnings(data[, mortality] <- as.numeric(data[, mortality]))
        
        bad <- is.na(data[, mortality])
        data <- data[!bad, ]
        
        sorted <- data[order(data[, mortality], data [,2]), ]
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(data)
        } else if (num > nrow(data))
                return(NA)
        
        result <- sorted[num, 2]
        
        return(result)
}