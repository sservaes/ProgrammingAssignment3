rankall <- function(outcome, num="best") {
        ## Read outcome data
        dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check state and outcome
        listOutcome <- c("heart failure", "heart attack", "pneumonia")
        
        if (!outcome %in% listOutcome) {
                stop("invalid outcome")
        }
        
        ## Set the condition outcome
        if (outcome == "heart attack") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                mortality <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        ## Make the conditions numeric
        dataSet[, mortality] <- suppressWarnings(as.numeric(dataSet[, mortality]))
        
        ## Filter out the bad data
        bad <- is.na(dataSet[, mortality])
        data <- dataSet[!bad,]
        
        ## Split in groups and sort
        groups <- split(data[, c("Hospital.Name", mortality, "State")], data$State)
        
        sorted <- lapply(groups, function(x) x[order(x[, mortality], x[, 1]), ])
        
        result <- data.frame()
        i <- 1
        for(element in sorted){
                if (num == "best") {
                        ranking <- 1
                } else if (num == "worst"){
                        ranking <- length(element[, 2])
                } else {
                        ranking <- num
                }
                if (!is.na(element[ranking,1])) {
                        result <- rbind(result, element[ranking, c(1, 3)])
                }
                else {
                        result <- rbind(result, data.frame("Hospital.Name" = NA, "State" = element[1, 3]))
                }
        }
        colnames(result) <- c("hospital", "state")
        rownames(result) <- result[,2]
        return(result)
}