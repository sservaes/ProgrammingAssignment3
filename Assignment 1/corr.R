corr <- function(directory, threshold = 0) {
        ## get all the data
        over <- complete(directory, 1:332)
        
        ## apply the threshold
        good <- over[, "nobs"] > threshold
        dataSet <- over[good,1]
        
        ## initiate empty vectors
        correlation <- c()
        
        ## loop through files
        for(el in dataSet){
                readNew <- read.csv(paste(directory, "/", formatC(el, width = 3, flag = "0"),
                                          ".csv", sep = ""))
                readNew <- readNew[complete.cases(readNew),]
                correlation = c(correlation, cor(readNew[,"nitrate"], readNew[,"sulfate"]))
        }
        
        ## return
        return(correlation)
}