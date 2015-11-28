complete <- function(directory, id) {
        # Initiate list
        nobs <- c()
        
        # Loop through files
        for(i in id){
                readNew <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"),
                                           ".csv", sep = ""))
                good <- complete.cases(readNew[,"sulfate"]) * 
                        complete.cases(readNew[,"nitrate"])
                
                nobs <- c(nobs, length(good[good == TRUE]))
        }
        
        # Output
        result = as.data.frame(cbind(id, nobs))
        return(result)
}