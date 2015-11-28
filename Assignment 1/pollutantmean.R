pollutantmean <- function(directory, pollutant, id = 1:332) {
        # Initiate list
        dataSet <- c()
        
        # Loop through files
        for(i in id){
                readNew <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"),
                                         ".csv", sep = ""))
                dataSet <- c(dataSet, readNew[,pollutant])
        }
        
        # Output
        return(mean(dataSet, na.rm = TRUE))
}